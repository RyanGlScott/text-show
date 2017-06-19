{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}

{-|
Module:      TextShow.Debug.Trace
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Functions for tracing and monitoring execution.

These can be useful for investigating bugs or performance problems.
They should /not/ be used in production code.

If you do not wish to require 'TextShow' instances for your @trace@ functions,
the "TextShow.Debug.Trace.TH" and "Text.Show.Text.Debug.Trace.Generic" modules
exist to convert the input to a debug message using Template Haskell or generics,
respectively.

/Since: 2/
-}
module TextShow.Debug.Trace (
      -- * Tracing
      -- $tracing
      tracet
    , tracetl
    , tracetId
    , tracetlId
    , traceTextShow
    , traceTextShowId
    , tracetStack
    , tracetlStack
    , tracetIO
    , tracetlIO
    , tracetM
    , tracetlM
    , traceTextShowM

      -- * Eventlog tracing
      -- $eventlog_tracing
    , tracetEvent
    , tracetlEvent
    , tracetEventIO
    , tracetlEventIO
#if MIN_VERSION_base(4,7,0)
      -- * Execution phase markers
      -- $markers
    , tracetMarker
    , tracetlMarker
    , tracetMarkerIO
    , tracetlMarkerIO
#endif
    ) where

import           Control.Monad (unless)

import qualified Data.ByteString as BS (null, partition)
import           Data.ByteString (ByteString, useAsCString)
import qualified Data.ByteString.Char8 as BS (pack)
import           Data.ByteString.Internal (c2w)
import qualified Data.Text as TS (Text, unpack)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL (Text, unpack)
import           Data.Text.Lazy (toStrict)

import           Debug.Trace

import           Foreign.C.String (CString)

import           GHC.Stack (currentCallStack, renderStack)

import           Prelude ()
import           Prelude.Compat

import           System.IO.Unsafe (unsafePerformIO)

import           TextShow.Classes (TextShow(..))
import           TextShow.Instances ()

-- $tracing
--
-- The @tracet(l)@, @traceTextShow@ and @tracet(l)IO@ functions print messages to an
-- output stream. They are intended for \"printf debugging\", that is: tracing the flow
-- of execution and printing interesting values.
--
-- All these functions evaluate the message completely before printing
-- it; so if the message is not fully defined, none of it will be
-- printed.
--
-- The usual output stream is 'System.IO.stderr'. For Windows GUI applications
-- (that have no stderr) the output is directed to the Windows debug console.
-- Some implementations of these functions may decorate the @Text@ that\'s
-- output to indicate that you\'re tracing.

-- | The 'tracetIO' function outputs the trace message from the IO monad.
-- This sequences the output with respect to other IO actions.
--
-- /Since: 2/
tracetIO :: TS.Text -> IO ()
tracetIO = traceIOByteString . encodeUtf8

-- | Like 'tracetIO' but accepts a lazy 'TL.Text' argument.
--
-- /Since: 2/
tracetlIO :: TL.Text -> IO ()
tracetlIO = tracetIO . toStrict

traceIOByteString :: ByteString -> IO ()
traceIOByteString msg = useAsCString "%s\n" $ \cfmt -> do
    -- NB: debugBelch can't deal with null bytes, so filter them
    -- out so we don't accidentally truncate the message.  See Trac #9395
    let (nulls, msg') = BS.partition (== c2w '\0') msg
    useAsCString msg' $ \cmsg ->
      debugBelch cfmt cmsg
    unless (BS.null nulls) $
      useAsCString "WARNING: previous trace message had null bytes" $ \cmsg ->
        debugBelch cfmt cmsg

-- don't use debugBelch() directly, because we cannot call varargs functions
-- using the FFI.
foreign import ccall unsafe "HsBase.h debugBelch2"
    debugBelch :: CString -> CString -> IO ()

{-|
The 'tracet' function outputs the trace message given as its first argument,
before returning the second argument as its result.

For example, this returns the value of @f x@ but first outputs the message.

> tracet ("calling f with x = " <> showt x) (f x)

The 'tracet' function should /only/ be used for debugging, or for monitoring
execution. The function is not referentially transparent: its type indicates
that it is a pure function but it has the side effect of outputting the
trace message.

/Since: 2/
-}
tracet :: TS.Text -> a -> a
tracet = traceByteString . encodeUtf8

-- | Like 'tracet' but accepts a lazy 'TL.Text' argument.
--
-- /Since: 2/
tracetl :: TL.Text -> a -> a
tracetl = tracet . toStrict

{-# NOINLINE traceByteString #-}
traceByteString :: ByteString -> a -> a
traceByteString bs expr = unsafePerformIO $ do
    traceIOByteString bs
    return expr

-- | Like 'tracet' but returns the message instead of a third value.
--
-- /Since: 2/
tracetId :: TS.Text -> TS.Text
tracetId a = tracet a a

-- | Like 'tracetId' but accepts a lazy 'TL.Text' argument.
--
-- /Since: 2/
tracetlId :: TL.Text -> TL.Text
tracetlId a = tracetl a a

{-|
Like 'tracet', but uses 'showt' on the argument to convert it to a 'TS.Text'.

This makes it convenient for printing the values of interesting variables or
expressions inside a function. For example here we print the value of the
variables @x@ and @z@:

> f x y =
>     traceTextShow (x, z) $ result
>   where
>     z = ...
>     ...

/Since: 2/
-}
traceTextShow :: TextShow a => a -> b -> b
traceTextShow = tracet . showt

-- | Like 'traceTextShow' but returns the shown value instead of a third value.
--
-- /Since: 2/
traceTextShowId :: TextShow a => a -> a
traceTextShowId a = tracet (showt a) a

{-|
Like 'tracet' but returning unit in an arbitrary 'Applicative' context. Allows for
convenient use in do-notation. Note that the application of 'tracet' is not an action
in the 'Applicative' context, as 'tracetIO' is in the 'IO' type.

> ... = do
>   x <- ...
>   tracetM $ "x: " <> showt x
>   y <- ...
>   tracetM $ "y: " <> showt y

/Since: 2/
-}
tracetM :: Applicative f => TS.Text -> f ()
tracetM text = tracet text $ pure ()

-- | Like 'tracetM' but accepts a lazy 'TL.Text' argument.
tracetlM :: Applicative f => TL.Text -> f ()
tracetlM text = tracetl text $ pure ()

{-|
Like 'tracetM', but uses 'showt' on the argument to convert it to a 'TS.Text'.

> ... = do
>   x <- ...
>   traceTextShowM x
>   y <- ...
>   traceTextShowM $ x + y

/Since: 2/
-}
traceTextShowM :: (TextShow a, Applicative f) => a -> f ()
traceTextShowM = tracetM . showt

-- | Like 'tracet' but additionally prints a call stack if one is
-- available.
--
-- In the current GHC implementation, the call stack is only
-- availble if the program was compiled with @-prof@; otherwise
-- 'tracetStack' behaves exactly like 'tracet'.  Entries in the call
-- stack correspond to @SCC@ annotations, so it is a good idea to use
-- @-fprof-auto@ or @-fprof-auto-calls@ to add SCC annotations automatically.
--
-- /Since: 2/
tracetStack :: TS.Text -> a -> a
tracetStack = traceStackByteString . encodeUtf8

-- | Like 'tracetStack' but accepts a lazy 'TL.Text' argument.
--
-- /Since: 2/
tracetlStack :: TL.Text -> a -> a
tracetlStack = tracetStack . toStrict

traceStackByteString :: ByteString -> a -> a
traceStackByteString bs expr = unsafePerformIO $ do
    traceIOByteString bs
    stack <- currentCallStack
    unless (null stack) . traceIOByteString . BS.pack $ renderStack stack
    return expr

-- $eventlog_tracing
--
-- Eventlog tracing is a performance profiling system. These functions emit
-- extra events into the eventlog. In combination with eventlog profiling
-- tools these functions can be used for monitoring execution and
-- investigating performance problems.
--
-- Currently only GHC provides eventlog profiling, see the GHC user guide for
-- details on how to use it. These function exists for other Haskell
-- implementations but no events are emitted. Note that the @Text@ message is
-- always evaluated, whether or not profiling is available or enabled.

-- | The 'tracetEvent' function behaves like 'tracet' with the difference that
-- the message is emitted to the eventlog, if eventlog profiling is available
-- and enabled at runtime.
--
-- It is suitable for use in pure code. In an IO context use 'tracetEventIO'
-- instead.
--
-- Note that when using GHC's SMP runtime, it is possible (but rare) to get
-- duplicate events emitted if two CPUs simultaneously evaluate the same thunk
-- that uses 'traceEvent'.
--
-- /Since: 2/
tracetEvent :: TS.Text -> a -> a
tracetEvent = traceEvent . TS.unpack

-- | Like 'tracetEvent' but accepts a lazy 'TL.Text' argument.
--
-- /Since: 2/
tracetlEvent :: TL.Text -> a -> a
tracetlEvent = traceEvent . TL.unpack

-- | The 'tracetEventIO' function emits a message to the eventlog, if eventlog
-- profiling is available and enabled at runtime.
--
-- Compared to 'tracetEvent', 'tracetEventIO' sequences the event with respect to
-- other IO actions.
--
-- /Since: 2/
tracetEventIO :: TS.Text -> IO ()
tracetEventIO = traceEventIO . TS.unpack

-- | Like 'tracetEventIO' but accepts a lazy 'TL.Text' argument.
--
-- /Since: 2/
tracetlEventIO :: TL.Text -> IO ()
tracetlEventIO = traceEventIO . TL.unpack

#if MIN_VERSION_base(4,7,0)
-- $markers
--
-- When looking at a profile for the execution of a program we often want to
-- be able to mark certain points or phases in the execution and see that
-- visually in the profile.

-- For example, a program might have several distinct phases with different
-- performance or resource behaviour in each phase. To properly interpret the
-- profile graph we really want to see when each phase starts and ends.
--
-- Markers let us do this: we can annotate the program to emit a marker at
-- an appropriate point during execution and then see that in a profile.
--
-- Currently this feature is only supported in GHC by the eventlog tracing
-- system, but in future it may also be supported by the heap profiling or
-- other profiling tools. These function exists for other Haskell
-- implementations but they have no effect. Note that the @Text@ message is
-- always evaluated, whether or not profiling is available or enabled.

-- | The 'tracetMarker' function emits a marker to the eventlog, if eventlog
-- profiling is available and enabled at runtime. The 'TS.Text' is the name of
-- the marker. The name is just used in the profiling tools to help you keep
-- clear which marker is which.
--
-- This function is suitable for use in pure code. In an IO context use
-- 'tracetMarkerIO' instead.
--
-- Note that when using GHC's SMP runtime, it is possible (but rare) to get
-- duplicate events emitted if two CPUs simultaneously evaluate the same thunk
-- that uses 'traceMarker'.
--
-- /Since: 2/
tracetMarker :: TS.Text -> a -> a
tracetMarker msg = traceMarker $ TS.unpack msg

-- | Like 'tracetMarker' but accepts a lazy 'TL.Text' argument.
--
-- /Since: 2/
tracetlMarker :: TL.Text -> a -> a
tracetlMarker msg = traceMarker $ TL.unpack msg

-- | The 'tracetMarkerIO' function emits a marker to the eventlog, if eventlog
-- profiling is available and enabled at runtime.
--
-- Compared to 'tracetMarker', 'tracetMarkerIO' sequences the event with respect to
-- other IO actions.
--
-- /Since: 2/
tracetMarkerIO :: TS.Text -> IO ()
tracetMarkerIO = traceMarkerIO . TS.unpack

-- | Like 'tracetMarkerIO' but accepts a lazy 'TL.Text' argument.
--
-- /Since: 2/
tracetlMarkerIO :: TL.Text -> IO ()
tracetlMarkerIO = traceMarkerIO . TL.unpack
#endif
