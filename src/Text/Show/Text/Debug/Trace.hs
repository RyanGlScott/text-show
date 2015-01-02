{-# LANGUAGE CPP #-}
{-|
Module:      Text.Show.Text.Debug.Trace
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Functions for tracing and monitoring execution.

These can be useful for investigating bugs or performance problems.
They should /not/ be used in production code.

/Since: 0.5/
-}
module Text.Show.Text.Debug.Trace (
      traceIO
    , traceIOLazy
    , trace
    , traceLazy
    , traceId
    , traceIdLazy
    , traceShow
    , traceShowId
    , traceM
    , traceMLazy
    , traceShowM
#if MIN_VERSION_base(4,5,0)
    , traceStack
    , traceStackLazy
    , traceEvent
    , traceEventLazy
    , traceEventIO
    , traceEventIOLazy
#endif
#if MIN_VERSION_base(4,7,0)
    , traceMarker
    , traceMarkerLazy
    , traceMarkerIO
    , traceMarkerIOLazy
#endif
    ) where

import qualified Data.Text as TS (Text, unpack)
import qualified Data.Text.Lazy as TL (Text, unpack)

import qualified Debug.Trace as S

import           Prelude hiding (Show(show))

import           Text.Show.Text.Classes (Show, showLazy)
import           Text.Show.Text.Instances ()

-- | The 'traceIO' function outputs the trace message from the IO monad.
-- This sequences the output with respect to other IO actions.
-- 
-- /Since: 0.5/
traceIO :: TS.Text -> IO ()
#if MIN_VERSION_base(4,5,0)
traceIO = S.traceIO . TS.unpack
#else
traceIO = S.putTraceMsg . TS.unpack
#endif
{-# INLINE traceIO #-}

-- | Like 'traceIO' but accepts a lazy 'TL.Text' argument.
-- 
-- /Since: 0.5/
traceIOLazy :: TL.Text -> IO ()
#if MIN_VERSION_base(4,5,0)
traceIOLazy = S.traceIO . TL.unpack
#else
traceIOLazy = S.putTraceMsg . TL.unpack
#endif
{-# INLINE traceIOLazy #-}

{-|
The 'trace' function outputs the trace message given as its first argument,
before returning the second argument as its result.

For example, this returns the value of @f x@ but first outputs the message.

> trace ("calling f with x = " <> show x) (f x)

The 'trace' function should /only/ be used for debugging, or for monitoring
execution. The function is not referentially transparent: its type indicates
that it is a pure function but it has the side effect of outputting the
trace message.

/Since: 0.5/
-}
trace :: TS.Text -> a -> a
trace text = S.trace $ TS.unpack text
{-# INLINE trace #-}

-- | Like 'trace' but accepts a lazy 'TL.Text' argument.
-- 
-- /Since: 0.5/
traceLazy :: TL.Text -> a -> a
traceLazy text = S.trace $ TL.unpack text
{-# INLINE traceLazy #-}

-- | Like 'trace' but returns the message instead of a third value.
-- 
-- /Since: 0.5/
traceId :: TS.Text -> TS.Text
traceId a = trace a a
{-# INLINE traceId #-}

-- | Like 'traceId' but accepts a lazy 'TL.Text' argument.
-- 
-- /Since: 0.5/
traceIdLazy :: TL.Text -> TL.Text
traceIdLazy a = traceLazy a a
{-# INLINE traceIdLazy #-}

{-|
Like 'trace', but uses 'show' on the argument to convert it to a 'TS.Text'.

This makes it convenient for printing the values of interesting variables or
expressions inside a function. For example here we print the value of the
variables @x@ and @z@:

> f x y =
>     traceShow (x, z) $ result
>   where
>     z = ...
>     ...

/Since: 0.5/
-}
traceShow :: Show a => a -> b -> b
traceShow = traceLazy . showLazy
{-# INLINE traceShow #-}

-- | Like 'traceShow' but returns the shown value instead of a third value.
-- 
-- /Since: 0.5/
traceShowId :: Show a => a -> a
traceShowId a = traceLazy (showLazy a) a
{-# INLINE traceShowId #-}

{-|
Like 'trace' but returning unit in an arbitrary monad. Allows for convenient
use in do-notation. Note that the application of 'trace' is not an action in the
monad, as 'traceIO' is in the 'IO' monad.

> ... = do
>   x <- ...
>   traceM $ "x: " <> show x
>   y <- ...
>   traceM $ "y: " <> show y

/Since: 0.5/
-}
traceM :: Monad m => TS.Text -> m ()
traceM text = trace text $ return ()
{-# INLINE traceM #-}

-- | Like 'traceM' but accepts a lazy 'TL.Text' argument.
traceMLazy :: Monad m => TL.Text -> m ()
traceMLazy text = traceLazy text $ return ()
{-# INLINE traceMLazy #-}

{-|
Like 'traceM', but uses 'show' on the argument to convert it to a 'TS.Text'.

> ... = do
>   x <- ...
>   traceMShow $ x
>   y <- ...
>   traceMShow $ x + y

/Since: 0.5/
-}
traceShowM :: (Show a, Monad m) => a -> m ()
traceShowM = traceMLazy . showLazy
{-# INLINE traceShowM #-}

#if MIN_VERSION_base(4,5,0)
-- | Like 'trace' but additionally prints a call stack if one is
-- available.
--
-- In the current GHC implementation, the call stack is only
-- availble if the program was compiled with @-prof@; otherwise
-- 'traceStack' behaves exactly like 'trace'.  Entries in the call
-- stack correspond to @SCC@ annotations, so it is a good idea to use
-- @-fprof-auto@ or @-fprof-auto-calls@ to add SCC annotations automatically.
-- 
-- /Since: 0.5/
traceStack :: TS.Text -> a -> a
traceStack text = S.traceStack $ TS.unpack text
{-# INLINE traceStack #-}

-- | Like 'traceStack' but accepts a lazy 'TL.Text' argument.
-- 
-- /Since: 0.5/
traceStackLazy :: TL.Text -> a -> a
traceStackLazy text = S.traceStack $ TL.unpack text
{-# INLINE traceStackLazy #-}

-- | The 'traceEvent' function behaves like 'trace' with the difference that
-- the message is emitted to the eventlog, if eventlog profiling is available
-- and enabled at runtime.
-- 
-- It is suitable for use in pure code. In an IO context use 'traceEventIO'
-- instead.
-- 
-- Note that when using GHC's SMP runtime, it is possible (but rare) to get
-- duplicate events emitted if two CPUs simultaneously evaluate the same thunk
-- that uses 'traceEvent'.
-- 
-- /Since: 0.5/
traceEvent :: TS.Text -> a -> a
traceEvent msg = S.traceEvent $ TS.unpack msg
{-# INLINE traceEvent #-}

-- | Like 'traceEvent' but accepts a lazy 'TL.Text' argument.
-- 
-- /Since: 0.5/
traceEventLazy :: TL.Text -> a -> a
traceEventLazy msg = S.traceEvent $ TL.unpack msg
{-# INLINE traceEventLazy #-}

-- | The 'traceEventIO' function emits a message to the eventlog, if eventlog
-- profiling is available and enabled at runtime.
--
-- Compared to 'traceEvent', 'traceEventIO' sequences the event with respect to
-- other IO actions.
-- 
-- /Since: 0.5/
traceEventIO :: TS.Text -> IO ()
traceEventIO = S.traceEventIO . TS.unpack
{-# INLINE traceEventIO #-}

-- | Like 'traceEventIO' but accepts a lazy 'TL.Text' argument.
-- 
-- /Since: 0.5/
traceEventIOLazy :: TL.Text -> IO ()
traceEventIOLazy = S.traceEventIO . TL.unpack
{-# INLINE traceEventIOLazy #-}
#endif

#if MIN_VERSION_base(4,7,0)
-- | The 'traceMarker' function emits a marker to the eventlog, if eventlog
-- profiling is available and enabled at runtime. The 'TS.Text' is the name of
-- the marker. The name is just used in the profiling tools to help you keep
-- clear which marker is which.
--
-- This function is suitable for use in pure code. In an IO context use
-- 'traceMarkerIO' instead.
--
-- Note that when using GHC's SMP runtime, it is possible (but rare) to get
-- duplicate events emitted if two CPUs simultaneously evaluate the same thunk
-- that uses 'traceMarker'.
-- 
-- /Since: 0.5/
traceMarker :: TS.Text -> a -> a
traceMarker msg = S.traceMarker $ TS.unpack msg
{-# INLINE traceMarker #-}

-- | Like 'traceMarker' but accepts a lazy 'TL.Text' argument.
-- 
-- /Since: 0.5/
traceMarkerLazy :: TL.Text -> a -> a
traceMarkerLazy msg = S.traceMarker $ TL.unpack msg
{-# INLINE traceMarkerLazy #-}

-- | The 'traceMarkerIO' function emits a marker to the eventlog, if eventlog
-- profiling is available and enabled at runtime.
--
-- Compared to 'traceMarker', 'traceMarkerIO' sequences the event with respect to
-- other IO actions.
-- 
-- /Since: 0.5/
traceMarkerIO :: TS.Text -> IO ()
traceMarkerIO = S.traceMarkerIO . TS.unpack
{-# INLINE traceMarkerIO #-}

-- | Like 'traceMarkerIO' but accepts a lazy 'TL.Text' argument.
-- 
-- /Since: 0.5/
traceMarkerIOLazy :: TL.Text -> IO ()
traceMarkerIOLazy = S.traceMarkerIO . TL.unpack
{-# INLINE traceMarkerIOLazy #-}
#endif