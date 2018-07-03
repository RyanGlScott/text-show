{-# LANGUAGE CPP                #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE DerivingVia        #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Control.Exception
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for 'Exception' data types.

/Since: 2/
-}
module TextShow.Control.Exception () where

import Control.Exception.Base

import Data.Text.Lazy.Builder (fromString)
#if MIN_VERSION_base(4,9,0)
import Data.Text.Lazy.Builder (singleton)
#endif

import Prelude ()
import Prelude.Compat

import TextShow.Classes (TextShow(..))
import TextShow.FromStringTextShow (FromStringShow(..))
import TextShow.TH.Internal (deriveTextShow)

-- | /Since: 2/
#if __GLASGOW_HASKELL__ >= 806
deriving via FromStringShow SomeException instance TextShow SomeException
#else
instance TextShow SomeException where
    showbPrec p (SomeException e) = showbPrec p $ FromStringShow e
    {-# INLINE showbPrec #-}
#endif

-- | /Since: 2/
#if __GLASGOW_HASKELL__ >= 806
deriving via FromStringShow IOException instance TextShow IOException
#else
instance TextShow IOException where
    showb = showb . FromStringShow
    {-# INLINE showb #-}
#endif

-- | /Since: 2/
instance TextShow ArithException where
    showb Overflow             = "arithmetic overflow"
    showb Underflow            = "arithmetic underflow"
    showb LossOfPrecision      = "loss of precision"
    showb DivideByZero         = "divide by zero"
    showb Denormal             = "denormal"
#if MIN_VERSION_base(4,6,0)
    showb RatioZeroDenominator = "Ratio has zero denominator"
#endif

-- | /Since: 2/
instance TextShow ArrayException where
    showb (IndexOutOfBounds s)
        =  "array index out of range"
        <> (if not $ null s then ": " <> fromString s
                            else mempty)
    showb (UndefinedElement s)
        =  "undefined array element"
        <> (if not $ null s then ": " <> fromString s
                            else mempty)
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow AssertionFailed where
    showb (AssertionFailed err) = fromString err
    {-# INLINE showb #-}

#if MIN_VERSION_base(4,7,0)
-- | Only available with @base-4.7.0.0@ or later.
--
-- /Since: 2/
instance TextShow SomeAsyncException where
    showb (SomeAsyncException e) = showb $ FromStringShow e
    {-# INLINE showb #-}
#endif

-- | /Since: 2/
instance TextShow AsyncException where
    showb StackOverflow = "stack overflow"
    showb HeapOverflow  = "heap overflow"
    showb ThreadKilled  = "thread killed"
    showb UserInterrupt = "user interrupt"
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow NonTermination where
    showb NonTermination = "<<loop>>"
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow NestedAtomically where
    showb NestedAtomically = "Control.Concurrent.STM.atomically was nested"
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow BlockedIndefinitelyOnMVar where
    showb BlockedIndefinitelyOnMVar = "thread blocked indefinitely in an MVar operation"
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow BlockedIndefinitelyOnSTM where
    showb BlockedIndefinitelyOnSTM = "thread blocked indefinitely in an STM transaction"
    {-# INLINE showb #-}

#if MIN_VERSION_base(4,8,0)
-- | Only available with @base-4.8.0.0@ or later.
--
-- /Since: 2/
instance TextShow AllocationLimitExceeded where
    showb AllocationLimitExceeded = "allocation limit exceeded"
    {-# INLINE showb #-}
#endif

#if MIN_VERSION_base(4,9,0)
-- | Only available with @base-4.9.0.0@ or later.
--
-- /Since: 3/
instance TextShow TypeError where
    showb (TypeError err) = fromString err
    {-# INLINE showb #-}
#endif

#if MIN_VERSION_base(4,10,0)
-- | Only available with @base-4.10.0.0@ or later.
--
-- /Since: 3.6/
instance TextShow CompactionFailed where
    showb (CompactionFailed why) = fromString ("compaction failed: " <> why)
#endif

#if MIN_VERSION_base(4,11,0)
-- | Only available with @base-4.11.0.0@ or later.
--
-- /Since: 3.7.3/
instance TextShow FixIOException where
    showbPrec _ FixIOException = fromString "cyclic evaluation in fixIO"
#endif

-- | /Since: 2/
instance TextShow Deadlock where
    showb Deadlock = "<<deadlock>>"
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow NoMethodError where
    showb (NoMethodError err) = fromString err
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow PatternMatchFail where
    showb (PatternMatchFail err) = fromString err
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow RecConError where
    showb (RecConError err) = fromString err
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow RecSelError where
    showb (RecSelError err) = fromString err
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow RecUpdError where
    showb (RecUpdError err) = fromString err
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow ErrorCall where
#if MIN_VERSION_base(4,9,0)
    showb (ErrorCallWithLocation err "")  = fromString err
    showb (ErrorCallWithLocation err loc) =
      fromString err <> singleton '\n' <> fromString loc
#else
    showb (ErrorCall err) = fromString err
#endif

-- | /Since: 2/
$(deriveTextShow ''MaskingState)
