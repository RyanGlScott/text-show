{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Control.Exception
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for 'Exception's.

/Since: 2/
-}
module TextShow.Control.Exception (
    showbSomeExceptionPrec
  , showbIOException
  , showbArithException
  , showbArrayException
  , showbAssertionFailed
#if MIN_VERSION_base(4,7,0)
  , showbSomeAsyncException
#endif
  , showbAsyncException
  , showbNonTermination
  , showbNestedAtomically
  , showbBlockedIndefinitelyOnMVar
  , showbBlockedIndefinitelyOnSTM
#if MIN_VERSION_base(4,8,0)
  , showbAllocationLimitExceeded
#endif
#if MIN_VERSION_base(4,9,0)
  , showbTypeError
#endif
#if MIN_VERSION_base(4,10,0)
  , showbCompactionFailed
#endif
  , showbDeadlock
  , showbNoMethodError
  , showbPatternMatchFail
  , showbRecConError
  , showbRecSelError
  , showbRecUpdError
  , showbErrorCall
  , showbMaskingState
  ) where

import Control.Exception.Base

import Data.Monoid.Compat ((<>))
import Data.Text.Lazy.Builder (Builder, fromString)
#if MIN_VERSION_base(4,9,0)
import Data.Text.Lazy.Builder (singleton)
#endif

import Prelude ()
import Prelude.Compat

import TextShow.Classes (TextShow(..))
import TextShow.FromStringTextShow (FromStringShow(..))
import TextShow.TH.Internal (deriveTextShow)

#include "inline.h"

-- | Convert a 'SomeException' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbSomeExceptionPrec :: Int -> SomeException -> Builder
showbSomeExceptionPrec p (SomeException e) = showbPrec p $ FromStringShow e
{-# INLINE showbSomeExceptionPrec #-}

-- | Convert an 'IOException' to a 'Builder'.
--
-- /Since: 2/
showbIOException :: IOException -> Builder
showbIOException = showb . FromStringShow
{-# INLINE showbIOException #-}

-- | Convert an 'ArithException' to a 'Builder'.
--
-- /Since: 2/
showbArithException :: ArithException -> Builder
showbArithException Overflow             = "arithmetic overflow"
showbArithException Underflow            = "arithmetic underflow"
showbArithException LossOfPrecision      = "loss of precision"
showbArithException DivideByZero         = "divide by zero"
showbArithException Denormal             = "denormal"
#if MIN_VERSION_base(4,6,0)
showbArithException RatioZeroDenominator = "Ratio has zero denominator"
#endif

-- | Convert an 'ArrayException' to a 'Builder'.
--
-- /Since: 2/
showbArrayException :: ArrayException -> Builder
showbArrayException (IndexOutOfBounds s)
    =  "array index out of range"
    <> (if not $ null s then ": " <> fromString s
                        else mempty)
showbArrayException (UndefinedElement s)
    =  "undefined array element"
    <> (if not $ null s then ": " <> fromString s
                        else mempty)
{-# INLINE showbArrayException #-}

-- | Convert an 'AssertionFailed' exception to a 'Builder'.
--
-- /Since: 2/
showbAssertionFailed :: AssertionFailed -> Builder
showbAssertionFailed (AssertionFailed err) = fromString err
{-# INLINE showbAssertionFailed #-}

#if MIN_VERSION_base(4,7,0)
-- | Convert a 'SomeAsyncException' value to a 'Builder'.
-- This function is only available with @base-4.7.0.0@ or later.
--
-- /Since: 2/
showbSomeAsyncException :: SomeAsyncException -> Builder
showbSomeAsyncException (SomeAsyncException e) = showb $ FromStringShow e
{-# INLINE showbSomeAsyncException #-}
#endif

-- | Convert an 'AsyncException' to a 'Builder'.
--
-- /Since: 2/
showbAsyncException :: AsyncException -> Builder
showbAsyncException StackOverflow = "stack overflow"
showbAsyncException HeapOverflow  = "heap overflow"
showbAsyncException ThreadKilled  = "thread killed"
showbAsyncException UserInterrupt = "user interrupt"
{-# INLINE showbAsyncException #-}

-- | Convert a 'NonTermination' exception to a 'Builder'.
--
-- /Since: 2/
showbNonTermination :: NonTermination -> Builder
showbNonTermination NonTermination = "<<loop>>"
{-# INLINE showbNonTermination #-}

-- | Convert a 'NestedAtomically' exception to a 'Builder'.
--
-- /Since: 2/
showbNestedAtomically :: NestedAtomically -> Builder
showbNestedAtomically NestedAtomically = "Control.Concurrent.STM.atomically was nested"
{-# INLINE showbNestedAtomically #-}

-- | Convert a 'BlockedIndefinitelyOnMVar' exception to a 'Builder'.
--
-- /Since: 2/
showbBlockedIndefinitelyOnMVar :: BlockedIndefinitelyOnMVar -> Builder
showbBlockedIndefinitelyOnMVar BlockedIndefinitelyOnMVar = "thread blocked indefinitely in an MVar operation"
{-# INLINE showbBlockedIndefinitelyOnMVar #-}

-- | Convert a 'BlockedIndefinitelyOnSTM' exception to a 'Builder'.
--
-- /Since: 2/
showbBlockedIndefinitelyOnSTM :: BlockedIndefinitelyOnSTM -> Builder
showbBlockedIndefinitelyOnSTM BlockedIndefinitelyOnSTM = "thread blocked indefinitely in an STM transaction"
{-# INLINE showbBlockedIndefinitelyOnSTM #-}

#if MIN_VERSION_base(4,8,0)
-- | Convert an 'AllocationLimitExceeded' exception to a 'Builder'.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 2/
showbAllocationLimitExceeded :: AllocationLimitExceeded -> Builder
showbAllocationLimitExceeded AllocationLimitExceeded = "allocation limit exceeded"
{-# INLINE showbAllocationLimitExceeded #-}
#endif

#if MIN_VERSION_base(4,9,0)
-- | Convert a 'TypeError' to a 'Builder'.
-- This function is only available with @base-4.9.0.0@ or later.
--
-- /Since: 3/
showbTypeError :: TypeError -> Builder
showbTypeError (TypeError err) = fromString err
{-# INLINE showbTypeError #-}
#endif

#if MIN_VERSION_base(4,10,0)
-- | Convert a 'CompactionFailed' value to a 'Builder'.
-- This function is only available with @base-4.10.0.0@ or later.
--
-- /Since: next/
showbCompactionFailed :: CompactionFailed -> Builder
showbCompactionFailed (CompactionFailed why) = fromString ("compaction failed: " <> why)
#endif

-- | Convert a 'Deadlock' exception to a 'Builder'.
--
-- /Since: 2/
showbDeadlock :: Deadlock -> Builder
showbDeadlock Deadlock = "<<deadlock>>"
{-# INLINE showbDeadlock #-}

-- | Convert a 'NoMethodError' to a 'Builder'.
--
-- /Since: 2/
showbNoMethodError :: NoMethodError -> Builder
showbNoMethodError (NoMethodError err) = fromString err
{-# INLINE showbNoMethodError #-}

-- | Convert a 'PatternMatchFail' to a 'Builder'.
--
-- /Since: 2/
showbPatternMatchFail :: PatternMatchFail -> Builder
showbPatternMatchFail (PatternMatchFail err) = fromString err
{-# INLINE showbPatternMatchFail #-}

-- | Convert a 'RecConError' to a 'Builder'.
--
-- /Since: 2/
showbRecConError :: RecConError -> Builder
showbRecConError (RecConError err) = fromString err
{-# INLINE showbRecConError #-}

-- | Convert a 'RecSelError' to a 'Builder'.
--
-- /Since: 2/
showbRecSelError :: RecSelError -> Builder
showbRecSelError (RecSelError err) = fromString err
{-# INLINE showbRecSelError #-}

-- | Convert a 'RecUpdError' to a 'Builder'.
--
-- /Since: 2/
showbRecUpdError :: RecUpdError -> Builder
showbRecUpdError (RecUpdError err) = fromString err
{-# INLINE showbRecUpdError #-}

-- | Convert an 'ErrorCall' to a 'Builder'.
--
-- /Since: 2/
showbErrorCall :: ErrorCall -> Builder
#if MIN_VERSION_base(4,9,0)
showbErrorCall (ErrorCallWithLocation err "")  = fromString err
showbErrorCall (ErrorCallWithLocation err loc) =
  fromString err <> singleton '\n' <> fromString loc
#else
showbErrorCall (ErrorCall err) = fromString err
#endif

-- | Convert a 'MaskingState' to a 'Builder'.
--
-- /Since: 2/
showbMaskingState :: MaskingState -> Builder
showbMaskingState = showb
{-# INLINE showbMaskingState #-}

instance TextShow SomeException where
    showbPrec = showbSomeExceptionPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow IOException where
    showb = showbIOException
    INLINE_INST_FUN(showb)

instance TextShow ArithException where
    showb = showbArithException
    INLINE_INST_FUN(showb)

instance TextShow ArrayException where
    showb = showbArrayException
    INLINE_INST_FUN(showb)

instance TextShow AssertionFailed where
    showb = showbAssertionFailed
    INLINE_INST_FUN(showb)

#if MIN_VERSION_base(4,7,0)
instance TextShow SomeAsyncException where
    showb = showbSomeAsyncException
    {-# INLINE showb #-}
#endif

instance TextShow AsyncException where
    showb = showbAsyncException
    INLINE_INST_FUN(showb)

instance TextShow NonTermination where
    showb = showbNonTermination
    INLINE_INST_FUN(showb)

instance TextShow NestedAtomically where
    showb = showbNestedAtomically
    INLINE_INST_FUN(showb)

instance TextShow BlockedIndefinitelyOnMVar where
    showb = showbBlockedIndefinitelyOnMVar
    INLINE_INST_FUN(showb)

instance TextShow BlockedIndefinitelyOnSTM where
    showb = showbBlockedIndefinitelyOnSTM
    INLINE_INST_FUN(showb)

#if MIN_VERSION_base(4,8,0)
instance TextShow AllocationLimitExceeded where
    showb = showbAllocationLimitExceeded
    {-# INLINE showb #-}
#endif

#if MIN_VERSION_base(4,9,0)
instance TextShow TypeError where
    showb = showbTypeError
    {-# INLINE showb #-}
#endif

#if MIN_VERSION_base(4,10,0)
instance TextShow CompactionFailed where
    showb = showbCompactionFailed
    {-# INLINE showb #-}
#endif

instance TextShow Deadlock where
    showb = showbDeadlock
    INLINE_INST_FUN(showb)

instance TextShow NoMethodError where
    showb = showbNoMethodError
    INLINE_INST_FUN(showb)

instance TextShow PatternMatchFail where
    showb = showbPatternMatchFail
    INLINE_INST_FUN(showb)

instance TextShow RecConError where
    showb = showbRecConError
    INLINE_INST_FUN(showb)

instance TextShow RecSelError where
    showb = showbRecSelError
    INLINE_INST_FUN(showb)

instance TextShow RecUpdError where
    showb = showbRecUpdError
    INLINE_INST_FUN(showb)

instance TextShow ErrorCall where
    showb = showbErrorCall
    INLINE_INST_FUN(showb)

$(deriveTextShow ''MaskingState)
