{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Control.Exception
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for 'Exception's.

/Since: 0.3/
-}
module Text.Show.Text.Control.Exception (
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

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (mempty)
#endif
import Data.Text.Lazy.Builder (Builder, fromString)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb, showbPrec), FromStringShow(..))
import Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowb)
import Text.Show.Text.Utils ((<>))

#include "inline.h"

-- | Convert a 'SomeException' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbSomeExceptionPrec :: Int -> SomeException -> Builder
showbSomeExceptionPrec p (SomeException e) = showbPrec p $ FromStringShow e
{-# INLINE showbSomeExceptionPrec #-}

-- | Convert an 'IOException' to a 'Builder'.
-- 
-- /Since: 0.3/
showbIOException :: IOException -> Builder
showbIOException = showb . FromStringShow
{-# INLINE showbIOException #-}

-- | Convert an 'ArithException' to a 'Builder'.
-- 
-- /Since: 0.3/
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
-- /Since: 0.3/
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
-- /Since: 0.3/
showbAssertionFailed :: AssertionFailed -> Builder
showbAssertionFailed (AssertionFailed err) = fromString err
{-# INLINE showbAssertionFailed #-}

#if MIN_VERSION_base(4,7,0)
-- | Convert a 'SomeAsyncException' value to a 'Builder'.
-- This function is only available with @base-4.7.0.0@ or later.
-- 
-- /Since: 0.3/
showbSomeAsyncException :: SomeAsyncException -> Builder
showbSomeAsyncException (SomeAsyncException e) = showb $ FromStringShow e
{-# INLINE showbSomeAsyncException #-}
#endif

-- | Convert an 'AsyncException' to a 'Builder'.
-- 
-- /Since: 0.3/
showbAsyncException :: AsyncException -> Builder
showbAsyncException StackOverflow = "stack overflow"
showbAsyncException HeapOverflow  = "heap overflow"
showbAsyncException ThreadKilled  = "thread killed"
showbAsyncException UserInterrupt = "user interrupt"
{-# INLINE showbAsyncException #-}

-- | Convert a 'NonTermination' exception to a 'Builder'.
-- 
-- /Since: 0.3/
showbNonTermination :: NonTermination -> Builder
showbNonTermination NonTermination = "<<loop>>"
{-# INLINE showbNonTermination #-}

-- | Convert a 'NestedAtomically' exception to a 'Builder'.
-- 
-- /Since: 0.3/
showbNestedAtomically :: NestedAtomically -> Builder
showbNestedAtomically NestedAtomically = "Control.Concurrent.STM.atomically was nested"
{-# INLINE showbNestedAtomically #-}

-- | Convert a 'BlockedIndefinitelyOnMVar' exception to a 'Builder'.
-- 
-- /Since: 0.3/
showbBlockedIndefinitelyOnMVar :: BlockedIndefinitelyOnMVar -> Builder
showbBlockedIndefinitelyOnMVar BlockedIndefinitelyOnMVar = "thread blocked indefinitely in an MVar operation"
{-# INLINE showbBlockedIndefinitelyOnMVar #-}

-- | Convert a 'BlockedIndefinitelyOnSTM' exception to a 'Builder'.
-- 
-- /Since: 0.3/
showbBlockedIndefinitelyOnSTM :: BlockedIndefinitelyOnSTM -> Builder
showbBlockedIndefinitelyOnSTM BlockedIndefinitelyOnSTM = "thread blocked indefinitely in an STM transaction"
{-# INLINE showbBlockedIndefinitelyOnSTM #-}

#if MIN_VERSION_base(4,8,0)
-- | Convert an 'AllocationLimitExceeded' exception to a 'Builder'.
-- This function is only available with @base-4.8.0.0@ or later.
-- 
-- /Since: 0.5/
showbAllocationLimitExceeded :: AllocationLimitExceeded -> Builder
showbAllocationLimitExceeded AllocationLimitExceeded = "allocation limit exceeded"
{-# INLINE showbAllocationLimitExceeded #-}
#endif

-- | Convert a 'Deadlock' exception to a 'Builder'.
-- 
-- /Since: 0.3/
showbDeadlock :: Deadlock -> Builder
showbDeadlock Deadlock = "<<deadlock>>"
{-# INLINE showbDeadlock #-}

-- | Convert a 'NoMethodError' to a 'Builder'.
-- 
-- /Since: 0.3/
showbNoMethodError :: NoMethodError -> Builder
showbNoMethodError (NoMethodError err) = fromString err
{-# INLINE showbNoMethodError #-}

-- | Convert a 'PatternMatchFail' to a 'Builder'.
-- 
-- /Since: 0.3/
showbPatternMatchFail :: PatternMatchFail -> Builder
showbPatternMatchFail (PatternMatchFail err) = fromString err
{-# INLINE showbPatternMatchFail #-}

-- | Convert a 'RecConError' to a 'Builder'.
-- 
-- /Since: 0.3/
showbRecConError :: RecConError -> Builder
showbRecConError (RecConError err) = fromString err
{-# INLINE showbRecConError #-}

-- | Convert a 'RecSelError' to a 'Builder'.
-- 
-- /Since: 0.3/
showbRecSelError :: RecSelError -> Builder
showbRecSelError (RecSelError err) = fromString err
{-# INLINE showbRecSelError #-}

-- | Convert a 'RecUpdError' to a 'Builder'.
-- 
-- /Since: 0.3/
showbRecUpdError :: RecUpdError -> Builder
showbRecUpdError (RecUpdError err) = fromString err
{-# INLINE showbRecUpdError #-}

-- | Convert an 'ErrorCall' to a 'Builder'.
-- 
-- /Since: 0.3/
showbErrorCall :: ErrorCall -> Builder
showbErrorCall (ErrorCall err) = fromString err
{-# INLINE showbErrorCall #-}

-- | Convert a 'MaskingState' to a 'Builder'.
-- 
-- /Since: 0.4/
showbMaskingState :: MaskingState -> Builder
showbMaskingState = showb
{-# INLINE showbMaskingState #-}

instance Show SomeException where
    showbPrec = showbSomeExceptionPrec
    INLINE_INST_FUN(showbPrec)

instance Show IOException where
    showb = showbIOException
    INLINE_INST_FUN(showb)

instance Show ArithException where
    showb = showbArithException
    INLINE_INST_FUN(showb)

instance Show ArrayException where
    showb = showbArrayException
    INLINE_INST_FUN(showb)

instance Show AssertionFailed where
    showb = showbAssertionFailed
    INLINE_INST_FUN(showb)

#if MIN_VERSION_base(4,7,0)
instance Show SomeAsyncException where
    showb = showbSomeAsyncException
    {-# INLINE showb #-}
#endif

instance Show AsyncException where
    showb = showbAsyncException
    INLINE_INST_FUN(showb)

instance Show NonTermination where
    showb = showbNonTermination
    INLINE_INST_FUN(showb)

instance Show NestedAtomically where
    showb = showbNestedAtomically
    INLINE_INST_FUN(showb)

instance Show BlockedIndefinitelyOnMVar where
    showb = showbBlockedIndefinitelyOnMVar
    INLINE_INST_FUN(showb)

instance Show BlockedIndefinitelyOnSTM where
    showb = showbBlockedIndefinitelyOnSTM
    INLINE_INST_FUN(showb)

#if MIN_VERSION_base(4,8,0)
instance Show AllocationLimitExceeded where
    showb = showbAllocationLimitExceeded
    {-# INLINE showb #-}
#endif

instance Show Deadlock where
    showb = showbDeadlock
    INLINE_INST_FUN(showb)

instance Show NoMethodError where
    showb = showbNoMethodError
    INLINE_INST_FUN(showb)

instance Show PatternMatchFail where
    showb = showbPatternMatchFail
    INLINE_INST_FUN(showb)

instance Show RecConError where
    showb = showbRecConError
    INLINE_INST_FUN(showb)

instance Show RecSelError where
    showb = showbRecSelError
    INLINE_INST_FUN(showb)

instance Show RecUpdError where
    showb = showbRecUpdError
    INLINE_INST_FUN(showb)

instance Show ErrorCall where
    showb = showbErrorCall
    INLINE_INST_FUN(showb)

$(deriveShowPragmas defaultInlineShowb ''MaskingState)
