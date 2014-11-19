{-# LANGUAGE CPP, NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Data.Monoid
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' functions for 'Exception's.
----------------------------------------------------------------------------
module Text.Show.Text.Control.Exception (
    showbSomeExceptionPrec
  , showbIOException
  , showbArithException
  , showbArrayException
  , showbAssertionFailed
  , showbSomeAsyncException
  , showbAsyncException
  , showbNonTermination
  , showbNestedAtomically
  , showbBlockedIndefinitelyOnMVar
  , showbBlockedIndefinitelyOnSTM
  , showbDeadlock
  , showbNoMethodError
  , showbPatternMatchFail
  , showbRecConError
  , showbRecSelError
  , showbRecUpdError
  , showbErrorCall
  ) where

import           Control.Exception.Base

import           Data.Monoid ((<>), mempty)
import           Data.Text.Lazy.Builder (Builder, fromString)

import qualified Prelude as P
import           Prelude hiding (Show)

import           Text.Show.Text.Class (Show(showb, showbPrec))

-- | Convert a 'SomeException' value to a 'Builder' with the given precedence.
showbSomeExceptionPrec :: Int -> SomeException -> Builder
showbSomeExceptionPrec p (SomeException e) = fromString $ P.showsPrec p e ""
{-# INLINE showbSomeExceptionPrec #-}

-- | Convert an 'IOException' to a 'Builder'.
showbIOException :: IOException -> Builder
showbIOException = fromString . show
{-# INLINE showbIOException #-}

-- | Convert an 'ArithException' to a 'Builder'.
showbArithException :: ArithException -> Builder
showbArithException Overflow             = "arithmetic overflow"
showbArithException Underflow            = "arithmetic underflow"
showbArithException LossOfPrecision      = "loss of precision"
showbArithException DivideByZero         = "divide by zero"
showbArithException Denormal             = "denormal"
#if MIN_VERSION_base(4,6,0)
showbArithException RatioZeroDenominator = "Ratio has zero denominator"
#endif
{-# INLINE showbArithException #-}

-- | Convert an 'ArrayException' to a 'Builder'.
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
showbAssertionFailed :: AssertionFailed -> Builder
showbAssertionFailed (AssertionFailed err) = fromString err
{-# INLINE showbAssertionFailed #-}

-- | Convert a 'SomeAsyncException' value to a 'Builder'.
showbSomeAsyncException :: SomeAsyncException -> Builder
showbSomeAsyncException (SomeAsyncException e) = fromString $ P.show e
{-# INLINE showbSomeAsyncException #-}

-- | Convert an 'AsyncException' to a 'Builder'.
showbAsyncException :: AsyncException -> Builder
showbAsyncException StackOverflow = "stack overflow"
showbAsyncException HeapOverflow  = "heap overflow"
showbAsyncException ThreadKilled  = "thread killed"
showbAsyncException UserInterrupt = "user interrupt"
{-# INLINE showbAsyncException #-}

-- | Convert a 'NonTermination' exception to a 'Builder'.
showbNonTermination :: NonTermination -> Builder
showbNonTermination NonTermination = "<<loop>>"
{-# INLINE showbNonTermination #-}

-- | Convert a 'NestedAtomically' exception to a 'Builder'.
showbNestedAtomically :: NestedAtomically -> Builder
showbNestedAtomically NestedAtomically = "Control.Concurrent.STM.atomically was nested"
{-# INLINE showbNestedAtomically #-}

-- | Convert a 'BlockedIndefinitelyOnMVar' exception to a 'Builder'.
showbBlockedIndefinitelyOnMVar :: BlockedIndefinitelyOnMVar -> Builder
showbBlockedIndefinitelyOnMVar BlockedIndefinitelyOnMVar = "thread blocked indefinitely in an MVar operation"
{-# INLINE showbBlockedIndefinitelyOnMVar #-}

-- | Convert a 'BlockedIndefinitelyOnSTM' exception to a 'Builder'.
showbBlockedIndefinitelyOnSTM :: BlockedIndefinitelyOnSTM -> Builder
showbBlockedIndefinitelyOnSTM BlockedIndefinitelyOnSTM = "thread blocked indefinitely in an STM transaction"
{-# INLINE showbBlockedIndefinitelyOnSTM #-}

-- | Convert a 'Deadlock' exception to a 'Builder'.
showbDeadlock :: Deadlock -> Builder
showbDeadlock Deadlock = "<<deadlock>>"
{-# INLINE showbDeadlock #-}

-- | Convert a 'NoMethodError' to a 'Builder'.
showbNoMethodError :: NoMethodError -> Builder
showbNoMethodError (NoMethodError err) = fromString err
{-# INLINE showbNoMethodError #-}

-- | Convert a 'PatternMatchFail' to a 'Builder'.
showbPatternMatchFail :: PatternMatchFail -> Builder
showbPatternMatchFail (PatternMatchFail err) = fromString err
{-# INLINE showbPatternMatchFail #-}

-- | Convert a 'RecConError' to a 'Builder'.
showbRecConError :: RecConError -> Builder
showbRecConError (RecConError err) = fromString err
{-# INLINE showbRecConError #-}

-- | Convert a 'RecSelError' to a 'Builder'.
showbRecSelError :: RecSelError -> Builder
showbRecSelError (RecSelError err) = fromString err
{-# INLINE showbRecSelError #-}

-- | Convert a 'RecUpdError' to a 'Builder'.
showbRecUpdError :: RecUpdError -> Builder
showbRecUpdError (RecUpdError err) = fromString err
{-# INLINE showbRecUpdError #-}

-- | Convert an 'ErrorCall' to a 'Builder'.
showbErrorCall :: ErrorCall -> Builder
showbErrorCall (ErrorCall err) = fromString err
{-# INLINE showbErrorCall #-}

-- | Convert a 'MaskingState' to a 'Builder'.
showbMaskingState :: MaskingState -> Builder
showbMaskingState Unmasked              = "Unmasked"
showbMaskingState MaskedInterruptible   = "MaskedInterruptible"
showbMaskingState MaskedUninterruptible = "MaskedUninterruptible"
{-# INLINE showbMaskingState #-}

instance Show SomeException where
    showbPrec = showbSomeExceptionPrec
    {-# INLINE showbPrec #-}

instance Show IOException where
    showb = showbIOException
    {-# INLINE showb #-}

instance Show ArithException where
    showb = showbArithException
    {-# INLINE showb #-}

instance Show ArrayException where
    showb = showbArrayException
    {-# INLINE showb #-}

instance Show AssertionFailed where
    showb = showbAssertionFailed
    {-# INLINE showb #-}

instance Show SomeAsyncException where
    showb = showbSomeAsyncException
    {-# INLINE showb #-}

instance Show AsyncException where
    showb = showbAsyncException
    {-# INLINE showb #-}

instance Show NonTermination where
    showb = showbNonTermination
    {-# INLINE showb #-}

instance Show NestedAtomically where
    showb = showbNestedAtomically
    {-# INLINE showb #-}

instance Show BlockedIndefinitelyOnMVar where
    showb = showbBlockedIndefinitelyOnMVar
    {-# INLINE showb #-}

instance Show BlockedIndefinitelyOnSTM where
    showb = showbBlockedIndefinitelyOnSTM
    {-# INLINE showb #-}

instance Show Deadlock where
    showb = showbDeadlock
    {-# INLINE showb #-}

instance Show NoMethodError where
    showb = showbNoMethodError
    {-# INLINE showb #-}

instance Show PatternMatchFail where
    showb = showbPatternMatchFail
    {-# INLINE showb #-}

instance Show RecConError where
    showb = showbRecConError
    {-# INLINE showb #-}

instance Show RecSelError where
    showb = showbRecSelError
    {-# INLINE showb #-}

instance Show RecUpdError where
    showb = showbRecUpdError
    {-# INLINE showb #-}

instance Show ErrorCall where
    showb = showbErrorCall
    {-# INLINE showb #-}

instance Show MaskingState where
    showb = showbMaskingState
    {-# INLINE showb #-}