{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Instances
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Miscellaneous typeclass instances.
----------------------------------------------------------------------------
module Instances where

import Control.Applicative
import Control.Exception

#if MIN_VERSION_bytestring(0,10,4)
import Data.ByteString.Short (ShortByteString, pack)
#endif
import Data.Char (GeneralCategory(..))
import Data.Monoid (All(..), Any(..), Dual(..), First(..),
                    Last(..), Product(..), Sum(..))
#if MIN_VERSION_base(4,6,0)
import Data.Ord (Down(..))
#endif
import Data.Text.Lazy.Builder (Builder, fromString)
import Data.Version (Version(..))

import Foreign.C.Types
import Foreign.Ptr (FunPtr, IntPtr, Ptr, WordPtr,
                    castPtrToFunPtr, nullPtr, plusPtr,
                    ptrToIntPtr, ptrToWordPtr)

import System.Exit (ExitCode(..))
import System.Posix.Types

import Test.QuickCheck

instance Arbitrary Builder where
    arbitrary = fromString <$> arbitrary

#if MIN_VERSION_bytestring(0,10,4)
instance Arbitrary ShortByteString where
    arbitrary = pack <$> arbitrary
#endif

instance Arbitrary (Ptr a) where
    arbitrary = plusPtr nullPtr <$> arbitrary

instance Arbitrary (FunPtr a) where
    arbitrary = castPtrToFunPtr <$> arbitrary

instance Arbitrary IntPtr where
    arbitrary = ptrToIntPtr <$> arbitrary

instance Arbitrary WordPtr where
    arbitrary = ptrToWordPtr <$> arbitrary

-- TODO: instance Arbitrary (ForeignPtr a)

instance Arbitrary GeneralCategory where
    arbitrary = oneof $ map pure [ UppercaseLetter     
                                 , LowercaseLetter
                                 , TitlecaseLetter
                                 , ModifierLetter
                                 , OtherLetter
                                 , NonSpacingMark
                                 , SpacingCombiningMark
                                 , EnclosingMark
                                 , DecimalNumber
                                 , LetterNumber
                                 , OtherNumber
                                 , ConnectorPunctuation
                                 , DashPunctuation
                                 , OpenPunctuation
                                 , ClosePunctuation
                                 , InitialQuote
                                 , FinalQuote 
                                 , OtherPunctuation
                                 , MathSymbol
                                 , CurrencySymbol
                                 , ModifierSymbol
                                 , OtherSymbol
                                 , Space
                                 , LineSeparator
                                 , ParagraphSeparator
                                 , Control
                                 , Format
                                 , Surrogate
                                 , PrivateUse
                                 , NotAssigned
                                 ]

instance Arbitrary Version where
    arbitrary = Version <$> arbitrary <*> arbitrary

-- instance Arbitrary SomeException

-- instance Arbitrary IOException

instance Arbitrary ArithException where
    arbitrary = oneof $ map pure [ Overflow
                                 , Underflow
                                 , LossOfPrecision
                                 , DivideByZero
                                 , Denormal
#if MIN_VERSION_base(4,6,0)
                                 , RatioZeroDenominator
#endif
                                 ]

instance Arbitrary ArrayException where
    arbitrary = oneof [IndexOutOfBounds <$> arbitrary, UndefinedElement <$> arbitrary]

instance Arbitrary AssertionFailed where
    arbitrary = AssertionFailed <$> arbitrary

-- instance Arbitrary SomeAsyncException

instance Arbitrary AsyncException where
    arbitrary = oneof $ map pure [ StackOverflow
                                 , HeapOverflow
                                 , ThreadKilled
                                 , UserInterrupt
                                 ]

instance Arbitrary NonTermination where
    arbitrary = pure NonTermination

instance Arbitrary NestedAtomically where
    arbitrary = pure NestedAtomically

instance Arbitrary BlockedIndefinitelyOnMVar where
    arbitrary = pure BlockedIndefinitelyOnMVar

instance Arbitrary BlockedIndefinitelyOnSTM where
    arbitrary = pure BlockedIndefinitelyOnSTM

instance Arbitrary Deadlock where
    arbitrary = pure Deadlock

instance Arbitrary NoMethodError where
    arbitrary = NoMethodError <$> arbitrary

instance Arbitrary PatternMatchFail where
    arbitrary = PatternMatchFail <$> arbitrary

instance Arbitrary RecConError where
    arbitrary = RecConError <$> arbitrary

instance Arbitrary RecSelError where
    arbitrary = RecSelError <$> arbitrary

instance Arbitrary RecUpdError where
    arbitrary = RecUpdError <$> arbitrary

deriving instance Arbitrary ErrorCall

instance Arbitrary MaskingState where
    arbitrary = oneof $ map pure [ Unmasked
                                 , MaskedInterruptible
                                 , MaskedUninterruptible
                                 ]

deriving instance Arbitrary CChar
deriving instance Arbitrary CSChar
deriving instance Arbitrary CUChar
deriving instance Arbitrary CShort
deriving instance Arbitrary CUShort
deriving instance Arbitrary CInt
deriving instance Arbitrary CUInt
deriving instance Arbitrary CLong
deriving instance Arbitrary CULong
deriving instance Arbitrary CLLong
deriving instance Arbitrary CULLong
deriving instance Arbitrary CFloat
deriving instance Arbitrary CDouble
deriving instance Arbitrary CPtrdiff
deriving instance Arbitrary CSize
deriving instance Arbitrary CWchar
deriving instance Arbitrary CSigAtomic
deriving instance Arbitrary CClock
deriving instance Arbitrary CTime
deriving instance Arbitrary CUSeconds
deriving instance Arbitrary CSUSeconds
deriving instance Arbitrary CIntPtr
deriving instance Arbitrary CUIntPtr
deriving instance Arbitrary CIntMax
deriving instance Arbitrary CUIntMax

instance Arbitrary ExitCode where
    arbitrary = oneof [pure ExitSuccess, ExitFailure <$> arbitrary]

deriving instance Arbitrary CDev
deriving instance Arbitrary CIno
deriving instance Arbitrary CMode
deriving instance Arbitrary COff
deriving instance Arbitrary CPid
deriving instance Arbitrary CSsize
deriving instance Arbitrary CGid
deriving instance Arbitrary CNlink
deriving instance Arbitrary CUid
deriving instance Arbitrary CCc
deriving instance Arbitrary CSpeed
deriving instance Arbitrary CTcflag
deriving instance Arbitrary CRLim
deriving instance Arbitrary Fd

deriving instance Arbitrary All
deriving instance Arbitrary Any
deriving instance Arbitrary a => Arbitrary (Dual a)
deriving instance Arbitrary a => Arbitrary (First a)
deriving instance Arbitrary a => Arbitrary (Last a)
deriving instance Arbitrary a => Arbitrary (Product a)
deriving instance Arbitrary a => Arbitrary (Sum a)
 
deriving instance Arbitrary a => Arbitrary (ZipList a)
#if !MIN_VERSION_base(4,7,0)
deriving instance Show a => Show (ZipList a)
#endif

#if MIN_VERSION_base(4,6,0)
deriving instance Arbitrary a => Arbitrary (Down a)
#if !MIN_VERSION_base(4,7,0)
deriving instance Show a => Show (Down a)
#endif
#endif