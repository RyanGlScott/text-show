{-# LANGUAGE CPP                #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Control.Exception
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

'Arbitrary' instances for data types in the "Control.Exception" module.
-}
module Instances.Control.Exception () where

import Control.Exception

import GHC.IO.Exception (IOException(..), IOErrorType(..))

import Instances.Foreign.C.Types ()
import Instances.System.IO ()

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), Gen, arbitraryBoundedEnum, oneof)

instance Arbitrary SomeException where
    arbitrary = SomeException <$> (arbitrary :: Gen AssertionFailed)

instance Arbitrary IOException where
    arbitrary = IOError <$> arbitrary <*> arbitrary <*> arbitrary
                        <*> arbitrary <*> arbitrary <*> arbitrary

deriving instance Bounded IOErrorType
deriving instance Enum IOErrorType
instance Arbitrary IOErrorType where
    arbitrary = arbitraryBoundedEnum

deriving instance Bounded ArithException
deriving instance Enum ArithException
instance Arbitrary ArithException where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary ArrayException where
    arbitrary = oneof [ IndexOutOfBounds <$> arbitrary
                      , UndefinedElement <$> arbitrary
                      ]

instance Arbitrary AssertionFailed where
    arbitrary = AssertionFailed <$> arbitrary

#if MIN_VERSION_base(4,7,0)
instance Arbitrary SomeAsyncException where
    arbitrary = SomeAsyncException <$> (arbitrary :: Gen AsyncException)
#endif

deriving instance Bounded AsyncException
deriving instance Enum AsyncException
instance Arbitrary AsyncException where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary NonTermination where
    arbitrary = pure NonTermination

instance Arbitrary NestedAtomically where
    arbitrary = pure NestedAtomically

instance Arbitrary BlockedIndefinitelyOnMVar where
    arbitrary = pure BlockedIndefinitelyOnMVar

instance Arbitrary BlockedIndefinitelyOnSTM where
    arbitrary = pure BlockedIndefinitelyOnSTM

#if MIN_VERSION_base(4,8,0)
instance Arbitrary AllocationLimitExceeded where
    arbitrary = pure AllocationLimitExceeded
#endif

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

-- ErrorCall is a newtype starting with base-4.7.0.0, but we'll
-- manually derive Arbitrary to support older versions of GHC.
instance Arbitrary ErrorCall where
    arbitrary = ErrorCall <$> arbitrary

deriving instance Bounded MaskingState
deriving instance Enum MaskingState
instance Arbitrary MaskingState where
    arbitrary = arbitraryBoundedEnum
