{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Control.Exception
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "Control.Exception" module.
-}
module Instances.Control.Exception () where

import Control.Exception hiding (IOException)

import GHC.Generics (Generic)
import GHC.IO.Exception (IOException(..), IOErrorType(..))
#if MIN_VERSION_base(4,11,0)
import GHC.IO.Exception (FixIOException(..))
#endif

import Instances.Foreign.C.Types ()
import Instances.System.IO ()
import Instances.Utils.GenericArbitrary (genericArbitrary)

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), Gen, arbitraryBoundedEnum)

instance Arbitrary SomeException where
    arbitrary = SomeException <$> (arbitrary :: Gen AssertionFailed)

instance Arbitrary IOException where
    arbitrary = genericArbitrary

deriving instance Bounded IOErrorType
deriving instance Enum IOErrorType
instance Arbitrary IOErrorType where
    arbitrary = arbitraryBoundedEnum

deriving instance Bounded ArithException
deriving instance Enum ArithException
instance Arbitrary ArithException where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary ArrayException where
    arbitrary = genericArbitrary

instance Arbitrary AssertionFailed where
    arbitrary = genericArbitrary

#if MIN_VERSION_base(4,7,0)
instance Arbitrary SomeAsyncException where
    arbitrary = SomeAsyncException <$> (arbitrary :: Gen AsyncException)
#endif

deriving instance Bounded AsyncException
deriving instance Enum AsyncException
instance Arbitrary AsyncException where
    arbitrary = arbitraryBoundedEnum

deriving instance Bounded NonTermination
deriving instance Enum NonTermination
instance Arbitrary NonTermination where
    arbitrary = arbitraryBoundedEnum

deriving instance Bounded NestedAtomically
deriving instance Enum NestedAtomically
instance Arbitrary NestedAtomically where
    arbitrary = arbitraryBoundedEnum

deriving instance Bounded BlockedIndefinitelyOnMVar
deriving instance Enum BlockedIndefinitelyOnMVar
instance Arbitrary BlockedIndefinitelyOnMVar where
    arbitrary = arbitraryBoundedEnum

deriving instance Bounded BlockedIndefinitelyOnSTM
deriving instance Enum BlockedIndefinitelyOnSTM
instance Arbitrary BlockedIndefinitelyOnSTM where
    arbitrary = arbitraryBoundedEnum

#if MIN_VERSION_base(4,8,0)
deriving instance Bounded AllocationLimitExceeded
deriving instance Enum AllocationLimitExceeded
instance Arbitrary AllocationLimitExceeded where
    arbitrary = arbitraryBoundedEnum
#endif

#if MIN_VERSION_base(4,9,0)
deriving instance Arbitrary TypeError
#endif

#if MIN_VERSION_base(4,10,0)
deriving instance Arbitrary CompactionFailed
#endif

#if MIN_VERSION_base(4,11,0)
deriving instance Bounded FixIOException
deriving instance Enum FixIOException
instance Arbitrary FixIOException where
    arbitrary = arbitraryBoundedEnum
#endif

deriving instance Bounded Deadlock
deriving instance Enum Deadlock
instance Arbitrary Deadlock where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary NoMethodError where
    arbitrary = genericArbitrary

instance Arbitrary PatternMatchFail where
    arbitrary = genericArbitrary

instance Arbitrary RecConError where
    arbitrary = genericArbitrary

instance Arbitrary RecSelError where
    arbitrary = genericArbitrary

instance Arbitrary RecUpdError where
    arbitrary = genericArbitrary

instance Arbitrary ErrorCall where
    arbitrary = genericArbitrary

deriving instance Bounded MaskingState
deriving instance Enum MaskingState
instance Arbitrary MaskingState where
    arbitrary = arbitraryBoundedEnum

deriving instance Generic ArrayException
deriving instance Generic AssertionFailed
deriving instance Generic IOException
deriving instance Generic Deadlock
deriving instance Generic NoMethodError
deriving instance Generic PatternMatchFail
deriving instance Generic RecConError
deriving instance Generic RecSelError
deriving instance Generic RecUpdError
deriving instance Generic ErrorCall
