{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.System.IO
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "System.IO" module.
-}
module Instances.System.IO () where

import GHC.Generics (Generic)
import GHC.IO.Encoding.Failure (CodingFailureMode(..))
import GHC.IO.Encoding.Types (CodingProgress(..))
import GHC.IO.Handle (HandlePosn(..))

import Instances.Utils.GenericArbitrary (genericArbitrary)

import Prelude ()
import Prelude.Compat

import System.IO (BufferMode(..), IOMode(..), Newline(..), NewlineMode(..),
                  SeekMode(..), Handle, stdin, stdout, stderr)

import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, oneof)

instance Arbitrary Handle where
    arbitrary = oneof $ map pure [stdin, stdout, stderr]

instance Arbitrary HandlePosn where
    arbitrary = genericArbitrary

deriving instance Bounded IOMode
instance Arbitrary IOMode where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary BufferMode where
    arbitrary = genericArbitrary

deriving instance Bounded SeekMode
instance Arbitrary SeekMode where
    arbitrary = arbitraryBoundedEnum

deriving instance Bounded CodingProgress
deriving instance Enum CodingProgress
instance Arbitrary CodingProgress where
    arbitrary = arbitraryBoundedEnum

deriving instance Bounded CodingFailureMode
deriving instance Enum CodingFailureMode
instance Arbitrary CodingFailureMode where
    arbitrary = arbitraryBoundedEnum

deriving instance Bounded Newline
deriving instance Enum Newline
instance Arbitrary Newline where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary NewlineMode where
    arbitrary = genericArbitrary

deriving instance Generic HandlePosn
deriving instance Generic BufferMode
deriving instance Generic NewlineMode
