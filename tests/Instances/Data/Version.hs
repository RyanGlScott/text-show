{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Version
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'Version'.
-}
module Instances.Data.Version () where

import Data.Version (Version(..))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

instance Arbitrary Version where
    arbitrary = Version <$> arbitrary <*> arbitrary
