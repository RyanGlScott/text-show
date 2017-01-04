{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Dynamic
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'Dynamic'.
-}
module Instances.Data.Dynamic () where

import Data.Dynamic (Dynamic, toDyn)

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), Gen)

instance Arbitrary Dynamic where
    arbitrary = toDyn <$> (arbitrary :: Gen Int)
