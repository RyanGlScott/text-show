{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module:      Instances.Data.Type.Equality
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for '(:~:)'.
-}
module Instances.Data.Type.Equality () where

import Data.Type.Equality.Compat

import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

instance a ~ b => Arbitrary (a :~: b) where
    arbitrary = arbitraryBoundedEnum

instance a ~~ b => Arbitrary (a :~~: b) where
    arbitrary = arbitraryBoundedEnum
