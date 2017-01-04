{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Char
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'GeneralCategory'.
-}
module Instances.Data.Char () where

import Data.Char (GeneralCategory)
import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

instance Arbitrary GeneralCategory where
    arbitrary = arbitraryBoundedEnum
