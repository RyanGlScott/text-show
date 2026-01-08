{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

#if !MIN_VERSION_QuickCheck(2,17,0)
import Data.Char (GeneralCategory)
import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

instance Arbitrary GeneralCategory where
    arbitrary = arbitraryBoundedEnum
#endif
