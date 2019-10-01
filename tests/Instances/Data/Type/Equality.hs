{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

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

#if MIN_VERSION_base(4,9,0)
instance a ~~ b => Arbitrary (a :~~: b) where
    arbitrary = arbitraryBoundedEnum
#endif
