{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Numeric.Natural
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

'Arbitrary' instance 'Natural' (if one isn't already defined).
-}
module Instances.Numeric.Natural () where

#if !(MIN_VERSION_QuickCheck(2,8,0)) || !(MIN_VERSION_base(4,8,0))
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary(..), arbitrarySizedNatural, shrinkIntegral)

instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural
    shrink    = shrinkIntegral
#endif
