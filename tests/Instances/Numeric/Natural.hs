{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Numeric.Natural
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance 'Natural' (if one isn't already defined).
-}
module Instances.Numeric.Natural () where

-- Copied from @QuickCheck@

#if !(MIN_VERSION_QuickCheck(2,8,0)) || !(MIN_VERSION_base(4,8,0))
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary(..), Gen,
                        choose, shrinkIntegral, sized, suchThat)

instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural
    shrink    = shrinkIntegral

-- | Generates a natural number. The number's maximum value depends on
-- the size parameter.
arbitrarySizedNatural :: Integral a => Gen a
arbitrarySizedNatural = sized $ \n ->
    inBounds fromInteger (choose (0, toInteger n))

inBounds :: Integral a => (Integer -> a) -> Gen Integer -> Gen a
inBounds fi g = fmap fi (g `suchThat` (\x -> toInteger (fi x) == x))
#endif
