{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Tuple
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for tuple types.
-}
module Instances.Data.Tuple () where

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d
         , Arbitrary e
         , Arbitrary f
         , Arbitrary g
         , Arbitrary h
         , Arbitrary i
         , Arbitrary j
         , Arbitrary k
         ) => Arbitrary (a, b, c, d, e, f, g, h, i, j, k) where
    arbitrary = (,,,,,,,,,,)
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d
         , Arbitrary e
         , Arbitrary f
         , Arbitrary g
         , Arbitrary h
         , Arbitrary i
         , Arbitrary j
         , Arbitrary k
         , Arbitrary l
         ) => Arbitrary (a, b, c, d, e, f, g, h, i, j, k, l) where
    arbitrary = (,,,,,,,,,,,)
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d
         , Arbitrary e
         , Arbitrary f
         , Arbitrary g
         , Arbitrary h
         , Arbitrary i
         , Arbitrary j
         , Arbitrary k
         , Arbitrary l
         , Arbitrary m
         ) => Arbitrary (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    arbitrary = (,,,,,,,,,,,,)
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d
         , Arbitrary e
         , Arbitrary f
         , Arbitrary g
         , Arbitrary h
         , Arbitrary i
         , Arbitrary j
         , Arbitrary k
         , Arbitrary l
         , Arbitrary m
         , Arbitrary n
         ) => Arbitrary (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    arbitrary = (,,,,,,,,,,,,,)
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d
         , Arbitrary e
         , Arbitrary f
         , Arbitrary g
         , Arbitrary h
         , Arbitrary i
         , Arbitrary j
         , Arbitrary k
         , Arbitrary l
         , Arbitrary m
         , Arbitrary n
         , Arbitrary o
         ) => Arbitrary (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    arbitrary = (,,,,,,,,,,,,,,)
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
