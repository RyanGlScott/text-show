{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|
Module:      Instances.Utils.GenericArbitrary
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

A generic default implemention of 'arbitrary'.

Ideally, this should be a part of @QuickCheck@ itself
(see https://github.com/nick8325/quickcheck/pull/40), but alas, it hasn't been
merged yet. Until then, we'll have to define it ourselves.
-}
module Instances.Utils.GenericArbitrary (genericArbitrary) where

import Generics.Deriving.Base

import GHC.Exts (Char(..), Double(..), Float(..), Int(..), Word(..))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), Gen, choose)

-- | `Gen` for generic instances in which each constructor has equal probability
-- of being chosen.
genericArbitrary :: (Generic a, GArbitrary (Rep a)) => Gen a
genericArbitrary = to <$> gArbitrary

class GArbitrary f where
  gArbitrary :: Gen (f a)

instance GArbitrary V1 where
  -- Following the `Encode' V1` example in GHC.Generics.
  gArbitrary = undefined

instance GArbitrary U1 where
  gArbitrary = return U1

instance (GArbitrary a, GArbitrary b) => GArbitrary (a :*: b) where
  gArbitrary = (:*:) <$> gArbitrary <*> gArbitrary

instance ( SumSize    a, SumSize    b
         , ChooseSum  a, ChooseSum  b ) => GArbitrary (a :+: b) where
  gArbitrary = do
    -- We cannot simply choose with equal probability between the left and
    -- right part of the `a :+: b` (e.g. with `choose (False, True)`),
    -- because GHC.Generics does not guarantee :+: to be balanced; even if it
    -- did, it could only do so for sum types with 2^n alternatives.
    -- If we did that and got a data structure of form `(a :+: (b :+: c))`,
    -- then a would be chosen just as often as b and c together.
    -- So we first have to compute the number of alternatives using `sumSize`,
    -- and then uniformly sample a number in the corresponding range.
    let size = unTagged2 (sumSize :: Tagged2 (a :+: b) Int)
    x <- choose (1, size)
    -- Optimisation:
    -- We could just recursively call `gArbitrary` on the left orright branch
    -- here, as in
    --   if x <= sizeL
    --     then L1 <$> gArbitrary
    --     else R1 <$> gArbitrary
    -- but this would unnecessarily sample again in the same sum type, and that
    -- even though `x` completely determines which alternative to choose,
    -- and sampling is slow because it needs IO and random numbers.
    -- So instead we use `chooseSum x` to pick the x'th alternative from the
    -- current sum type.
    -- This made it around 50% faster for a sum type with 26 alternatives
    -- on my computer.
    chooseSum x

instance GArbitrary a => GArbitrary (M1 i c a) where
  gArbitrary = M1 <$> gArbitrary

instance Arbitrary a => GArbitrary (K1 i a) where
  gArbitrary = K1 <$> arbitrary

instance GArbitrary UChar where
  gArbitrary = do
    C# c <- arbitrary
    return (UChar c)

instance GArbitrary UDouble where
  gArbitrary = do
    D# d <- arbitrary
    return (UDouble d)

instance GArbitrary UFloat where
  gArbitrary = do
    F# f <- arbitrary
    return (UFloat f)

instance GArbitrary UInt where
  gArbitrary = do
    I# i <- arbitrary
    return (UInt i)

instance GArbitrary UWord where
  gArbitrary = do
    W# w <- arbitrary
    return (UWord w)

newtype Tagged2 (s :: * -> *) b = Tagged2 {unTagged2 :: b}

-- | Calculates the size of a sum type (numbers of alternatives).
--
-- Example: `data X = A | B | C` has `sumSize` 3.
class SumSize f where
  sumSize :: Tagged2 f Int

-- Recursive case: Sum split `(:+:)`..
instance (SumSize a, SumSize b) => SumSize (a :+: b) where
  sumSize = Tagged2 $ unTagged2 (sumSize :: Tagged2 a Int) +
                          unTagged2 (sumSize :: Tagged2 b Int)
  {-# INLINE sumSize #-}

-- Constructor base case.
instance SumSize (C1 s a) where
  sumSize = Tagged2 1
  {-# INLINE sumSize #-}

-- | This class takes an integer `x` and returns a `gArbitrary` value
-- for the `x`'th alternative in a sum type.
class ChooseSum f where
  chooseSum :: Int -> Gen (f a)

-- Recursive case: Check whether `x` lies in the left or the right side
-- of the (:+:) split.
instance (SumSize a, ChooseSum a, ChooseSum b) => ChooseSum (a :+: b) where
  chooseSum x = do
    let sizeL = unTagged2 (sumSize :: Tagged2 a Int)
    if x <= sizeL
      then L1 <$> chooseSum x
      else R1 <$> chooseSum (x - sizeL)

-- Constructor base case.
instance (GArbitrary a) => ChooseSum (C1 s a) where
  chooseSum 1 = gArbitrary
  chooseSum _ = error "chooseSum: BUG"
