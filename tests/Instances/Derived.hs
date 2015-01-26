{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, GADTs,
             GeneralizedNewtypeDeriving, OverlappingInstances, StandaloneDeriving,
             TemplateHaskell, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.Derived
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Defines data types with derived 'Show' instances (using "Text.Show.Text.TH")
for testing purposes, including 'Arbitrary' instances.
-}
module Instances.Derived () where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<*>), pure)
#endif

import Data.Functor ((<$>))

import Derived

import Prelude hiding (Show)

import Test.Tasty.QuickCheck (Arbitrary(..), Gen, oneof)

import Text.Show.Text (Show(showbPrec))
import Text.Show.Text.TH (deriveShow, mkShowbPrec)

$(deriveShow ''Nullary)
instance Arbitrary Nullary where
    arbitrary = pure Nullary

$(deriveShow ''PhantomNullary)
instance Arbitrary (PhantomNullary a) where
    arbitrary = pure PhantomNullary

$(deriveShow ''MonomorphicUnary)
deriving instance Arbitrary MonomorphicUnary

$(deriveShow ''PolymorphicUnary)
deriving instance Arbitrary a => Arbitrary (PolymorphicUnary a b)

$(deriveShow ''MonomorphicProduct)
instance Arbitrary MonomorphicProduct where
    arbitrary = MonomorphicProduct <$> arbitrary <*> arbitrary <*> arbitrary

$(deriveShow ''PolymorphicProduct)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (PolymorphicProduct a b c d) where
    arbitrary = PolymorphicProduct <$> arbitrary <*> arbitrary <*> arbitrary

$(deriveShow ''MonomorphicRecord)
instance Arbitrary MonomorphicRecord where
    arbitrary = MonomorphicRecord <$> arbitrary <*> arbitrary <*> arbitrary

$(deriveShow ''PolymorphicRecord)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (PolymorphicRecord a b c d) where
    arbitrary = PolymorphicRecord <$> arbitrary <*> arbitrary <*> arbitrary

$(deriveShow ''MonomorphicInfix)
instance Arbitrary MonomorphicInfix where
    arbitrary = (:/:) <$> arbitrary <*> arbitrary

$(deriveShow ''PolymorphicInfix)
instance (Arbitrary a, Arbitrary b) => Arbitrary (PolymorphicInfix a b c) where
    arbitrary = PolyInf <$> arbitrary <*> arbitrary

$(deriveShow ''MonomorphicForall)
instance Arbitrary MonomorphicForall where
    arbitrary = MonomorphicForall <$> (arbitrary :: Gen Int)

$(deriveShow ''PolymorphicForall)
instance Arbitrary a => Arbitrary (PolymorphicForall a b) where
    arbitrary = PolymorphicForall <$> arbitrary <*> (arbitrary :: Gen Int)

$(deriveShow ''AllAtOnce)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (AllAtOnce a b c d) where
    arbitrary = oneof [ pure AAONullary
                      , AAOUnary   <$> arbitrary
                      , AAOProduct <$> arbitrary <*> arbitrary <*> arbitrary
                      , AAORecord  <$> arbitrary <*> arbitrary <*> arbitrary
                      , (:/\:)     <$> arbitrary <*> arbitrary
                      , AAOForall  <$> arbitrary <*> arbitrary <*> (arbitrary :: Gen Int)
                      ]

$(deriveShow ''GADT)
instance Arbitrary (GADT Char b c) where
    arbitrary = pure GADTCon1
instance Arbitrary (GADT Double Double c) where
    arbitrary = GADTCon2 <$> arbitrary
instance Arbitrary (GADT Int String c) where
    arbitrary = GADTCon3 <$> arbitrary
instance Arbitrary a => Arbitrary (GADT a b c) where
    arbitrary = GADTCon4 <$> arbitrary
instance Arbitrary b => Arbitrary (GADT b b c) where
    arbitrary = GADTCon5 <$> arbitrary

$(deriveShow ''LeftAssocTree)
instance Arbitrary a => Arbitrary (LeftAssocTree a) where
    arbitrary = oneof [ LeftAssocLeaf <$> arbitrary
                      , (:<:) <$> (LeftAssocLeaf <$> arbitrary)
                              <*> (LeftAssocLeaf <$> arbitrary)
                      ]

$(deriveShow ''RightAssocTree)
instance Arbitrary a => Arbitrary (RightAssocTree a) where
    arbitrary = oneof [ RightAssocLeaf <$> arbitrary
                      , (:>:) <$> (RightAssocLeaf <$> arbitrary)
                              <*> (RightAssocLeaf <$> arbitrary)
                      ]

$(deriveShow ''(:?:))
instance (Arbitrary a, Arbitrary b) => Arbitrary (a :?: b) where
    arbitrary = (:?:) <$> arbitrary <*> arbitrary

instance Show (f a) => Show (HigherKindedTypeParams f a) where
    showbPrec = $(mkShowbPrec ''HigherKindedTypeParams)
deriving instance Arbitrary (f a) => Arbitrary (HigherKindedTypeParams f a)

instance (Read a, Show a) => Show (Restriction a) where
    showbPrec = $(mkShowbPrec ''Restriction)
deriving instance Arbitrary a => Arbitrary (Restriction a)

instance (Read a, Show a) => Show (RestrictedContext a) where
    showbPrec = $(mkShowbPrec ''RestrictedContext)
deriving instance Arbitrary a => Arbitrary (RestrictedContext a)

instance Show (f (Fix f)) => Show (Fix f) where
    showbPrec = $(mkShowbPrec ''Fix)
deriving instance Arbitrary (f (Fix f)) => Arbitrary (Fix f)

#if MIN_VERSION_template_haskell(2,7,0)
$(deriveShow ''AllShow)
instance Arbitrary (AllShow () () c d) where
    arbitrary = pure ASNullary
deriving instance Arbitrary (AllShow Int b c d)
instance Arbitrary (AllShow Bool Bool c d) where
    arbitrary = ASProduct <$> arbitrary <*> arbitrary
instance Arbitrary c => Arbitrary (AllShow Char Double c d) where
    arbitrary = ASRecord <$> arbitrary <*> arbitrary <*> arbitrary
instance Arbitrary (AllShow Float Ordering c d) where
    arbitrary = ASInfix <$> arbitrary <*> arbitrary

$(deriveShow 'NASShow1)
instance Arbitrary c => Arbitrary (NotAllShow Int Int c d) where
    arbitrary = oneof [NASShow1 <$> arbitrary <*> arbitrary, NASShow2 <$> arbitrary]

$(deriveShow ''OneDataInstance)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (OneDataInstance a b c d) where
    arbitrary = oneof [ pure ODINullary
                      , ODIUnary   <$> arbitrary
                      , ODIProduct <$> arbitrary <*> arbitrary
                      , ODIRecord  <$> arbitrary <*> arbitrary <*> arbitrary
                      , ODIInfix   <$> arbitrary <*> arbitrary
                      ]

$(deriveShow ''AssocData1)
deriving instance Arbitrary (AssocData1 ())

$(deriveShow 'AssocCon2)
deriving instance Arbitrary (AssocData2 () b c)

#if __GLASGOW_HASKELL__ >= 708
$(deriveShow 'NullaryCon)
deriving instance Arbitrary NullaryData
#endif

$(deriveShow 'GADTFamCon1)
instance Arbitrary (GADTFam Char b c) where
    arbitrary = pure GADTFamCon1
instance Arbitrary (GADTFam Double Double c) where
    arbitrary = GADTFamCon2 <$> arbitrary
instance Arbitrary (GADTFam Int String c) where
    arbitrary = GADTFamCon3 <$> arbitrary
instance Arbitrary a => Arbitrary (GADTFam a b c) where
    arbitrary = GADTFamCon4 <$> arbitrary
instance Arbitrary b => Arbitrary (GADTFam b b c) where
    arbitrary = GADTFamCon5 <$> arbitrary
#endif