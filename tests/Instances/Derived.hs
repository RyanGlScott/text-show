{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
#include "overlap.h"
__LANGUAGE_OVERLAPPING_INSTANCES__
#if __GLASGOW_HASKELL__ >= 706
-- GHC 7.4 also supports PolyKinds, but Template Haskell doesn't seem to play
-- nicely with it for some reason.
{-# LANGUAGE PolyKinds                  #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans               #-}
#if __GLASGOW_HASKELL__ >= 711
-- The TH deriving mechanism isn't smart enough at the moment to eliminate
-- redundant constraints in Show instance declarations.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
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
module Instances.Derived (
      showbNullary
    , showbPhantomNullary
    , showbMonomorphicUnaryPrec
    , showbPolymorphicUnaryPrec
    , showbMonomorphicProductPrec
    , showbPolymorphicProductPrec
    , showbMonomorphicRecordPrec
    , showbPolymorphicRecordPrec
    , showbMonomorphicInfixPrec
    , showbPolymorphicInfixPrec
    , showbMonomorphicForallPrec
    , showbPolymorphicForallPrec
    , showbAllAtOncePrec
    , showbGADTPrec
    , showbPrimADTPrec#
    , showbLeftAssocTreePrec
    , showbRightAssocTreePrec
    , showbQuestionMarkPrec
--     , showbHigherKindedTypeParamsPrec
--     , showbRestrictionPrec
--     , showbRestrictedContextPrec
--     , showbFixPrec
#if MIN_VERSION_template_haskell(2,7,0)
    , showbASNullary
    , showbASUnaryPrec
    , showbASProductPrec
    , showbASRecordPrec
    , showbASInfixPrec
    , showbNASShowPrec
    , showbOneDataInstancePrec
    , showbAssocData1Prec
    , showbAssocData2Prec
    -- , showbAssocData3Prec
# if __GLASGOW_HASKELL__ >= 708
    , showbNullaryDataPrec
# endif
    , showbGADTFamPrec
#endif
    ) where

import Derived

import GHC.Exts (Char(..), Double(..), Float(..), Int(..), Word(..))

import Prelude ()
import Prelude.Compat hiding (Show)

import Test.Tasty.QuickCheck (Arbitrary(..), Gen, oneof)

import Text.Show.Text (Show(showb, showbPrec), Builder)
import Text.Show.Text.TH (deriveShow, mkShowbPrec)

showbNullary :: Nullary -> Builder
showbNullary = showb

showbPhantomNullary :: Show a => PhantomNullary a -> Builder
showbPhantomNullary = showb

showbMonomorphicUnaryPrec :: Int -> MonomorphicUnary -> Builder
showbMonomorphicUnaryPrec = showbPrec

showbPolymorphicUnaryPrec :: (Show a, Show b) => Int -> PolymorphicUnary a b -> Builder
showbPolymorphicUnaryPrec = showbPrec

showbMonomorphicProductPrec :: Int -> MonomorphicProduct -> Builder
showbMonomorphicProductPrec = showbPrec

showbPolymorphicProductPrec :: (Show a, Show b, Show c, Show d)
                            => Int -> PolymorphicProduct a b c d -> Builder
showbPolymorphicProductPrec = showbPrec

showbMonomorphicRecordPrec :: Int -> MonomorphicRecord -> Builder
showbMonomorphicRecordPrec = showbPrec

showbPolymorphicRecordPrec :: (Show a, Show b, Show c, Show d)
                           => Int -> PolymorphicRecord a b c d -> Builder
showbPolymorphicRecordPrec = showbPrec

showbMonomorphicInfixPrec :: Int -> MonomorphicInfix -> Builder
showbMonomorphicInfixPrec = showbPrec

showbPolymorphicInfixPrec :: (Show a, Show b, Show c)
                          => Int -> PolymorphicInfix a b c -> Builder
showbPolymorphicInfixPrec = showbPrec

showbMonomorphicForallPrec :: Int -> MonomorphicForall -> Builder
showbMonomorphicForallPrec = showbPrec

showbPolymorphicForallPrec :: (Show a, Show b) => Int -> PolymorphicForall a b -> Builder
showbPolymorphicForallPrec = showbPrec

showbAllAtOncePrec :: (Show a, Show b, Show c, Show d)
                   => Int -> AllAtOnce a b c d -> Builder
showbAllAtOncePrec = showbPrec

showbGADTPrec :: (Show a, Show b, Show c) => Int -> GADT a b c -> Builder
showbGADTPrec = showbPrec

showbPrimADTPrec# :: Show a => Int -> PrimADT# a -> Builder
showbPrimADTPrec# = showbPrec

showbLeftAssocTreePrec :: Show a => Int -> LeftAssocTree a -> Builder
showbLeftAssocTreePrec = showbPrec

showbRightAssocTreePrec :: Show a => Int -> RightAssocTree a -> Builder
showbRightAssocTreePrec = showbPrec

showbQuestionMarkPrec :: (Show a, Show b) => Int -> a :?: b -> Builder
showbQuestionMarkPrec = showbPrec

-- showbHigherKindedTypeParamsPrec :: Show (f a)
--                                 => Int -> HigherKindedTypeParams f a -> Builder
-- showbHigherKindedTypeParamsPrec = showbPrec
-- 
-- showbRestrictionPrec :: (Read a, Show a) => Int -> Restriction a -> Builder
-- showbRestrictionPrec = showbPrec
-- 
-- showbRestrictedContextPrec :: (Read a, Show a) => Int -> RestrictedContext a -> Builder
-- showbRestrictedContextPrec = showbPrec
-- 
-- showbFixPrec :: Show (f (Fix f)) => Int -> Fix f -> Builder
-- showbFixPrec = showbPrec

#if MIN_VERSION_template_haskell(2,7,0)
showbASNullary :: (Show c, Show d) => AllShow () () c d -> Builder
showbASNullary = showb

showbASUnaryPrec :: (Show b, Show c, Show d) => Int -> AllShow Int b c d -> Builder
showbASUnaryPrec = showbPrec

showbASProductPrec :: (Show c, Show d) => Int -> AllShow Bool Bool c d -> Builder
showbASProductPrec = showbPrec

showbASRecordPrec :: (Show c, Show d) => Int -> AllShow Char Double c d -> Builder
showbASRecordPrec = showbPrec

showbASInfixPrec :: (Show c, Show d) => Int -> AllShow Float Ordering c d -> Builder
showbASInfixPrec = showbPrec

showbNASShowPrec :: (Show b, Show d) => Int -> NotAllShow Int b Int d -> Builder
showbNASShowPrec = showbPrec

showbOneDataInstancePrec :: (Show a, Show b, Show c, Show d)
                         => Int -> OneDataInstance a b c d -> Builder
showbOneDataInstancePrec = showbPrec

showbAssocData1Prec :: Int -> AssocData1 () -> Builder
showbAssocData1Prec = showbPrec

showbAssocData2Prec :: Int -> AssocData2 () Int Int -> Builder
showbAssocData2Prec = showbPrec

-- showbAssocData3Prec :: Int -> AssocData3 () b c -> Builder
-- showbAssocData3Prec = showbPrec

# if __GLASGOW_HASKELL__ >= 708
showbNullaryDataPrec :: Int -> NullaryData -> Builder
showbNullaryDataPrec = showbPrec
# endif

showbGADTFamPrec :: (Show a, Show b, Show c) => Int -> GADTFam a b c -> Builder
showbGADTFamPrec = showbPrec
#endif

-------------------------------------------------------------------------------

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
instance __OVERLAPPING__  Arbitrary (GADT Char b c) where
    arbitrary = pure GADTCon1
instance __OVERLAPPING__  Arbitrary (GADT Double Double c) where
    arbitrary = GADTCon2 <$> arbitrary
instance __OVERLAPPING__  Arbitrary (GADT Int String c) where
    arbitrary = GADTCon3 <$> arbitrary
instance __OVERLAPPABLE__ Arbitrary a => Arbitrary (GADT a b c) where
    arbitrary = GADTCon4 <$> arbitrary
instance __OVERLAPPING__  Arbitrary b => Arbitrary (GADT b b c) where
    arbitrary = GADTCon5 <$> arbitrary

$(deriveShow ''PrimADT#)
instance Arbitrary (PrimADT# a) where
    arbitrary = do
        i@(I# i#) <- arbitrary
        F# f#     <- arbitrary
        D# d#     <- arbitrary
        C# c#     <- arbitrary
        W# w#     <- arbitrary
        oneof $ map pure [ PrimNormal# i# f# d# c# w#
                         , PrimRecord# i# f# d# c# w#
                         , i# `PrimInfixIntegral#` w#
                         , f# `PrimInfixFloating#` d#
                         , c# `PrimInfixChar#` c#
                         , PrimForall# i i# f# d# c# w#
                         ]

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
instance Arbitrary b => Arbitrary (NotAllShow Int b Int d) where
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
deriving instance Arbitrary (AssocData2 () Int Int)

#if __GLASGOW_HASKELL__ >= 708
$(deriveShow 'NullaryCon)
deriving instance Arbitrary NullaryData
#endif

$(deriveShow 'GADTFamCon1)
instance __OVERLAPPING__  Arbitrary (GADTFam Char b c) where
    arbitrary = pure GADTFamCon1
instance __OVERLAPPING__  Arbitrary (GADTFam Double Double c) where
    arbitrary = GADTFamCon2 <$> arbitrary
instance __OVERLAPPING__  Arbitrary (GADTFam Int String c) where
    arbitrary = GADTFamCon3 <$> arbitrary
instance __OVERLAPPABLE__ Arbitrary a => Arbitrary (GADTFam a b c) where
    arbitrary = GADTFamCon4 <$> arbitrary
instance __OVERLAPPING__  Arbitrary b => Arbitrary (GADTFam b b c) where
    arbitrary = GADTFamCon5 <$> arbitrary
#endif
