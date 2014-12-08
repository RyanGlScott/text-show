{-# LANGUAGE FlexibleContexts, GADTs, GeneralizedNewtypeDeriving,
             NoImplicitPrelude, TemplateHaskell, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Instances.Derived
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- This module defines data types that have derived 'Show' instances (using
-- "Text.Show.Text.TH") for testing purposes, including 'Arbitrary'
-- instances.
----------------------------------------------------------------------------
module Instances.Derived (
      Nullary(..)
    , PhantomNullary(..)
    , MonomorphicUnary(..)
    , PolymorphicUnary(..)
    , MonomorphicNewtype(..)
    , PolymorphicNewtype(..)
    , MonomorphicProduct(..)
    , PolymorphicProduct(..)
    , MonomorphicRecord(..)
    , PolymorphicRecord(..)
    , MonomorphicInfix(..)
    , PolymorphicInfix(..)
    , AllAtOnce(..)
    , GADT(..)
    , LeftAssocTree(..)
    , RightAssocTree(..)
    , (:?:)(..)
    , HigherKindedTypeParams(..)
    , RestrictedContext(..)
    ) where

import           Control.Applicative ((<$>), (<*>), pure)

import           GHC.Show (appPrec, appPrec1)

import           Prelude hiding (Show)

import           Test.Tasty.QuickCheck (Arbitrary(arbitrary), oneof)

import qualified Text.Show as S (Show)
import qualified Text.Show.Text as T (Show)
import           Text.Show.Text (showbPrec)
import           Text.Show.Text.TH (deriveShow, mkShowbPrec)

data Nullary = Nullary deriving S.Show
$(deriveShow ''Nullary)
instance Arbitrary Nullary where
    arbitrary = pure Nullary

data PhantomNullary a = PhantomNullary deriving S.Show
$(deriveShow ''PhantomNullary)
instance Arbitrary (PhantomNullary a) where
    arbitrary = pure PhantomNullary

data MonomorphicUnary = MonomorphicUnary Int deriving S.Show
$(deriveShow ''MonomorphicUnary)
instance Arbitrary MonomorphicUnary where
    arbitrary = MonomorphicUnary <$> arbitrary

data PolymorphicUnary a b = PolymorphicUnary a deriving S.Show
$(deriveShow ''PolymorphicUnary)
instance Arbitrary a => Arbitrary (PolymorphicUnary a b) where
    arbitrary = PolymorphicUnary <$> arbitrary

newtype MonomorphicNewtype = MonomorphicNewtype Int deriving (Arbitrary, S.Show)
$(deriveShow ''MonomorphicNewtype)

newtype PolymorphicNewtype a b = PolymorphicNewtype a deriving (Arbitrary, S.Show)
$(deriveShow ''PolymorphicNewtype)

data MonomorphicProduct = MonomorphicProduct Char Double Int deriving S.Show
$(deriveShow ''MonomorphicProduct)
instance Arbitrary MonomorphicProduct where
    arbitrary = MonomorphicProduct <$> arbitrary <*> arbitrary <*> arbitrary

data PolymorphicProduct a b c d = PolymorphicProduct a b c deriving S.Show
$(deriveShow ''PolymorphicProduct)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (PolymorphicProduct a b c d) where
    arbitrary = PolymorphicProduct <$> arbitrary <*> arbitrary <*> arbitrary

data MonomorphicRecord = MonomorphicRecord {
    monomorphicRecord1 :: Char
  , monomorphicRecord2 :: Double
  , monomorphicRecord3 :: Int
} deriving S.Show
$(deriveShow ''MonomorphicRecord)
instance Arbitrary MonomorphicRecord where
    arbitrary = MonomorphicRecord <$> arbitrary <*> arbitrary <*> arbitrary

data PolymorphicRecord a b c d = PolymorphicRecord {
    polymorphicRecord1 :: a
  , polymorphicRecord2 :: b
  , polymorphicRecord3 :: c
} deriving S.Show
$(deriveShow ''PolymorphicRecord)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (PolymorphicRecord a b c d) where
    arbitrary = PolymorphicRecord <$> arbitrary <*> arbitrary <*> arbitrary

infix 7 :/:
data MonomorphicInfix = Int :/: Double deriving S.Show
$(deriveShow ''MonomorphicInfix)
instance Arbitrary MonomorphicInfix where
    arbitrary = (:/:) <$> arbitrary <*> arbitrary

infix 8 :\:
data PolymorphicInfix a b c = a :\: b deriving S.Show
$(deriveShow ''PolymorphicInfix)
instance (Arbitrary a, Arbitrary b) => Arbitrary (PolymorphicInfix a b c) where
    arbitrary = (:\:) <$> arbitrary <*> arbitrary

infix 3 :/\:
data AllAtOnce a b c d = AAONullary
                       | AAOUnary a
                       | AAOProduct a b c
                       | AAORecord {
                           aaoRecord1 :: a
                         , aaoRecord2 :: b
                         , aaoRecord3 :: c
                       }
                       | a :/\: b
  deriving S.Show
$(deriveShow ''AllAtOnce)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (AllAtOnce a b c d) where
    arbitrary = oneof [ pure AAONullary
                      , AAOUnary <$> arbitrary
                      , AAOProduct <$> arbitrary <*> arbitrary <*> arbitrary
                      , AAORecord <$> arbitrary <*> arbitrary <*> arbitrary
                      , (:/\:) <$> arbitrary <*> arbitrary
                      ]

data GADT a b where
    GADTCon1 ::           GADT Char b
    GADTCon2 :: Double -> GADT Double Double
    GADTCon3 :: Int    -> GADT Int String
instance S.Show a => S.Show (GADT a b) where
    showsPrec _ GADTCon1     = showString "GADTCon1"
    showsPrec p (GADTCon2 d)
        = showParen (p > appPrec) $ showString "GADTCon2 " . showsPrec appPrec1 d
    showsPrec p (GADTCon3 i)
        = showParen (p > appPrec) $ showString "GADTCon3 " . showsPrec appPrec1 i
$(deriveShow ''GADT)

infixl 5 :<:
data LeftAssocTree a = LeftAssocLeaf a
                     | LeftAssocTree a :<: LeftAssocTree a
  deriving S.Show
$(deriveShow ''LeftAssocTree)
instance Arbitrary a => Arbitrary (LeftAssocTree a) where
    arbitrary = oneof [ LeftAssocLeaf <$> arbitrary
                      , (:<:) <$> arbitrary <*> arbitrary
                      ]

infixl 5 :>:
data RightAssocTree a = RightAssocLeaf a
                      | RightAssocTree a :>: RightAssocTree a
  deriving S.Show
$(deriveShow ''RightAssocTree)
instance Arbitrary a => Arbitrary (RightAssocTree a) where
    arbitrary = oneof [ RightAssocLeaf <$> arbitrary
                      , (:>:) <$> arbitrary <*> arbitrary
                      ]

infix 4 :?:
data a :?: b = a :?: b deriving S.Show
$(deriveShow ''(:?:))
instance (Arbitrary a, Arbitrary b) => Arbitrary (a :?: b) where
    arbitrary = (:?:) <$> arbitrary <*> arbitrary

data HigherKindedTypeParams f a = HigherKindedTypeParams (f a) deriving S.Show
$(return []) -- Hack to make HigherKindedTypeParams available in the type environment at the time of reification
instance T.Show (f a) => T.Show (HigherKindedTypeParams f a) where
    showbPrec = $(mkShowbPrec ''HigherKindedTypeParams)
instance Arbitrary (f a) => Arbitrary (HigherKindedTypeParams f a) where
    arbitrary = HigherKindedTypeParams <$> arbitrary

data Restriction a = Restriction a
$(return []) -- Hack to make Restriction available in the type environment at the time of reification
instance (Read a, S.Show a) => S.Show (Restriction a) where
    showsPrec p (Restriction r)
        = showParen (p > appPrec) $ showString "Restriction " . showsPrec appPrec1 r
instance (Read a, T.Show a) => T.Show (Restriction a) where
    showbPrec = $(mkShowbPrec ''Restriction)
instance Arbitrary a => Arbitrary (Restriction a) where
    arbitrary = Restriction <$> arbitrary

data RestrictedContext a = RestrictedContext (Restriction a) deriving S.Show
$(return []) -- Hack to make RestrictedContext available in the type environment at the time of reification
instance (Read a, T.Show a) => T.Show (RestrictedContext a) where
    showbPrec = $(mkShowbPrec ''RestrictedContext)
instance Arbitrary a => Arbitrary (RestrictedContext a) where
    arbitrary = RestrictedContext <$> arbitrary

-- TODO: Test data family instances, once they're supported
-- 
-- data family DataFamily a b c :: *
-- data instance DataFamily [a] [b] c = DataInstance1 a
--                                    | DataInstance2 [b]
--   deriving Show
-- newtype instance DataFamily Int Int c = NewtypeInstance Int deriving Show