{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TemplateHaskell, TypeOperators #-}
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
    ) where

import Control.Applicative ((<$>), (<*>), pure)
import GHC.Show (appPrec, appPrec1)
import Test.QuickCheck
import Text.Show.Text.TH (deriveShow)

data Nullary = Nullary deriving Show
$(deriveShow ''Nullary)
instance Arbitrary Nullary where
    arbitrary = pure Nullary

data PhantomNullary a = PhantomNullary deriving Show
$(deriveShow ''PhantomNullary)
instance Arbitrary (PhantomNullary a) where
    arbitrary = pure PhantomNullary

data MonomorphicUnary = MonomorphicUnary Int deriving Show
$(deriveShow ''MonomorphicUnary)
instance Arbitrary MonomorphicUnary where
    arbitrary = MonomorphicUnary <$> arbitrary

data PolymorphicUnary a b = PolymorphicUnary a deriving Show
$(deriveShow ''PolymorphicUnary)
instance Arbitrary a => Arbitrary (PolymorphicUnary a b) where
    arbitrary = PolymorphicUnary <$> arbitrary

newtype MonomorphicNewtype = MonomorphicNewtype Int deriving (Arbitrary, Show)
$(deriveShow ''MonomorphicNewtype)

newtype PolymorphicNewtype a b = PolymorphicNewtype a deriving (Arbitrary, Show)
$(deriveShow ''PolymorphicNewtype)

data MonomorphicProduct = MonomorphicProduct Char Double Int deriving Show
$(deriveShow ''MonomorphicProduct)
instance Arbitrary MonomorphicProduct where
    arbitrary = MonomorphicProduct <$> arbitrary <*> arbitrary <*> arbitrary

data PolymorphicProduct a b c d = PolymorphicProduct a b c deriving Show
$(deriveShow ''PolymorphicProduct)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (PolymorphicProduct a b c d) where
    arbitrary = PolymorphicProduct <$> arbitrary <*> arbitrary <*> arbitrary

data MonomorphicRecord = MonomorphicRecord {
    monomorphicRecord1 :: Char
  , monomorphicRecord2 :: Double
  , monomorphicRecord3 :: Int
} deriving Show
$(deriveShow ''MonomorphicRecord)
instance Arbitrary MonomorphicRecord where
    arbitrary = MonomorphicRecord <$> arbitrary <*> arbitrary <*> arbitrary

data PolymorphicRecord a b c d = PolymorphicRecord {
    polymorphicRecord1 :: a
  , polymorphicRecord2 :: b
  , polymorphicRecord3 :: c
} deriving Show
$(deriveShow ''PolymorphicRecord)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (PolymorphicRecord a b c d) where
    arbitrary = PolymorphicRecord <$> arbitrary <*> arbitrary <*> arbitrary

infix 7 :/:
data MonomorphicInfix = Int :/: Double deriving Show
$(deriveShow ''MonomorphicInfix)
instance Arbitrary MonomorphicInfix where
    arbitrary = (:/:) <$> arbitrary <*> arbitrary

infix 8 :\:
data PolymorphicInfix a b c = a :\: b deriving Show
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
  deriving Show
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
instance Show a => Show (GADT a b) where
    showsPrec _ GADTCon1     = showString "GADTCon1"
    showsPrec p (GADTCon2 d)
        = showParen (p > appPrec) $ showString "GADTCon2 " . showsPrec appPrec1 d
    showsPrec p (GADTCon3 i)
        = showParen (p > appPrec) $ showString "GADTCon3 " . showsPrec appPrec1 i
$(deriveShow ''GADT)

infixl 5 :<:
data LeftAssocTree a = LeftAssocLeaf a
                     | LeftAssocTree a :<: LeftAssocTree a
  deriving Show
$(deriveShow ''LeftAssocTree)
instance Arbitrary a => Arbitrary (LeftAssocTree a) where
    arbitrary = oneof [ LeftAssocLeaf <$> arbitrary
                      , (:<:) <$> arbitrary <*> arbitrary
                      ]

infixl 5 :>:
data RightAssocTree a = RightAssocLeaf a
                      | RightAssocTree a :>: RightAssocTree a
  deriving Show
$(deriveShow ''RightAssocTree)
instance Arbitrary a => Arbitrary (RightAssocTree a) where
    arbitrary = oneof [ RightAssocLeaf <$> arbitrary
                      , (:>:) <$> arbitrary <*> arbitrary
                      ]

infix 4 :?:
data a :?: b = a :?: b deriving Show
$(deriveShow ''(:?:))
instance (Arbitrary a, Arbitrary b) => Arbitrary (a :?: b) where
    arbitrary = (:?:) <$> arbitrary <*> arbitrary

-- TODO: Test data family instances, once they're supported
-- 
-- data family DataFamily a b c :: *
-- data instance DataFamily [a] [b] c = DataInstance1 a
--                                    | DataInstance2 [b]
--   deriving Show
-- newtype instance DataFamily Int Int c = NewtypeInstance Int deriving Show