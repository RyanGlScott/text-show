{-# LANGUAGE CPP, ExistentialQuantification, FlexibleContexts,
             GADTs, GeneralizedNewtypeDeriving, StandaloneDeriving,
             TypeOperators, UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 706
-- GHC 7.4 also supports PolyKinds, but Template Haskell doesn't seem to play
-- nicely with it for some reason.
{-# LANGUAGE PolyKinds #-}
#endif
{-|
Module:      Derived
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Defines data types with derived 'Show' instances (using "Text.Show.Text.TH")
for testing purposes, including 'Arbitrary' instances.
-}
module Derived (
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
    , MonomorphicForall(..)
    , PolymorphicForall(..)
    , AllAtOnce(..)
    , GADT(..)
    , LeftAssocTree(..)
    , RightAssocTree(..)
    , (:?:)(..)
    , HigherKindedTypeParams(..)
    , Restriction(..)
    , RestrictedContext(..)
    , Fix(..)
    ) where

import           Prelude hiding (Show)

import           Test.Tasty.QuickCheck (Arbitrary)

import qualified Text.Show as S (Show)
import qualified Text.Show.Text as T (Show)

data Nullary = Nullary deriving S.Show

data PhantomNullary a = PhantomNullary deriving S.Show

data MonomorphicUnary = MonomorphicUnary Int deriving S.Show

data PolymorphicUnary a b = PolymorphicUnary a deriving S.Show

newtype MonomorphicNewtype = MonomorphicNewtype Int deriving (Arbitrary, S.Show)

newtype PolymorphicNewtype a b = PolymorphicNewtype a deriving (Arbitrary, S.Show)

data MonomorphicProduct = MonomorphicProduct Char Double Int deriving S.Show

data PolymorphicProduct a b c d = PolymorphicProduct a b c deriving S.Show

data MonomorphicRecord = MonomorphicRecord {
    monomorphicRecord1 :: Char
  , monomorphicRecord2 :: Double
  , monomorphicRecord3 :: Int
} deriving S.Show

data PolymorphicRecord a b c d = PolymorphicRecord {
    polymorphicRecord1 :: a
  , polymorphicRecord2 :: b
  , polymorphicRecord3 :: c
} deriving S.Show

infix 7 :/:
data MonomorphicInfix = Int :/: Double deriving S.Show

infix 8 :\:
data PolymorphicInfix a b c = a :\: b deriving S.Show

data MonomorphicForall = forall a. (Arbitrary a, S.Show a, T.Show a) => MonomorphicForall a
deriving instance S.Show MonomorphicForall

data PolymorphicForall a b = forall c. (Arbitrary c, S.Show c, T.Show c) => PolymorphicForall a c
deriving instance S.Show a => S.Show (PolymorphicForall a b)

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
                       | forall e. (Arbitrary e, S.Show e, T.Show e) => AAOForall a c e
deriving instance (S.Show a, S.Show b, S.Show c) => S.Show (AllAtOnce a b c d)

data GADT a b c where
    GADTCon1 ::           GADT Char   b      c
    GADTCon2 :: Double -> GADT Double Double c
    GADTCon3 :: Int    -> GADT Int    String c
    GADTCon4 :: a      -> GADT a      b      c
    GADTCon5 :: b      -> GADT b      b      c
deriving instance (S.Show a, S.Show b) => S.Show (GADT a b c)

infixl 5 :<:
data LeftAssocTree a = LeftAssocLeaf a
                     | LeftAssocTree a :<: LeftAssocTree a
  deriving S.Show

infixl 5 :>:
data RightAssocTree a = RightAssocLeaf a
                      | RightAssocTree a :>: RightAssocTree a
  deriving S.Show

infix 4 :?:
data a :?: b = a :?: b deriving S.Show

data HigherKindedTypeParams f a = HigherKindedTypeParams (f a) deriving S.Show

data Restriction a = Restriction a
deriving instance (Read a, S.Show a) => S.Show (Restriction a)

data RestrictedContext a = RestrictedContext (Restriction a) deriving S.Show

newtype Fix f = Fix (f (Fix f))
deriving instance S.Show (f (Fix f)) => S.Show (Fix f)

-- TODO: Test data family instances, once they're supported
-- 
-- data family DataFamily a b c :: *
-- data instance DataFamily [a] [b] c = DataInstance1 a
--                                    | DataInstance2 [b]
--   deriving Show
-- newtype instance DataFamily Int Int c = NewtypeInstance Int deriving Show 
