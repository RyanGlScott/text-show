{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
#if MIN_VERSION_template_haskell(2,7,0)
{-# LANGUAGE TypeFamilies               #-}
#endif
#if __GLASGOW_HASKELL__ >= 706
-- GHC 7.4 also supports PolyKinds, but Template Haskell doesn't seem to play
-- nicely with it for some reason.
{-# LANGUAGE PolyKinds                  #-}
#endif
#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
-- Starting with GHC 7.10, NullaryTypeClasses was deprecated in favor of
-- MultiParamTypeClasses, which is already enabled
{-# LANGUAGE NullaryTypeClasses         #-}
#endif
{-|
Module:      Derived
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Defines data types with derived 'Show' instances (using "Text.Show.Text.TH")
for testing purposes.
-}
module Derived (
      Nullary(..)
    , PhantomNullary(..)
    , MonomorphicUnary(..)
    , PolymorphicUnary(..)
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
    , PrimADT#(..)
    , LeftAssocTree(..)
    , RightAssocTree(..)
    , (:?:)(..)
    , HigherKindedTypeParams(..)
    , Restriction(..)
    , RestrictedContext(..)
    , Fix(..)
#if MIN_VERSION_template_haskell(2,7,0)
    , AllShow(..)
    , NotAllShow(..)
    , OneDataInstance(..)
    , AssocClass1(..)
    , AssocData1(..)
    , AssocClass2(..)
    , AssocData2(..)
    -- , AssocClass3(..)
    -- , AssocData3(..)
# if __GLASGOW_HASKELL__ >= 708
    , NullaryClass(..)
    , NullaryData(..)
# endif
    , GADTFam(..)
#endif
    ) where

import           GHC.Generics (Generic)
import           GHC.Prim (Char#, Double#, Float#, Int#, Word#)

import           Prelude hiding (Show)

import           Test.Tasty.QuickCheck (Arbitrary)

import qualified Text.Show as S (Show)
import qualified Text.Show.Text as T (Show)

data Nullary = Nullary deriving (S.Show, Generic)

data PhantomNullary a = PhantomNullary deriving (S.Show, Generic)

newtype MonomorphicUnary = MonomorphicUnary Int deriving (S.Show, Generic)

newtype PolymorphicUnary a b = PolymorphicUnary a deriving (S.Show, Generic)

data MonomorphicProduct = MonomorphicProduct Char Double Int deriving (S.Show, Generic)

data PolymorphicProduct a b c d = PolymorphicProduct a b c deriving (S.Show, Generic)

data MonomorphicRecord = MonomorphicRecord {
    monomorphicRecord1 :: Char
  , monomorphicRecord2 :: Double
  , monomorphicRecord3 :: Int
} deriving (S.Show, Generic)

data PolymorphicRecord a b c d = PolymorphicRecord {
    polymorphicRecord1 :: a
  , polymorphicRecord2 :: b
  , polymorphicRecord3 :: c
} deriving (S.Show, Generic)

infix 7 :/:
data MonomorphicInfix = Int :/: Double deriving (S.Show, Generic)

infix 8 `PolyInf`
data PolymorphicInfix a b c = a `PolyInf` b deriving (S.Show, Generic)

-- TODO: Figure out how to create Generic instances for MonomorphicForall, PolymorphicForall,
-- AllAtOnce, GADT, and PrimADT

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

infixr 5 `PrimInfixIntegral#`, `PrimInfixFloating#`, `PrimInfixChar#`
data PrimADT# a = PrimNormal# Int# Float# Double# Char# Word#
                | PrimRecord# {
                    primRecordInt#    :: Int#
                  , primRecordFloat#  :: Float#
                  , primRecordDouble# :: Double#
                  , primRecordChar#   :: Char#
                  , primRecordWord#   :: Word#
                }
                | Int#   `PrimInfixIntegral#` Word#
                | Float# `PrimInfixFloating#` Double#
                | Char#  `PrimInfixChar#`     Char#
                | forall b. (Arbitrary b, S.Show b, T.Show b)
                    => PrimForall# b Int# Float# Double# Char# Word#
deriving instance S.Show (PrimADT# a)

infixl 5 :<:
data LeftAssocTree a = LeftAssocLeaf a
                     | LeftAssocTree a :<: LeftAssocTree a
  deriving (S.Show, Generic)

infixl 5 :>:
data RightAssocTree a = RightAssocLeaf a
                      | RightAssocTree a :>: RightAssocTree a
  deriving (S.Show, Generic)

infix 4 :?:
data a :?: b = a :?: b deriving (S.Show, Generic)

newtype HigherKindedTypeParams f a = HigherKindedTypeParams (f a) deriving (S.Show, Generic)

newtype Restriction a = Restriction a deriving Generic
deriving instance (Read a, S.Show a) => S.Show (Restriction a)

newtype RestrictedContext a = RestrictedContext (Restriction a) deriving (S.Show, Generic)

newtype Fix f = Fix (f (Fix f)) deriving Generic
deriving instance S.Show (f (Fix f)) => S.Show (Fix f)

#if MIN_VERSION_template_haskell(2,7,0)
infix 2 `ASInfix`
data family AllShow a b c d :: *
data instance AllShow () () c d = ASNullary
  deriving ( S.Show
# if __GLASGOW_HASKELL__ >= 706
           {- Woraround for a bizarre bug in older GHCs -}
           , Generic
# endif
           )
newtype instance AllShow Int b c d = ASUnary Int
  deriving ( S.Show
# if __GLASGOW_HASKELL__ >= 706
           {- Woraround for a bizarre bug in older GHCs -}
           , Generic
# endif
           )
data instance AllShow Bool Bool c d = ASProduct Bool Bool
  deriving ( S.Show
# if __GLASGOW_HASKELL__ >= 706
           {- Woraround for a bizarre bug in older GHCs -}
           , Generic
# endif
           )
data instance AllShow Char Double c d = ASRecord {
    asRecord1 :: Char
  , asRecord2 :: Double
  , asRecord3 :: c
  } deriving ( S.Show
# if __GLASGOW_HASKELL__ >= 706
             {- Woraround for a bizarre bug in older GHCs -}
             , Generic
# endif
             )
data instance AllShow Float Ordering c d = Float `ASInfix` Ordering
  deriving ( S.Show
# if __GLASGOW_HASKELL__ >= 706
           {- Woraround for a bizarre bug in older GHCs -}
           , Generic
# endif
           )

data family NotAllShow a b c d :: *
data instance NotAllShow ()  ()  ()  d = NASNoShow
data instance NotAllShow Int b   Int d = NASShow1 Int Int
                                       | NASShow2 b
  deriving ( S.Show
# if __GLASGOW_HASKELL__ >= 706
           {- Woraround for a bizarre bug in older GHCs -}
           , Generic
# endif
           )

infix 1 `ODIInfix`
data family OneDataInstance a b c d :: *
data instance OneDataInstance a b c d = ODINullary
                                      | ODIUnary a
                                      | ODIProduct a b
                                      | ODIRecord {
                                          odiRecord1 :: a
                                        , odiRecord2 :: b
                                        , odiRecord3 :: c
                                      }
                                      | a `ODIInfix` b
  deriving ( S.Show
# if __GLASGOW_HASKELL__ >= 706
           {- Woraround for a bizarre bug in older GHCs -}
           , Generic
# endif
           )

class AssocClass1 a where
    data AssocData1 a :: *
instance AssocClass1 () where
    newtype AssocData1 () = AssocCon1 Int deriving ( S.Show
# if __GLASGOW_HASKELL__ >= 706
                                                   {- Woraround for a bizarre bug in older GHCs -}
                                                   , Generic
# endif
                                                   )

class AssocClass2 a b c where
    data AssocData2 a b c :: *
instance AssocClass2 () Int Int where
    newtype AssocData2 () Int Int = AssocCon2 Int deriving ( S.Show
# if __GLASGOW_HASKELL__ >= 706
                                                           {- Woraround for a bizarre bug in older GHCs -}
                                                           , Generic
# endif
                                                           )

-- class AssocClass3 a b c where
--     data AssocData3 a b c :: *
-- instance AssocClass3 () b c where
--     newtype AssocData3 () b c = AssocCon3 Int deriving ( S.Show
-- # if __GLASGOW_HASKELL__ >= 706
--                                                        {- Woraround for a bizarre bug in older GHCs -}
--                                                        , Generic
-- # endif
--                                                        )

# if __GLASGOW_HASKELL__ >= 708
class NullaryClass where
    data NullaryData
instance NullaryClass where
    newtype NullaryData = NullaryCon Int deriving (S.Show, Generic)
# endif

data family GADTFam a b c :: *
data instance GADTFam a b c where
    GADTFamCon1 ::           GADTFam Char   b      c
    GADTFamCon2 :: Double -> GADTFam Double Double c
    GADTFamCon3 :: Int    -> GADTFam Int    String c
    GADTFamCon4 :: a      -> GADTFam a      b      c
    GADTFamCon5 :: b      -> GADTFam b      b      c
deriving instance (S.Show a, S.Show b) => S.Show (GADTFam a b c)
#endif
