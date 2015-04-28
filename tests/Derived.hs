{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric              #-}
#endif

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

#if __GLASGOW_HASKELL__ >= 709 || \
   (__GLASGOW_HASKELL__ == 708 && \
    defined(__GLASGOW_HASKELL_PATCHLEVEL1__) && \
    __GLASGOW_HASKELL_PATCHLEVEL1__ == 4)
-- Workaround for https://ghc.haskell.org/trac/ghc/ticket/9563
# define LANGUAGE_DeriveGeneric1TypeFamilies
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
    , NormalGADT(..)
    , ExistentialGADT(..)
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

#if __GLASGOW_HASKELL__ >= 702
import           GHC.Generics (Generic)
# if __GLASGOW_HASKELL__ >= 706
import           GHC.Generics (Generic1)
# endif
#endif
import           GHC.Prim (Char#, Double#, Float#, Int#, Word#)

import           Prelude hiding (Show)

import           Test.QuickCheck (Arbitrary)

import qualified Text.Show as S (Show)
import qualified Text.Show.Text as T (Show)

data Nullary = Nullary
  deriving ( S.Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
#endif
           )

data PhantomNullary a = PhantomNullary
  deriving ( S.Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
# if __GLASGOW_HASKELL__ >= 708
-- Won't work on GHC 7.6 due to a PolyKinds-related bug
           , Generic1
# endif
#endif
           )

newtype MonomorphicUnary = MonomorphicUnary Int
  deriving ( S.Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
#endif
           )

newtype PolymorphicUnary a b = PolymorphicUnary b
  deriving ( S.Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
# if __GLASGOW_HASKELL__ >= 706
           , Generic1
# endif
#endif
           )

data MonomorphicProduct = MonomorphicProduct Char Double Int
  deriving ( S.Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
#endif
           )

data PolymorphicProduct a b c d = PolymorphicProduct a b d
  deriving ( S.Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
# if __GLASGOW_HASKELL__ >= 706
           , Generic1
# endif
#endif
           )

data MonomorphicRecord = MonomorphicRecord {
    monomorphicRecord1 :: Char
  , monomorphicRecord2 :: Double
  , monomorphicRecord3 :: Int
} deriving ( S.Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
#endif
           )

infixr 5 :%%%:
data PolymorphicRecord a b c d = (:%%%:) {
    polymorphicRecord1 :: a
  , polymorphicRecord2 :: b
  , polymorphicRecord3 :: d
} deriving ( S.Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
# if __GLASGOW_HASKELL__ >= 706
           , Generic1
# endif
#endif
           )

infix 7 :/:
data MonomorphicInfix = Int :/: Double
  deriving ( S.Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
#endif
           )

infix 8 `PolyInf`
data PolymorphicInfix a b c = a `PolyInf` c
  deriving ( S.Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
# if __GLASGOW_HASKELL__ >= 706
           , Generic1
# endif
#endif
           )

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

infixr 1 :.
data NormalGADT where
    (:.)  :: Int -> Int -> NormalGADT
    (:..) :: Int -> Int -> NormalGADT
  deriving ( S.Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
#endif
           )

data ExistentialGADT a b c where
    GADTCon1 ::           ExistentialGADT Char   b      c
    GADTCon2 :: Double -> ExistentialGADT Double Double c
    GADTCon3 :: Int    -> ExistentialGADT Int    String c
    GADTCon4 :: a      -> ExistentialGADT a      b      c
    GADTCon5 :: b      -> ExistentialGADT b      b      c
deriving instance (S.Show a, S.Show b) => S.Show (ExistentialGADT a b c)

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
  deriving ( S.Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
# if __GLASGOW_HASKELL__ >= 706
           , Generic1
# endif
#endif
           )

infixl 5 :>:
data RightAssocTree a = RightAssocLeaf a
                      | RightAssocTree a :>: RightAssocTree a
  deriving ( S.Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
# if __GLASGOW_HASKELL__ >= 706
           , Generic1
# endif
#endif
           )

infix 4 :?:
data a :?: b = (:?:) a b
  deriving ( S.Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
# if __GLASGOW_HASKELL__ >= 706
           , Generic1
# endif
#endif
           )

newtype HigherKindedTypeParams f a = HigherKindedTypeParams (f a)
  deriving ( S.Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
# if __GLASGOW_HASKELL__ >= 706
           , Generic1
# endif
#endif
           )

newtype Restriction a = Restriction a
#if __GLASGOW_HASKELL__ >= 702
  deriving ( Generic
# if __GLASGOW_HASKELL__ >= 706
           , Generic1
# endif
#endif
           )
deriving instance (Read a, S.Show a) => S.Show (Restriction a)

newtype RestrictedContext a = RestrictedContext (Restriction a)
  deriving ( S.Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
# if __GLASGOW_HASKELL__ >= 706
           , Generic1
# endif
#endif
           )

newtype Fix f = Fix (f (Fix f))
#if __GLASGOW_HASKELL__ >= 702
  deriving (Generic)
#endif
deriving instance S.Show (f (Fix f)) => S.Show (Fix f)

#if MIN_VERSION_template_haskell(2,7,0)
infix 2 `ASInfix`
data family AllShow a b c d :: *
data instance AllShow () () c d = ASNullary
  deriving ( S.Show
# if __GLASGOW_HASKELL__ >= 706
           , Generic
#  if defined(LANGUAGE_DeriveGeneric1TypeFamilies)
           , Generic1
#  endif
# endif
           )
newtype instance AllShow Int b c d = ASUnary Int
  deriving ( S.Show
# if __GLASGOW_HASKELL__ >= 706
           , Generic
#  if defined(LANGUAGE_DeriveGeneric1TypeFamilies)
           , Generic1
#  endif
# endif
           )
data instance AllShow Bool Bool c d = ASProduct Bool Bool
  deriving ( S.Show
# if __GLASGOW_HASKELL__ >= 706
           , Generic
#  if defined(LANGUAGE_DeriveGeneric1TypeFamilies)
           , Generic1
#  endif
# endif
           )
data instance AllShow Char Double c d = ASRecord {
    asRecord1 :: Char
  , asRecord2 :: Double
  , asRecord3 :: c
  } deriving ( S.Show
# if __GLASGOW_HASKELL__ >= 706
             , Generic
#  if defined(LANGUAGE_DeriveGeneric1TypeFamilies)
           , Generic1
#  endif
# endif
             )
data instance AllShow Float Ordering c d = Float `ASInfix` Ordering
  deriving ( S.Show
# if __GLASGOW_HASKELL__ >= 706
           , Generic
#  if defined(LANGUAGE_DeriveGeneric1TypeFamilies)
           , Generic1
#  endif
# endif
           )

data family NotAllShow a b c d :: *
data instance NotAllShow ()  ()  ()  d = NASNoShow
data instance NotAllShow Int b   Int d = NASShow1 Int Int
                                       | NASShow2 b
  deriving ( S.Show
# if __GLASGOW_HASKELL__ >= 706
           , Generic
#  if defined(LANGUAGE_DeriveGeneric1TypeFamilies)
           , Generic1
#  endif
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
           , Generic
#  if defined(LANGUAGE_DeriveGeneric1TypeFamilies)
           , Generic1
#  endif
# endif
           )

class AssocClass1 a where
    data AssocData1 a :: *
instance AssocClass1 () where
    newtype AssocData1 () = AssocCon1 Int
      deriving ( S.Show
# if __GLASGOW_HASKELL__ >= 706
               , Generic
# endif
               )

class AssocClass2 a b c where
    data AssocData2 a b c :: *
instance AssocClass2 () Int Int where
    newtype AssocData2 () Int Int = AssocCon2 Int
      deriving ( S.Show
# if __GLASGOW_HASKELL__ >= 706
               , Generic
# endif
               )

-- TODO: Uncomment this when smarter the typeclass instance context solver is implemented
-- 
-- class AssocClass3 a b c where
--     data AssocData3 a b c :: *
-- instance AssocClass3 () b c where
--     newtype AssocData3 () b c = AssocCon3 Int
--       deriving ( S.Show
-- # if __GLASGOW_HASKELL__ >= 706
--                , Generic
-- #  if defined(LANGUAGE_DeriveGeneric1TypeFamilies)
--                , Generic1
-- #  endif
-- # endif
--                )

# if __GLASGOW_HASKELL__ >= 708
class NullaryClass where
    data NullaryData
instance NullaryClass where
    newtype NullaryData = NullaryCon Int
      deriving (S.Show, Generic)
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
