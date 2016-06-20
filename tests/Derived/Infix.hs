{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric     #-}
#endif

{-|
Module:      Derived.Infix
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines data types with infix constructors.
-}
module Derived.Infix (
      TyConPlain(..)
    , TyConGADT(..)
    , TyFamilyPlain(..)
    , TyFamilyGADT(..)
    ) where

#include "generic.h"

#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
import qualified Generics.Deriving.TH as Generics
#endif

#if __GLASGOW_HASKELL__ >= 702
import           GHC.Generics (Generic)
# if __GLASGOW_HASKELL__ >= 706
import           GHC.Generics (Generic1)
# endif
#endif

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..), oneof)

import           Text.Show.Deriving (deriveShow1)
import           TextShow.TH (deriveTextShow, deriveTextShow1, deriveTextShow2)

#if defined(NEW_FUNCTOR_CLASSES)
# if MIN_VERSION_template_haskell(2,7,0)
import           Text.Show.Deriving (deriveShow2)
# else
import           Data.Functor.Classes (Show1(..), Show2(..), showsBinaryWith)
import           GHC.Show (appPrec, appPrec1, showSpace)
# endif
#endif

-------------------------------------------------------------------------------

infixl 3 :!:
infix  4 :@:
infixr 5 `TyConPlain`
infixr 6 `TyConFakeInfix`
data TyConPlain a b = (:!:) a b
                    | a :@: b
                    | a `TyConPlain` b
                    | TyConFakeInfix a b
  deriving ( Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
# if __GLASGOW_HASKELL__ >= 706
           , Generic1
# endif
#endif
           )

-------------------------------------------------------------------------------

infixr 1 :., :..., :....
data TyConGADT a b where
    (:.)    ::           c ->       d        -> TyConGADT c d
    (:..)   ::           e ->       f        -> TyConGADT e f
    (:...)  ::           g ->       h -> Int -> TyConGADT g h
    (:....) :: { tcg1 :: i, tcg2 :: j }      -> TyConGADT i j
  deriving ( Show
#if __GLASGOW_HASKELL__ >= 706
           , Generic
           , Generic1
#endif
           )

-------------------------------------------------------------------------------

data family TyFamilyPlain y z :: *

infixl 3 :#:
infix  4 :$:
infixr 5 `TyFamilyPlain`
infixr 6 `TyFamilyFakeInfix`
data instance TyFamilyPlain a b = (:#:) a b
                                | a :$: b
                                | a `TyFamilyPlain` b
                                | TyFamilyFakeInfix a b
  deriving ( Show
#if __GLASGOW_HASKELL__ >= 706
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
#endif
           )

-------------------------------------------------------------------------------

data family TyFamilyGADT y z :: *

infixr 1 :*, :***, :****
data instance TyFamilyGADT a b where
    (:*)    ::           c ->       d        -> TyFamilyGADT c d
    (:**)   ::           e ->       f        -> TyFamilyGADT e f
    (:***)  ::           g ->       h -> Int -> TyFamilyGADT g h
    (:****) :: { tfg1 :: i, tfg2 :: j }      -> TyFamilyGADT i j
  deriving ( Show
#if __GLASGOW_HASKELL__ >= 706
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
#endif
           )

-------------------------------------------------------------------------------

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyConPlain a b) where
    arbitrary = oneof (map pure [(:!:), (:@:), TyConPlain]) <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyConGADT a b) where
    arbitrary = oneof [ pure (:.)
                      , pure (:..)
                      , flip (flip . (:...)) <$> arbitrary
                      , pure (:....)
                      ]
                <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyFamilyPlain a b) where
    arbitrary = oneof (map pure [(:#:), (:$:), TyFamilyPlain]) <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyFamilyGADT a b) where
    arbitrary = oneof [ pure (:*)
                      , pure (:**)
                      , flip (flip . (:***)) <$> arbitrary
                      , pure (:****)
                      ]
                <*> arbitrary <*> arbitrary

-------------------------------------------------------------------------------

$(deriveShow1 ''TyConPlain)
$(deriveShow1 ''TyConGADT)
#if defined(NEW_FUNCTOR_CLASSES)
$(deriveShow2 ''TyConPlain)
$(deriveShow2 ''TyConGADT)
#endif

$(deriveTextShow  ''TyConPlain)
$(deriveTextShow1 ''TyConPlain)
$(deriveTextShow2 ''TyConPlain)

$(deriveTextShow  ''TyConGADT)
$(deriveTextShow1 ''TyConGADT)
$(deriveTextShow2 ''TyConGADT)

#if !defined(NEW_FUNCTOR_CLASSES)
$(deriveShow1 '(:#:))

$(deriveShow1 '(:*))
#elif MIN_VERSION_template_haskell(2,7,0)
$(deriveShow1 '(:#:))
$(deriveShow2 '(:$:))

$(deriveShow1 '(:*))
$(deriveShow2 '(:***))
#else
instance Show a => Show1 (TyFamilyPlain a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList
instance Show a => Show1 (TyFamilyGADT a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show2 TyFamilyPlain where
    liftShowsPrec2 sp1 _ sp2 _ p (a :#: b) =
        showsBinaryWith sp1 sp2 "(:#:)" p a b
    liftShowsPrec2 sp1 _ sp2 _ p (a :$: b) =
        showsInfix sp1 sp2 ":$:" p 4 a b
    liftShowsPrec2 sp1 _ sp2 _ p (TyFamilyPlain a b) =
        showsInfix sp1 sp2 "`TyFamilyPlain`" p 5 a b
    liftShowsPrec2 sp1 _ sp2 _ p (TyFamilyFakeInfix a b) =
        showsBinaryWith sp1 sp2 "TyFamilyFakeInfix" p a b
instance Show2 TyFamilyGADT where
    liftShowsPrec2 sp1 _ sp2 _ p (a :* b) =
        showsInfix sp1 sp2 ":*" p 1 a b
    liftShowsPrec2 sp1 _ sp2 _ p (a :** b) =
        showsBinaryWith sp1 sp2 "(:**)" p a b
    liftShowsPrec2 sp1 _ sp2 _ p ((:***) a b i) =
        showsTernaryWith sp1 sp2 "(:***)" p a b i
    liftShowsPrec2 sp1 _ sp2 _ p (a :**** b) =
        showsBinaryWith sp1 sp2 "(:****)" p a b

showsInfix :: (Int -> a -> ShowS) -> (Int -> b -> ShowS)
           -> String -> Int -> Int -> a -> b -> ShowS
showsInfix sp1 sp2 name p infixPrec a b = showParen (p > infixPrec) $
      sp1 (infixPrec + 1) a . showSpace
    . showString name       . showSpace
    . sp2 (infixPrec + 1) b

showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS)
                 -> String -> Int -> a -> b -> Int -> ShowS
showsTernaryWith sp1 sp2 name p a b i = showParen (p > appPrec) $
      showString name . showSpace
    . sp1 appPrec1 a  . showSpace
    . sp2 appPrec1 b  . showSpace
    . showsPrec appPrec1 i
#endif

#if MIN_VERSION_template_haskell(2,7,0)
$(deriveTextShow  '(:#:))
$(deriveTextShow1 '(:$:))
$(deriveTextShow2 'TyFamilyPlain)

$(deriveTextShow  '(:*))
$(deriveTextShow1 '(:***))
$(deriveTextShow2 '(:****))
#endif

#if __GLASGOW_HASKELL__ < 706
$(Generics.deriveMeta           ''TyConPlain)
$(Generics.deriveRepresentable1 ''TyConPlain)
$(Generics.deriveAll0And1       ''TyConGADT)
#endif

#if __GLASGOW_HASKELL__ < 702
$(Generics.deriveRepresentable0 ''TyConPlain)
#endif

#if MIN_VERSION_template_haskell(2,7,0)
# if !defined(__LANGUAGE_DERIVE_GENERIC1__)
$(Generics.deriveMeta           '(:#:))
$(Generics.deriveRepresentable1 '(:$:))
$(Generics.deriveMeta           '(:*))
$(Generics.deriveRepresentable1 '(:**))
# endif

# if __GLASGOW_HASKELL__ < 706
$(Generics.deriveRepresentable0 'TyFamilyPlain)
$(Generics.deriveRepresentable0 '(:***))
# endif
#endif
