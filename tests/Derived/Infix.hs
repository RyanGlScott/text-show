{-# LANGUAGE CPP             #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric   #-}
#endif

{-|
Module:      Derived.Infix
Copyright:   (C) 2014-2015 Ryan Scott
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

#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
import GHC.Generics (Generic1)
# endif
#endif
import GHC.Show (appPrec, appPrec1, showSpace)

import Prelude ()
import Prelude.Compat hiding (Show(..))

import Test.QuickCheck (Arbitrary(..), oneof)

import Text.Show as S (Show(..))
import Text.Show.Text.TH (deriveShow, deriveShow1, deriveShow2)

import TransformersCompat as S (Show1(..), Show2(..), showsBinaryWith)

-------------------------------------------------------------------------------

infixl 3 :!:
infix  4 :@:
infixr 5 `TyConPlain`
data TyConPlain a b = (:!:) a b
                    | a :@: b
                    | a `TyConPlain` b
  deriving ( S.Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
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
  deriving ( S.Show
#if __GLASGOW_HASKELL__ >= 706
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
#endif
           )

-------------------------------------------------------------------------------

data family TyFamilyPlain
#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
                          a b :: *
#else
                          y z :: *
#endif

infixl 3 :#:
infix  4 :$:
infixr 5 `TyFamilyPlain`
data instance TyFamilyPlain a b = (:#:) a b
                                | a :$: b
                                | a `TyFamilyPlain` b
  deriving ( S.Show
#if __GLASGOW_HASKELL__ >= 706
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
#endif
           )

-------------------------------------------------------------------------------

data family TyFamilyGADT
#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
                         a b :: *
#else
                         y z :: *
#endif

infixr 1 :*, :***, :****
data instance TyFamilyGADT a b where
    (:*)    ::           c ->       d        -> TyFamilyGADT c d
    (:**)   ::           e ->       f        -> TyFamilyGADT e f
    (:***)  ::           g ->       h -> Int -> TyFamilyGADT g h
    (:****) :: { tfg1 :: i, tfg2 :: j }      -> TyFamilyGADT i j
  deriving ( S.Show
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

instance S.Show a => S.Show1 (TyConPlain a) where
    showsPrecWith = showsPrecWith2 showsPrec
instance S.Show2 TyConPlain where
    showsPrecWith2 sp1 sp2 p (a :!: b) =
        showsBinaryWith sp1 sp2 "(:!:)" p a b
    showsPrecWith2 sp1 sp2 p (a :@: b) =
        showsInfix sp1 sp2 ":@:" p 4 a b
    showsPrecWith2 sp1 sp2 p (TyConPlain a b) =
        showsInfix sp1 sp2 "`TyConPlain`" p 5 a b

instance S.Show a => S.Show1 (TyConGADT a) where
    showsPrecWith = showsPrecWith2 showsPrec
instance S.Show2 TyConGADT where
    showsPrecWith2 sp1 sp2 p (a :. b) =
        showsInfix sp1 sp2 ":." p 1 a b
    showsPrecWith2 sp1 sp2 p (a :.. b) =
        showsBinaryWith sp1 sp2 "(:..)" p a b
    showsPrecWith2 sp1 sp2 p ((:...) a b i) =
        showsTernaryWith sp1 sp2 "(:...)" p a b i
    showsPrecWith2 sp1 sp2 p (a :.... b) =
        showsBinaryWith sp1 sp2 "(:....)" p a b

instance S.Show a => S.Show1 (TyFamilyPlain a) where
    showsPrecWith = showsPrecWith2 showsPrec
instance S.Show2 TyFamilyPlain where
    showsPrecWith2 sp1 sp2 p (a :#: b) =
        showsBinaryWith sp1 sp2 "(:#:)" p a b
    showsPrecWith2 sp1 sp2 p (a :$: b) =
        showsInfix sp1 sp2 ":$:" p 4 a b
    showsPrecWith2 sp1 sp2 p (TyFamilyPlain a b) =
        showsInfix sp1 sp2 "`TyFamilyPlain`" p 5 a b

instance S.Show a => S.Show1 (TyFamilyGADT a) where
    showsPrecWith = showsPrecWith2 showsPrec
instance S.Show2 TyFamilyGADT where
    showsPrecWith2 sp1 sp2 p (a :* b) =
        showsInfix sp1 sp2 ":*" p 1 a b
    showsPrecWith2 sp1 sp2 p (a :** b) =
        showsBinaryWith sp1 sp2 "(:**)" p a b
    showsPrecWith2 sp1 sp2 p ((:***) a b i) =
        showsTernaryWith sp1 sp2 "(:***)" p a b i
    showsPrecWith2 sp1 sp2 p (a :**** b) =
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

-------------------------------------------------------------------------------

$(deriveShow  ''TyConPlain)
$(deriveShow1 ''TyConPlain)
$(deriveShow2 ''TyConPlain)

$(deriveShow  ''TyConGADT)
$(deriveShow1 ''TyConGADT)
$(deriveShow2 ''TyConGADT)

#if MIN_VERSION_template_haskell(2,7,0)
$(deriveShow  '(:#:))
$(deriveShow1 '(:$:))
$(deriveShow2 'TyFamilyPlain)

$(deriveShow  '(:*))
$(deriveShow1 '(:***))
$(deriveShow2 '(:****))
#endif
