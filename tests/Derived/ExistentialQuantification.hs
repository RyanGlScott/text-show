{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-|
Module:      Derived.ExistentialQuantification
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines data types with existentially quantified type variables.
-}
module Derived.ExistentialQuantification (TyCon(..), TyFamily(..)) where

import GHC.Show (appPrec, appPrec1, showSpace)

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), Gen, oneof)

import TextShow (TextShow)
import TextShow.TH (deriveTextShow, deriveTextShow1, deriveTextShow2)

import TransformersCompat (Show1(..), Show2(..), showsBinaryWith)

-------------------------------------------------------------------------------

data TyCon a b c d where
    TyConClassConstraints    :: (Ord m, Ord n, Ord o, Ord p)
                             => m -> n -> o -> p
                             -> TyCon m n o p

    TyConEqualityConstraints :: (e ~ g, f ~ h, e ~ f)
                                => e -> f -> g -> h
                             -> TyCon e f g h

    TyConTypeRefinement1,
      TyConTypeRefinement2   :: Int -> z
                             -> TyCon Int Int z z

    TyConForalls             :: forall p q r s t u.
                                (Arbitrary p, Show p, TextShow p,
                                 Arbitrary q, Show q, TextShow q)
                             => p -> q -> u -> t
                             -> TyCon r s t u

-------------------------------------------------------------------------------

data family TyFamily
#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
    a b c d :: *
#else
    w x y z :: *
#endif

data instance TyFamily _a _b _c _d where
    TyFamilyClassConstraints    :: (Ord m, Ord n, Ord o, Ord p)
                                => m -> n -> o -> p
                                -> TyFamily m n o p

    TyFamilyEqualityConstraints :: (e ~ g, f ~ h, e ~ f)
                                => e -> f -> g -> h
                                -> TyFamily e f g h

    TyFamilyTypeRefinement1,
      TyFamilyTypeRefinement2   :: Int -> z
                                -> TyFamily Int Int z z

    TyFamilyForalls             :: forall p q r s t u.
                                   (Arbitrary p, Show p, TextShow p,
                                    Arbitrary q, Show q, TextShow q)
                                => p -> q -> u -> t
                                -> TyFamily r s t u

-------------------------------------------------------------------------------

instance (a ~ Int, b ~ Int, c ~ Int, d ~ Int) => Arbitrary (TyCon a b c d) where
    arbitrary = oneof [ TyConClassConstraints    <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      , TyConEqualityConstraints <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      , TyConTypeRefinement1     <$> arbitrary <*> arbitrary
                      , TyConTypeRefinement2     <$> arbitrary <*> arbitrary
                      , TyConForalls             <$> (arbitrary :: Gen Int) <*> (arbitrary :: Gen Int)
                                                 <*> arbitrary              <*> arbitrary
                      ]

instance (a ~ Int, b ~ Int, c ~ Int, d ~ Int) => Arbitrary (TyFamily a b c d) where
    arbitrary = oneof [ TyFamilyClassConstraints    <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      , TyFamilyEqualityConstraints <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      , TyFamilyTypeRefinement1     <$> arbitrary <*> arbitrary
                      , TyFamilyTypeRefinement2     <$> arbitrary <*> arbitrary
                      , TyFamilyForalls             <$> (arbitrary :: Gen Int) <*> (arbitrary :: Gen Int)
                                                    <*> arbitrary              <*> arbitrary
                      ]

-------------------------------------------------------------------------------

deriving instance (Show a, Show b, Show c, Show d) => Show (TyCon a b c d)
instance (Show a, Show b, Show c) => Show1 (TyCon a b c) where
    showsPrecWith = showsPrecWith2 showsPrec
instance (Show a, Show b) => Show2 (TyCon a b) where
    showsPrecWith2 sp1 sp2 p (TyConClassConstraints a b c d) =
        showsFour sp1 sp2 "TyConClassConstraints" p a b c d
    showsPrecWith2 sp1 sp2 p (TyConEqualityConstraints a b c d) =
        showsFour sp1 sp2 "TyConEqualityConstraints" p a b c d
    showsPrecWith2 _   sp2 p (TyConTypeRefinement1 i d) =
        showsBinaryWith showsPrec sp2 "TyConTypeRefinement1" p i d
    showsPrecWith2 _   sp2 p (TyConTypeRefinement2 i d) =
        showsBinaryWith showsPrec sp2 "TyConTypeRefinement2" p i d
    showsPrecWith2 sp1 sp2 p (TyConForalls p' q d c) =
        showsFour sp2 sp1 "TyConForalls" p p' q d c

deriving instance (Show a, Show b, Show c, Show d) => Show (TyFamily a b c d)
instance (Show a, Show b, Show c) => Show1 (TyFamily a b c) where
    showsPrecWith = showsPrecWith2 showsPrec
instance (Show a, Show b) => Show2 (TyFamily a b) where
    showsPrecWith2 sp1 sp2 p (TyFamilyClassConstraints a b c d) =
        showsFour sp1 sp2 "TyFamilyClassConstraints" p a b c d
    showsPrecWith2 sp1 sp2 p (TyFamilyEqualityConstraints a b c d) =
        showsFour sp1 sp2 "TyFamilyEqualityConstraints" p a b c d
    showsPrecWith2 _ sp2 p (TyFamilyTypeRefinement1 i d) =
        showsBinaryWith showsPrec sp2 "TyFamilyTypeRefinement1" p i d
    showsPrecWith2 _ sp2 p (TyFamilyTypeRefinement2 i d) =
        showsBinaryWith showsPrec sp2 "TyFamilyTypeRefinement2" p i d
    showsPrecWith2 sp1 sp2 p (TyFamilyForalls p' q d c) =
        showsFour sp2 sp1 "TyFamilyForalls" p p' q d c

showsFour :: (Show a, Show b)
          => (Int -> c -> ShowS) -> (Int -> d -> ShowS)
          -> String -> Int -> a -> b -> c -> d -> ShowS
showsFour sp1 sp2 name p a b c d = showParen (p > appPrec) $
      showString name      . showSpace
    . showsPrec appPrec1 a . showSpace
    . showsPrec appPrec1 b . showSpace
    . sp1 appPrec1 c       . showSpace
    . sp2 appPrec1 d

-------------------------------------------------------------------------------

$(deriveTextShow  ''TyCon)
$(deriveTextShow1 ''TyCon)
$(deriveTextShow2 ''TyCon)

#if MIN_VERSION_template_haskell(2,7,0)
$(deriveTextShow  'TyFamilyClassConstraints)
$(deriveTextShow1 'TyFamilyTypeRefinement1)
$(deriveTextShow2 'TyFamilyTypeRefinement2)
#endif
