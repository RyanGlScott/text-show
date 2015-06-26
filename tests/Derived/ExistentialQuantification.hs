{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module:      Derived.ExistentialQuantification
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines data tpes with existentially quantified type variables.
-}
module Derived.ExistentialQuantification (TyCon(..), TyFamily(..)) where

import           GHC.Show (appPrec, appPrec1, showSpace)

import           Prelude ()
import           Prelude.Compat hiding (Show)

import           Test.QuickCheck (Arbitrary(..), oneof)

import           Text.Show as S (Show)
import           Text.Show.Text.TH (deriveShow, deriveShow1, deriveShow2)

import           TransformersCompat as S (Show1(..), Show2(..), showsBinaryWith)

-------------------------------------------------------------------------------

data TyCon a b c d where
    TyConClassConstraints    :: (Ord a, Ord b, Ord c, Ord d)
                             => a -> b -> c -> d
                             -> TyCon a b c d

    TyConEqualityConstraints :: (a ~ c, b ~ d, a ~ b)
                             => a -> b -> c -> d
                             -> TyCon a b c d

    TyConTypeRefinement      :: Int -> d
                             -> TyCon Int d Int d

-------------------------------------------------------------------------------

data family TyFamily a b c d :: *

data instance TyFamily a b c d where
    TyFamilyClassConstraints    :: (Ord a, Ord b, Ord c, Ord d)
                                => a -> b -> c -> d
                                -> TyFamily a b c d

    TyFamilyEqualityConstraints :: (a ~ c, b ~ d, a ~ b)
                                => a -> b -> c -> d
                                -> TyFamily a b c d

    TyFamilyTypeRefinement      :: Int -> d
                                -> TyFamily Int d Int d

-------------------------------------------------------------------------------

instance (a ~ Int, b ~ Int, c ~ Int, d ~ Int) => Arbitrary (TyCon a b c d) where
    arbitrary = oneof [ TyConClassConstraints    <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      , TyConEqualityConstraints <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      , TyConTypeRefinement      <$> arbitrary <*> arbitrary
                      ]

instance (a ~ Int, b ~ Int, c ~ Int, d ~ Int) => Arbitrary (TyFamily a b c d) where
    arbitrary = oneof [ TyFamilyClassConstraints    <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      , TyFamilyEqualityConstraints <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      , TyFamilyTypeRefinement      <$> arbitrary <*> arbitrary
                      ]

-------------------------------------------------------------------------------

deriving instance (S.Show a, S.Show b, S.Show c, S.Show d) => S.Show (TyCon a b c d)
instance (S.Show a, S.Show b, S.Show c) => S.Show1 (TyCon a b c) where
    showsPrecWith = showsPrecWith2 showsPrec
instance (S.Show a, S.Show b) => S.Show2 (TyCon a b) where
    showsPrecWith2 sp1 sp2 p (TyConClassConstraints a b c d) =
        showsFour sp1 sp2 "TyConClassConstraints" p a b c d
    showsPrecWith2 sp1 sp2 p (TyConEqualityConstraints a b c d) =
        showsFour sp1 sp2 "TyConEqualityConstraints" p a b c d
    showsPrecWith2 _ sp2 p (TyConTypeRefinement i d) =
        showsBinaryWith showsPrec sp2 "TyConTypeRefinement" p i d

deriving instance (S.Show a, S.Show b, S.Show c, S.Show d) => S.Show (TyFamily a b c d)
instance (S.Show a, S.Show b, S.Show c) => S.Show1 (TyFamily a b c) where
    showsPrecWith = showsPrecWith2 showsPrec
instance (S.Show a, S.Show b) => S.Show2 (TyFamily a b) where
    showsPrecWith2 sp1 sp2 p (TyFamilyClassConstraints a b c d) =
        showsFour sp1 sp2 "TyFamilyClassConstraints" p a b c d
    showsPrecWith2 sp1 sp2 p (TyFamilyEqualityConstraints a b c d) =
        showsFour sp1 sp2 "TyFamilyEqualityConstraints" p a b c d
    showsPrecWith2 _ sp2 p (TyFamilyTypeRefinement i d) =
        showsBinaryWith showsPrec sp2 "TyFamilyTypeRefinement" p i d

showsFour :: (S.Show a, S.Show b)
          => (Int -> c -> ShowS) -> (Int -> d -> ShowS)
          -> String -> Int -> a -> b -> c -> d -> ShowS
showsFour sp1 sp2 name p a b c d = showParen (p > appPrec) $
      showString name      . showSpace
    . showsPrec appPrec1 a . showSpace
    . showsPrec appPrec1 b . showSpace
    . sp1 appPrec1 c       . showSpace
    . sp2 appPrec1 d

-------------------------------------------------------------------------------

$(deriveShow  ''TyCon)
$(deriveShow1 ''TyCon)
$(deriveShow2 ''TyCon)

#if MIN_VERSION_template_haskell(2,7,0)
$(deriveShow  'TyFamilyClassConstraints)
$(deriveShow1 'TyFamilyEqualityConstraints)
$(deriveShow2 'TyFamilyTypeRefinement)
#endif
