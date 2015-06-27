{-# LANGUAGE CPP              #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# OPTIONS_GHC -w #-}

{-|
Module:      Derived.DatatypeContexts
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines data types with DatatypeContexts (which are gross, but still possible).
-}
module Derived.DatatypeContexts (TyCon(..), TyFamily(..)) where

import GHC.Show (appPrec, appPrec1, showSpace)

import Prelude ()
import Prelude.Compat hiding (Show)

import Test.QuickCheck (Arbitrary(..))

import Text.Show as S
import Text.Show.Text as T
import Text.Show.Text.TH (mkShowbPrec, mkShowbPrecWith, mkShowbPrecWith2)

import TransformersCompat as S

-------------------------------------------------------------------------------

data Ord a => TyCon a b c = TyCon a b c
  deriving S.Show

-------------------------------------------------------------------------------

data family TyFamily
#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
    a b c :: *
#else
    x y z :: *
#endif

data instance Ord a => TyFamily a b c = TyFamily a b c
  deriving S.Show

-------------------------------------------------------------------------------

instance (Ord a, Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (TyCon a b c) where
    arbitrary = TyCon <$> arbitrary <*> arbitrary <*> arbitrary

instance (Ord a, Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (TyFamily a b c) where
    arbitrary = TyFamily <$> arbitrary <*> arbitrary <*> arbitrary

-------------------------------------------------------------------------------

instance (Ord a, S.Show a, S.Show b) => S.Show1 (TyCon a b) where
    showsPrecWith = showsPrecWith2 showsPrec
instance (Ord a, S.Show a) => S.Show2 (TyCon a) where
    showsPrecWith2 sp1 sp2 p (TyCon a b c) =
        showsThree sp1 sp2 "TyCon" p a b c

instance (Ord a, S.Show a, S.Show b) => S.Show1 (TyFamily a b) where
    showsPrecWith = showsPrecWith2 showsPrec
instance (Ord a, S.Show a) => S.Show2 (TyFamily a) where
    showsPrecWith2 sp1 sp2 p (TyFamily a b c) =
        showsThree sp1 sp2 "TyFamily" p a b c

showsThree :: S.Show a
           => (Int -> b -> ShowS) -> (Int -> c -> ShowS)
           -> String -> Int -> a -> b -> c -> ShowS
showsThree sp1 sp2 name p a b c = showParen (p > appPrec) $
      showString name      . showSpace
    . showsPrec appPrec1 a . showSpace
    . sp1 appPrec1 b       . showSpace
    . sp2 appPrec1 c

-------------------------------------------------------------------------------

$(return [])

instance (Ord a, T.Show a, T.Show b, T.Show c) => T.Show (TyCon a b c) where
    showbPrec = $(mkShowbPrec ''TyCon)
instance (Ord a, T.Show a, T.Show b) => T.Show1 (TyCon a b) where
    showbPrecWith = $(mkShowbPrecWith ''TyCon)
instance (Ord a, T.Show a) => T.Show2 (TyCon a) where
    showbPrecWith2 = $(mkShowbPrecWith2 ''TyCon)

#if MIN_VERSION_template_haskell(2,7,0)
instance (Ord a, T.Show a, T.Show b, T.Show c) => T.Show (TyFamily a b c) where
    showbPrec = $(mkShowbPrec 'TyFamily)
instance (Ord a, T.Show a, T.Show b) => T.Show1 (TyFamily a b) where
    showbPrecWith = $(mkShowbPrecWith 'TyFamily)
instance (Ord a, T.Show a) => T.Show2 (TyFamily a) where
    showbPrecWith2 = $(mkShowbPrecWith2 'TyFamily)
#endif
