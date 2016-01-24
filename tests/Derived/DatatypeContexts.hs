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
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

import TextShow (TextShow(..), TextShow1(..), TextShow2(..))
import TextShow.TH (makeShowbPrec, makeShowbPrecWith, makeShowbPrecWith2)

import TransformersCompat (Show1(..), Show2(..))

-------------------------------------------------------------------------------

data Ord a => TyCon a b c = TyCon a b c
  deriving Show

-------------------------------------------------------------------------------

data family TyFamily x y z :: *

data instance Ord a => TyFamily a b c = TyFamily a b c
  deriving Show

-------------------------------------------------------------------------------

instance (Ord a, Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (TyCon a b c) where
    arbitrary = TyCon <$> arbitrary <*> arbitrary <*> arbitrary

instance (Ord a, Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (TyFamily a b c) where
    arbitrary = TyFamily <$> arbitrary <*> arbitrary <*> arbitrary

-------------------------------------------------------------------------------

instance (Ord a, Show a, Show b) => Show1 (TyCon a b) where
    showsPrecWith = showsPrecWith2 showsPrec
instance (Ord a, Show a) => Show2 (TyCon a) where
    showsPrecWith2 sp1 sp2 p (TyCon a b c) =
        showsThree sp1 sp2 "TyCon" p a b c

instance (Ord a, Show a, Show b) => Show1 (TyFamily a b) where
    showsPrecWith = showsPrecWith2 showsPrec
instance (Ord a, Show a) => Show2 (TyFamily a) where
    showsPrecWith2 sp1 sp2 p (TyFamily a b c) =
        showsThree sp1 sp2 "TyFamily" p a b c

showsThree :: Show a
           => (Int -> b -> ShowS) -> (Int -> c -> ShowS)
           -> String -> Int -> a -> b -> c -> ShowS
showsThree sp1 sp2 name p a b c = showParen (p > appPrec) $
      showString name      . showSpace
    . showsPrec appPrec1 a . showSpace
    . sp1 appPrec1 b       . showSpace
    . sp2 appPrec1 c

-------------------------------------------------------------------------------

$(return [])

instance (Ord a, TextShow a, TextShow b, TextShow c) => TextShow (TyCon a b c) where
    showbPrec = $(makeShowbPrec ''TyCon)
instance (Ord a, TextShow a, TextShow b) => TextShow1 (TyCon a b) where
    showbPrecWith = $(makeShowbPrecWith ''TyCon)
instance (Ord a, TextShow a) => TextShow2 (TyCon a) where
    showbPrecWith2 = $(makeShowbPrecWith2 ''TyCon)

#if MIN_VERSION_template_haskell(2,7,0)
instance (Ord a, TextShow a, TextShow b, TextShow c) => TextShow (TyFamily a b c) where
    showbPrec = $(makeShowbPrec 'TyFamily)
instance (Ord a, TextShow a, TextShow b) => TextShow1 (TyFamily a b) where
    showbPrecWith = $(makeShowbPrecWith 'TyFamily)
instance (Ord a, TextShow a) => TextShow2 (TyFamily a) where
    showbPrecWith2 = $(makeShowbPrecWith2 'TyFamily)
#endif
