{-# LANGUAGE CPP              #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
-- I don't know how to silence the -XDatatypeContexts warnings otherwise...
{-# OPTIONS_GHC -w #-}

{-|
Module:      Derived.DatatypeContexts
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines data types with DatatypeContexts (which are gross, but still possible).
-}
module Derived.DatatypeContexts (TyCon(..), TyFamily(..)) where

import Data.Functor.Classes (Show1(..))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

import TextShow (TextShow(..), TextShow1(..), TextShow2(..))
import TextShow.TH (makeShowbPrec, makeLiftShowbPrec, makeLiftShowbPrec2)

#if defined(NEW_FUNCTOR_CLASSES)
import Data.Functor.Classes (Show2(..))
import Text.Show.Deriving (makeLiftShowsPrec, makeLiftShowsPrec2)
# if MIN_VERSION_template_haskell(2,7,0)
import Text.Show.Deriving (deriveShow2)
# else
import GHC.Show (appPrec, appPrec1, showSpace)
# endif
#else
import Text.Show.Deriving (makeShowsPrec1)
#endif

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

$(return [])

instance (Ord a, Show a, Show b) => Show1 (TyCon a b) where
#if defined(NEW_FUNCTOR_CLASSES)
    liftShowsPrec = $(makeLiftShowsPrec ''TyCon)
#else
    showsPrec1 = $(makeShowsPrec1 ''TyCon)
#endif
#if defined(NEW_FUNCTOR_CLASSES)
instance (Ord a, Show a) => Show2 (TyCon a) where
    liftShowsPrec2 = $(makeLiftShowsPrec2 ''TyCon)
#endif

#if !defined(NEW_FUNCTOR_CLASSES)
instance (Ord a, Show a, Show b) => Show1 (TyFamily a b) where
    showsPrec1 = $(makeShowsPrec1 'TyFamily)
#elif MIN_VERSION_template_haskell(2,7,0)
instance (Ord a, Show a, Show b) => Show1 (TyFamily a b) where
    liftShowsPrec = $(makeLiftShowsPrec 'TyFamily)

instance (Ord a, Show a) => Show2 (TyFamily a) where
    liftShowsPrec2 = $(makeLiftShowsPrec2 'TyFamily)
#else
instance (Ord a, Show a, Show b) => Show1 (TyFamily a b) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Ord a, Show a) => Show2 (TyFamily a) where
    liftShowsPrec2 sp1 _ sp2 _ p (TyFamily a b c) =
        showsThree sp1 sp2 "TyFamily" p a b c

showsThree :: Show a
           => (Int -> b -> ShowS) -> (Int -> c -> ShowS)
           -> String -> Int -> a -> b -> c -> ShowS
showsThree sp1 sp2 name p a b c = showParen (p > appPrec) $
      showString name      . showSpace
    . showsPrec appPrec1 a . showSpace
    . sp1 appPrec1 b       . showSpace
    . sp2 appPrec1 c
#endif

instance (Ord a, TextShow a, TextShow b, TextShow c) => TextShow (TyCon a b c) where
    showbPrec = $(makeShowbPrec ''TyCon)
instance (Ord a, TextShow a, TextShow b) => TextShow1 (TyCon a b) where
    liftShowbPrec = $(makeLiftShowbPrec ''TyCon)
instance (Ord a, TextShow a) => TextShow2 (TyCon a) where
    liftShowbPrec2 = $(makeLiftShowbPrec2 ''TyCon)

#if MIN_VERSION_template_haskell(2,7,0)
instance (Ord a, TextShow a, TextShow b, TextShow c) => TextShow (TyFamily a b c) where
    showbPrec = $(makeShowbPrec 'TyFamily)
instance (Ord a, TextShow a, TextShow b) => TextShow1 (TyFamily a b) where
    liftShowbPrec = $(makeLiftShowbPrec 'TyFamily)
instance (Ord a, TextShow a) => TextShow2 (TyFamily a) where
    liftShowbPrec2 = $(makeLiftShowbPrec2 'TyFamily)
#endif
