{-# LANGUAGE CPP                #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-|
Module:      Derived.RankNTypes
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines data types with rank-n voodoo.
-}
module Derived.RankNTypes (TyCon(..), TyFamily(..)) where

import Data.Functor.Classes (Show1(..))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

import Text.Show.Deriving (deriveShow1)
import TextShow (TextShow(..), TextShow1(..), TextShow2(..))
import TextShow.TH (deriveTextShow, deriveTextShow1, deriveTextShow2,
                    makeShowbPrec, makeLiftShowbPrec, makeLiftShowbPrec2)

#if defined(NEW_FUNCTOR_CLASSES)
import Data.Functor.Classes (Show2(..))
import Text.Show.Deriving (makeLiftShowsPrec, makeLiftShowsPrec2)
# if MIN_VERSION_template_haskell(2,7,0)
import Text.Show.Deriving (deriveShow2)
# else
import Data.Functor.Classes (showsBinaryWith)
# endif
#else
import Text.Show.Deriving (makeShowsPrec1)
#endif

-------------------------------------------------------------------------------

data TyCon a b = TyCon (forall a. Tagged2 a Int b)
                       (forall b. Tagged2 b a   a)

deriving instance (Show a, Show b) => Show (TyCon a b)

-------------------------------------------------------------------------------

data family TyFamily x y :: *

data instance TyFamily a b = TyFamily (forall a. Tagged2 a Int b)
                                      (forall b. Tagged2 b a   a)

deriving instance (Show a, Show b) => Show (TyFamily a b)

-------------------------------------------------------------------------------

newtype Tagged2 s t c = Tagged2 c
  deriving Show

-------------------------------------------------------------------------------

-- There's so much rank-n voodoo going on that we can't have a more generalized
-- Arbitrary instances. Oh well, this is close enough.
instance Arbitrary (TyCon Int Int) where
    arbitrary = do
        i1 <- arbitrary
        i2 <- arbitrary
        pure $ TyCon (Tagged2 i1) (Tagged2 i2)

instance Arbitrary (TyFamily Int Int) where
    arbitrary = do
        i1 <- arbitrary
        i2 <- arbitrary
        pure $ TyFamily (Tagged2 i1) (Tagged2 i2)

-------------------------------------------------------------------------------

$(return [])

instance Show1 (Tagged2 s t) where
#if defined(NEW_FUNCTOR_CLASSES)
    liftShowsPrec = $(makeLiftShowsPrec ''Tagged2)
#else
    showsPrec1 = $(makeShowsPrec1 ''Tagged2)
#endif
#if defined(NEW_FUNCTOR_CLASSES)
instance Show2 (Tagged2 s) where
    liftShowsPrec2 = $(makeLiftShowsPrec2 ''Tagged2)
#endif

$(deriveShow1 ''TyCon)
#if defined(NEW_FUNCTOR_CLASSES)
$(deriveShow2 ''TyCon)
#endif

$(deriveTextShow  ''TyCon)
$(deriveTextShow1 ''TyCon)
$(deriveTextShow2 ''TyCon)

#if !defined(NEW_FUNCTOR_CLASSES)
$(deriveShow1 'TyFamily)
#elif MIN_VERSION_template_haskell(2,7,0)
$(deriveShow1 'TyFamily)
$(deriveShow2 'TyFamily)
#else
instance Show a => Show1 (TyFamily a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show2 TyFamily where
    liftShowsPrec2 sp1 sl1 sp2 sl2 p (TyFamily b a) =
        showsForall sp1 sl1 sp2 sl2 "TyFamily" p b a

showsForall :: (Int -> a -> ShowS) -> ([a] -> ShowS)
            -> (Int -> b -> ShowS) -> ([b] -> ShowS)
            -> String -> Int
            -> (forall a. Tagged2 a Int b)
            -> (forall b. Tagged2 b a a)
            -> ShowS
showsForall sp1 sl1 sp2 sl2 name p b a =
        showsBinaryWith (liftShowsPrec2 showsPrec showList sp2 sl2)
                        (liftShowsPrec2 sp1       sl1      sp1 sl1)
                        name p b a
#endif

#if MIN_VERSION_template_haskell(2,7,0)
$(deriveTextShow  'TyFamily)
$(deriveTextShow1 'TyFamily)
$(deriveTextShow2 'TyFamily)
#endif

-------------------------------------------------------------------------------

instance TextShow c => TextShow (Tagged2 s t c) where
    showbPrec = $(makeShowbPrec ''Tagged2)

instance TextShow1 (Tagged2 s t) where
    liftShowbPrec = $(makeLiftShowbPrec ''Tagged2)

instance TextShow2 (Tagged2 s) where
    liftShowbPrec2 = $(makeLiftShowbPrec2 ''Tagged2)
