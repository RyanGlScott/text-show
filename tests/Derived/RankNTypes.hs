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
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines data types with rank-n voodoo.
-}
module Derived.RankNTypes (TyCon(..), TyFamily(..)) where

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

import TextShow (Show(..), Show1(..), Show2(..))
import TextShow.TH (deriveTextShow, deriveTextShow1, deriveTextShow2,
                    makeShowbPrec, makeShowbPrecWith, makeShowbPrecWith2)

import TransformersCompat (Show1(..), Show2(..), showsUnaryWith, showsBinaryWith)

-------------------------------------------------------------------------------

data TyCon a b = TyCon (forall a. Tagged2 a Int b)
                       (forall b. Tagged2 b a   a)

deriving instance (Show a, Show b) => Show (TyCon a b)

-------------------------------------------------------------------------------

data family TyFamily
#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
                     a b :: *
#else
                     x y :: *
#endif

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

instance Show a => Show1 (TyCon a) where
    showsPrecWith = showsPrecWith2 showsPrec
instance Show2 TyCon where
    showsPrecWith2 sp1 sp2 p (TyCon b a) =
        showsForall sp1 sp2 "TyCon" p b a

instance Show a => Show1 (TyFamily a) where
    showsPrecWith = showsPrecWith2 showsPrec
instance Show2 TyFamily where
    showsPrecWith2 sp1 sp2 p (TyFamily b a) =
        showsForall sp1 sp2 "TyFamily" p b a

showsForall :: (Int -> a -> ShowS) -> (Int -> b -> ShowS)
            -> String -> Int
            -> (forall a. Tagged2 a Int b)
            -> (forall b. Tagged2 b a a)
            -> ShowS
showsForall sp1 sp2 name p b a =
        showsBinaryWith (showsPrecWith2 showsPrec sp2)
                        (showsPrecWith2 sp1       sp1)
                        name p b a

-------------------------------------------------------------------------------

$(deriveShow  ''TyCon)
$(deriveShow1 ''TyCon)
$(deriveShow2 ''TyCon)

#if MIN_VERSION_template_haskell(2,7,0)
$(deriveShow  'TyFamily)
$(deriveShow1 'TyFamily)
$(deriveShow2 'TyFamily)
#endif

-------------------------------------------------------------------------------

$(return [])

instance TextShow1 (Tagged2 s t) where
    showsPrecWith sp p (Tagged2 b) = showsUnaryWith sp "Tagged2" p b

instance TextShow2 (Tagged2 s) where
    showsPrecWith2 _ = showsPrecWith

instance TextShow c => TextShow (Tagged2 s t c) where
    showbPrec = $(makeShowbPrec ''Tagged2)

instance TextShow1 (Tagged2 s t) where
    showbPrecWith = $(makeShowbPrecWith ''Tagged2)

instance TextShow2 (Tagged2 s) where
    showbPrecWith2 = $(makeShowbPrecWith2 ''Tagged2)
