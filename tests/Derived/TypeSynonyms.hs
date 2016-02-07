{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric              #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Derived.TypeSynonyms
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines data types that use type synonyms.
-}
module Derived.TypeSynonyms (TyCon(..), TyFamily(..)) where

#include "generic.h"

import           Data.Functor.Classes ( Show1(..)
#if !(MIN_VERSION_transformers(0,4,0)) || MIN_VERSION_transformers(0,5,0)
                                      , Show2(..)
                                      , showsUnaryWith
#endif
                                      )

#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
import qualified Generics.Deriving.TH as Generics
#endif

#if __GLASGOW_HASKELL__ >= 702
import           GHC.Generics (Generic)
# if __GLASGOW_HASKELL__ >= 706
import           GHC.Generics (Generic1)
# endif
#endif

import           Prelude

import           Test.QuickCheck (Arbitrary)

import           TextShow.TH (deriveTextShow, deriveTextShow1, deriveTextShow2)

-------------------------------------------------------------------------------

type FakeOut a = Int
type Id a = a
type Flip f a b = f b a

-- Needed for the Generic1 instances
instance Functor ((,,,) a b c) where
    fmap f (a, b, c, d) = (a, b, c, f d)

#if !(MIN_VERSION_transformers(0,4,0)) || MIN_VERSION_transformers(0,5,0)
instance (Show a, Show b) => Show2 ((,,,) a b) where
    liftShowsPrec2 sp1 _ sp2 _ _ (a, b, c, d) =
                          showChar '('
        . showsPrec 0 a . showChar ','
        . showsPrec 0 b . showChar ','
        . sp1       0 c . showChar ','
        . sp2       0 d . showChar ')'
#endif

-------------------------------------------------------------------------------

newtype TyCon a b = TyCon
    ( Id (FakeOut (Id a))
    , Id (FakeOut (Id b))
    , Id (Flip Either (Id a) (Id Int))
    , Id (Flip Either (Id b) (Id a))
    )
  deriving ( Arbitrary
           , Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
# if __GLASGOW_HASKELL__ >= 706
           , Generic1
# endif
#endif
           )

-------------------------------------------------------------------------------

data family TyFamily y z :: *

newtype instance TyFamily a b = TyFamily
    ( Id (FakeOut (Id a))
    , Id (FakeOut (Id b))
    , Id (Flip Either (Id a) (Id Int))
    , Id (Flip Either (Id b) (Id a))
    )
  deriving ( Arbitrary
           , Show
#if __GLASGOW_HASKELL__ >= 706
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
#endif
           )

-------------------------------------------------------------------------------

#if MIN_VERSION_transformers(0,4,0) && !(MIN_VERSION_transformers(0,5,0))
instance Show a => Show1 (TyCon a) where
    showsPrec1 = showsPrec
instance Show a => Show1 (TyFamily a) where
    showsPrec1 = showsPrec
#else
instance Show a => Show1 (TyCon a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList
instance Show a => Show1 (TyFamily a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show2 TyCon where
    liftShowsPrec2 sp1 sl1 sp2 sl2 p (TyCon x) =
        showsUnaryWith (liftShowsPrec2 (liftShowsPrec2 showsPrec showList sp1 sl1)
                                       (liftShowList2  showsPrec showList sp1 sl1)
                                       (liftShowsPrec2 sp1       sl1      sp2 sl2)
                                       (liftShowList2  sp1       sl1      sp2 sl2)
                       ) "TyCon" p x
instance Show2 TyFamily where
    liftShowsPrec2 sp1 sl1 sp2 sl2 p (TyFamily x) =
        showsUnaryWith (liftShowsPrec2 (liftShowsPrec2 showsPrec showList sp1 sl1)
                                       (liftShowList2  showsPrec showList sp1 sl1)
                                       (liftShowsPrec2 sp1       sl1      sp2 sl2)
                                       (liftShowList2  sp1       sl1      sp2 sl2)
                       ) "TyFamily" p x
#endif

-------------------------------------------------------------------------------

$(deriveTextShow  ''TyCon)
$(deriveTextShow1 ''TyCon)
$(deriveTextShow2 ''TyCon)

#if MIN_VERSION_template_haskell(2,7,0)
$(deriveTextShow  'TyFamily)
$(deriveTextShow1 'TyFamily)
$(deriveTextShow2 'TyFamily)
#endif

#if __GLASGOW_HASKELL__ < 706
$(Generics.deriveMeta           ''TyCon)
$(Generics.deriveRepresentable1 ''TyCon)
#endif

#if __GLASGOW_HASKELL__ < 702
$(Generics.deriveRepresentable0 ''TyCon)
#endif

#if MIN_VERSION_template_haskell(2,7,0)
# if !defined(__LANGUAGE_DERIVE_GENERIC1__)
$(Generics.deriveMeta           'TyFamily)
$(Generics.deriveRepresentable1 'TyFamily)
# endif

# if __GLASGOW_HASKELL__ < 706
$(Generics.deriveRepresentable0 'TyFamily)
# endif
#endif
