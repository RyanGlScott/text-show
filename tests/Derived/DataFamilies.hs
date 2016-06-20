{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric              #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE PolyKinds                  #-}
#endif

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
-- Starting with GHC 7.10, NullaryTypeClasses was deprecated in favor of
-- MultiParamTypeClasses, which is already enabled
{-# LANGUAGE NullaryTypeClasses         #-}
#endif

{-|
Module:      Derived.DataFamilies
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines corner case-provoking data families.
-}
module Derived.DataFamilies (
      NotAllShow(..)
#if __GLASGOW_HASKELL__ >= 706
    , KindDistinguished(..)
#endif
#if __GLASGOW_HASKELL__ >= 708
    , NullaryClass(..)
    , NullaryData(..)
#endif
    ) where

#include "generic.h"

#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
import qualified Generics.Deriving.TH as Generics
#endif

#if __GLASGOW_HASKELL__ >= 706
import           GHC.Generics (Generic)
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
import           GHC.Generics (Generic1)
# endif
#endif

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..), oneof)

#if MIN_VERSION_template_haskell(2,7,0)
import           Text.Show.Deriving (deriveShow1)
import           TextShow.TH (deriveTextShow, deriveTextShow1, deriveTextShow2)
#endif

#if defined(NEW_FUNCTOR_CLASSES)
# if MIN_VERSION_template_haskell(2,7,0)
import           Text.Show.Deriving (deriveShow2)
# else
import           Data.Functor.Classes (Show1(..), Show2(..),
                                       showsUnaryWith, showsBinaryWith)
# endif
#endif

-------------------------------------------------------------------------------

data family NotAllShow (w :: *) (x :: *) (y :: *) (z :: *) :: *

data instance NotAllShow ()  ()  () d = NASNoShow
data instance NotAllShow Int b   c  d = NASShow1 c b
                                      | NASShow2 d
  deriving ( Show
#if __GLASGOW_HASKELL__ >= 706
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
#endif
           )

instance (Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (NotAllShow Int b c d) where
    arbitrary = oneof [ NASShow1 <$> arbitrary <*> arbitrary
                      , NASShow2 <$> arbitrary
                      ]

#if !defined(NEW_FUNCTOR_CLASSES)
$(deriveShow1 'NASShow1)
#elif MIN_VERSION_template_haskell(2,7,0)
$(deriveShow1 'NASShow1)
$(deriveShow2 'NASShow2)
#else
instance (Show b, Show c) => Show1 (NotAllShow Int b c) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList
instance Show b => Show2 (NotAllShow Int b) where
    liftShowsPrec2 sp1 _ _ _ p (NASShow1 c b) =
        showsBinaryWith sp1 showsPrec "NASShow1" p c b
    liftShowsPrec2 _ _ sp2 _ p (NASShow2 d) =
        showsUnaryWith sp2 "NASShow2" p d
#endif

#if MIN_VERSION_template_haskell(2,7,0)
$(deriveTextShow  'NASShow1)
$(deriveTextShow1 'NASShow2)
$(deriveTextShow2 'NASShow1)

# if !defined(__LANGUAGE_DERIVE_GENERIC1__)
$(Generics.deriveMeta           'NASShow1)
$(Generics.deriveRepresentable1 'NASShow2)
# endif

# if __GLASGOW_HASKELL__ < 706
$(Generics.deriveRepresentable0 'NASShow1)
# endif
#endif

-------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 706
data family KindDistinguished (x :: k) (y :: *) (z :: *) :: *

data instance KindDistinguished (a :: ()) b c = KindDistinguishedUnit b c
  deriving ( Show
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
           )

data instance KindDistinguished (a :: Bool) b c = KindDistinguishedBool b c
  deriving ( Show
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
           )

instance (Arbitrary b, Arbitrary c)
      => Arbitrary (KindDistinguished (a :: ()) b c) where
    arbitrary = KindDistinguishedUnit <$> arbitrary <*> arbitrary

instance (Arbitrary b, Arbitrary c)
      => Arbitrary (KindDistinguished (a :: Bool) b c) where
    arbitrary = KindDistinguishedBool <$> arbitrary <*> arbitrary

#if !defined(NEW_FUNCTOR_CLASSES)
$(deriveShow1 'KindDistinguishedUnit)

$(deriveShow1 'KindDistinguishedBool)
#elif MIN_VERSION_template_haskell(2,7,0)
$(deriveShow1 'KindDistinguishedUnit)
$(deriveShow2 'KindDistinguishedUnit)

$(deriveShow1 'KindDistinguishedBool)
$(deriveShow2 'KindDistinguishedBool)
#else
instance Show b => Show1 (KindDistinguished (a :: ())   b) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList
instance Show b => Show1 (KindDistinguished (a :: Bool) b) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show2 (KindDistinguished (a :: ())) where
    liftShowsPrec2 sp1 _ sp2 _ p (KindDistinguishedUnit b c) =
        showsBinaryWith sp1 sp2 "KindDistinguishedUnit" p b c
instance Show2 (KindDistinguished (a :: Bool)) where
    liftShowsPrec2 sp1 _ sp2 _ p (KindDistinguishedBool b c) =
        showsBinaryWith sp1 sp2 "KindDistinguishedBool" p b c
#endif

$(deriveTextShow  'KindDistinguishedUnit)
$(deriveTextShow1 'KindDistinguishedUnit)
$(deriveTextShow2 'KindDistinguishedUnit)

$(deriveTextShow  'KindDistinguishedBool)
$(deriveTextShow1 'KindDistinguishedBool)
$(deriveTextShow2 'KindDistinguishedBool)

# if !defined(__LANGUAGE_DERIVE_GENERIC1__)
$(Generics.deriveAll1 'KindDistinguishedUnit)
$(Generics.deriveAll1 'KindDistinguishedBool)
# endif
#endif

-------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 708
class NullaryClass where
    data NullaryData :: *

instance NullaryClass where
    newtype NullaryData = NullaryCon Int
      deriving (Arbitrary, Show, Generic)

$(deriveTextShow 'NullaryCon)

# if __GLASGOW_HASKELL__ < 706
$(Generics.deriveAll 'NullaryCon)
# endif
#endif
