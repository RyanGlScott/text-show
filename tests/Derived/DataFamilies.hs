{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

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
Copyright:   (C) 2014-2017 Ryan Scott
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

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..))

import           Text.Show.Deriving (deriveShow1)
#if defined(NEW_FUNCTOR_CLASSES)
import           Text.Show.Deriving (deriveShow2)
#endif

import           TextShow.TH (deriveTextShow, deriveTextShow1, deriveTextShow2)

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
    arbitrary = genericArbitrary

#if !defined(NEW_FUNCTOR_CLASSES)
$(deriveShow1 'NASShow1)
#else
$(deriveShow1 'NASShow1)
$(deriveShow2 'NASShow2)
#endif

$(deriveTextShow  'NASShow1)
$(deriveTextShow1 'NASShow2)
$(deriveTextShow2 'NASShow1)

#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
$(Generics.deriveMeta           'NASShow1)
$(Generics.deriveRepresentable1 'NASShow2)
#endif

#if __GLASGOW_HASKELL__ < 706
$(Generics.deriveRepresentable0 'NASShow1)
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
    arbitrary = genericArbitrary

instance (Arbitrary b, Arbitrary c)
      => Arbitrary (KindDistinguished (a :: Bool) b c) where
    arbitrary = genericArbitrary

# if !defined(NEW_FUNCTOR_CLASSES)
$(deriveShow1 'KindDistinguishedUnit)

$(deriveShow1 'KindDistinguishedBool)
# else
$(deriveShow1 'KindDistinguishedUnit)
$(deriveShow2 'KindDistinguishedUnit)

$(deriveShow1 'KindDistinguishedBool)
$(deriveShow2 'KindDistinguishedBool)
# endif

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
#endif
