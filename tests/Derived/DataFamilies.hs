{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric              #-}
#endif

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
-- Starting with GHC 7.10, NullaryTypeClasses was deprecated in favor of
-- MultiParamTypeClasses, which is already enabled
{-# LANGUAGE NullaryTypeClasses         #-}
#endif

{-|
Module:      Derived.DataFamilies
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines corner case-provoking data families.
-}
module Derived.DataFamilies (
      NotAllShow(..)
# if __GLASGOW_HASKELL__ >= 708
    , NullaryClass(..)
    , NullaryData(..)
#endif
    ) where

#include "generic.h"

#if __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic)
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
import GHC.Generics (Generic1)
# endif
#endif
import GHC.Show (appPrec, appPrec1, showSpace)

import Prelude ()
import Prelude.Compat hiding (Show)

import Test.QuickCheck (Arbitrary(..), oneof)

import Text.Show as S
#if MIN_VERSION_template_haskell(2,7,0)
import Text.Show.Text.TH (deriveShow, deriveShow1, deriveShow2)
#endif

import TransformersCompat as S

-------------------------------------------------------------------------------

data family NotAllShow
#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKEL__ < 710
    a b c d :: *
#else
    w x y z :: *
#endif

data instance NotAllShow ()  ()  () d = NASNoShow
data instance NotAllShow Int b   c  d = NASShow1 c b
                                      | NASShow2 d
  deriving ( S.Show
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

instance (S.Show b, S.Show c) => S.Show1 (NotAllShow Int b c) where
    showsPrecWith = showsPrecWith2 showsPrec
instance S.Show b => S.Show2 (NotAllShow Int b) where
    showsPrecWith2 sp1 _ p (NASShow1 c b) = showParen (p > appPrec) $
          showString "NASShow1 "
        . sp1 appPrec1 c . showSpace
        . showsPrec appPrec1 b
    showsPrecWith2 _ sp2 p (NASShow2 d) = showParen (p > appPrec) $
          showString "NASShow2 "
        . sp2 appPrec1 d

#if MIN_VERSION_template_haskell(2,7,0)
$(deriveShow  'NASShow1)
$(deriveShow1 'NASShow2)
$(deriveShow2 'NASShow1)
#endif

-------------------------------------------------------------------------------

# if __GLASGOW_HASKELL__ >= 708
class NullaryClass where
    data NullaryData :: *

instance NullaryClass where
    newtype NullaryData = NullaryCon Int
      deriving (Arbitrary, S.Show, Generic)

$(deriveShow 'NullaryCon)
# endif
