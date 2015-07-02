{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module:      Spec.Utils
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Testing-related utility functions.
-}
module Spec.Utils (
      ioProperty
    , prop_matchesShow
    , prop_matchesShow1
    , prop_matchesShow2
    , prop_genericShow
    , prop_genericShow'
    , prop_genericShow1
    ) where

#include "generic.h"

#if __GLASGOW_HASKELL__ >= 702
import           GHC.Generics (Generic, Rep)
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
import           GHC.Generics (Generic1, Rep1)
# endif
import           Text.Show.Text.Generic
#endif

import           Prelude hiding (Show)

#if MIN_VERSION_QuickCheck(2,7,0)
import qualified Test.QuickCheck as QC (ioProperty)
#else
import           Test.QuickCheck (morallyDubiousIOProperty)
#endif
import           Test.QuickCheck (Property, Testable)

import qualified Text.Show as S (Show)
import           Text.Show.Text as T

import           TransformersCompat as S

ioProperty :: Testable prop => IO prop -> Property
#if MIN_VERSION_QuickCheck(2,7,0)
ioProperty = QC.ioProperty
#else
ioProperty = morallyDubiousIOProperty
#endif

-- | Verifies that a type's @Show@ instances coincide for both 'String's and 'Text',
-- irrespective of precedence.
prop_matchesShow :: (S.Show a, T.Show a) => Int -> a -> Bool
prop_matchesShow p x = showbPrec p (FromStringShow x) == showbPrec p x

-- | Verifies that a type's @Show1@ instances coincide for both 'String's and 'Text',
-- irrespective of precedence.
prop_matchesShow1 :: (S.Show1 f, T.Show1 f) => Int -> f a -> Bool
prop_matchesShow1 p x = showbPrecWith showb27Prec p (FromStringShow1 x)
                       == showbPrecWith showb27Prec p x

-- | Verifies that a type's @Show2@ instances coincide for both 'String's and 'Text',
-- irrespective of precedence.
prop_matchesShow2 :: (S.Show2 f, T.Show2 f) => Int -> f a b -> Bool
prop_matchesShow2 p x = showbPrecWith2 showb27Prec showb42Prec p (FromStringShow2 x)
                       == showbPrecWith2 showb27Prec showb42Prec p x

-- | Show the number 27, which certain parody singer-songwriters find humorous.
-- Useful for testing higher-order @Show@ classes.
showb27Prec :: Int -> a -> Builder
showb27Prec p _ = showbPrec p $ Just (27 :: Int)

-- | Show the number 42, which is said to be the answer to something or other.
-- Useful for testing higher-order @Show@ classes.
showb42Prec :: Int -> a -> Builder
showb42Prec p _ = showbPrec p $ Just (42 :: Int)

-- | Verifies that a type's @Show@ instance coincides with the output produced
-- by the equivalent 'Generic' functions.
#if __GLASGOW_HASKELL__ >= 702
prop_genericShow :: (T.Show a, Generic a, GShow (Rep a))
                 => Int -> a -> Bool
prop_genericShow p x = showbPrec p x == genericShowbPrec p x
#else
prop_genericShow :: Int -> a -> Bool
prop_genericShow _ _ = True
#endif

-- | Behaves exactly like 'prop_genericShow', except only for GHC 7.6 and above.
-- This is useful with type families, which couldn't properly have derived 'Generic'
-- instances until GHC 7.6 due to a bug.
#if __GLASGOW_HASKELL__ >= 706
prop_genericShow' :: (T.Show a, Generic a, GShow (Rep a))
                   => Int -> a -> Bool
prop_genericShow' = prop_genericShow
#else
prop_genericShow' :: Int -> f a -> Bool
prop_genericShow' _ _ = True
#endif

-- | Verifies that a type's @Show1@ instance coincides with the output produced
-- by the equivalent 'Generic1' functions.
#if defined(__LANGUAGE_DERIVE_GENERIC1__)
prop_genericShow1 :: (T.Show1 f, Generic1 f, GShow1 (Rep1 f))
                  => Int -> f a -> Bool
prop_genericShow1 p x = showbPrecWith showb27Prec p x
                        == genericShowbPrecWith showb27Prec p x
#else
prop_genericShow1 :: Int -> f a -> Bool
prop_genericShow1 _ _ = True
#endif
