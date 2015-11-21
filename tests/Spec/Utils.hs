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
    , prop_matchesTextShow
    , prop_matchesTextShow1
    , prop_matchesTextShow2
    , prop_genericTextShow
    , prop_genericTextShow1
    ) where

#include "generic.h"

import           Generics.Deriving.Base

#if MIN_VERSION_QuickCheck(2,7,0)
import qualified Test.QuickCheck as QC (ioProperty)
#else
import           Test.QuickCheck (morallyDubiousIOProperty)
#endif
import           Test.QuickCheck (Property, Testable)

import           TextShow (TextShow(..), TextShow1(..), TextShow2(..),
                           Builder, FromStringShow(..))
import           TextShow.Generic

import           TransformersCompat (Show1, Show2,
                                    FromStringShow1(..), FromStringShow2(..))

ioProperty :: Testable prop => IO prop -> Property
#if MIN_VERSION_QuickCheck(2,7,0)
ioProperty = QC.ioProperty
#else
ioProperty = morallyDubiousIOProperty
#endif

-- | Verifies that a type's @Show@ instances coincide for both 'String's and 'Text',
-- irrespective of precedence.
prop_matchesTextShow :: (Show a, TextShow a) => Int -> a -> Bool
prop_matchesTextShow p x = showbPrec p (FromStringShow x) == showbPrec p x

-- | Verifies that a type's @Show1@ instances coincide for both 'String's and 'Text',
-- irrespective of precedence.
prop_matchesTextShow1 :: (Show1 f, TextShow1 f) => Int -> f a -> Bool
prop_matchesTextShow1 p x = showbPrecWith showb27Prec p (FromStringShow1 x)
                       == showbPrecWith showb27Prec p x

-- | Verifies that a type's @Show2@ instances coincide for both 'String's and 'Text',
-- irrespective of precedence.
prop_matchesTextShow2 :: (Show2 f, TextShow2 f) => Int -> f a b -> Bool
prop_matchesTextShow2 p x = showbPrecWith2 showb27Prec showb42Prec p (FromStringShow2 x)
                           == showbPrecWith2 showb27Prec showb42Prec p x

-- | Show the number 27, which certain parody singer-songwriters find humorous.
-- Useful for testing higher-order @Show@ classes.
showb27Prec :: Int -> a -> Builder
showb27Prec p _ = showbPrec p $ Just (27 :: Int)

-- | Show the number 42, which is said to be the answer to something or other.
-- Useful for testing higher-order @Show@ classes.
showb42Prec :: Int -> a -> Builder
showb42Prec p _ = showbPrec p $ Just (42 :: Int)

-- | Verifies that a type's 'TextShow' instance coincides with the output produced
-- by the equivalent 'Generic' functions.
prop_genericTextShow :: (TextShow a, Generic a, GTextShow (Rep a))
                     => Int -> a -> Bool
prop_genericTextShow p x = showbPrec p x == genericShowbPrec p x

-- | Verifies that a type's 'TextShow1' instance coincides with the output produced
-- by the equivalent 'Generic1' functions.
prop_genericTextShow1 :: (TextShow1 f, Generic1 f, GTextShow1 (Rep1 f))
                      => Int -> f a -> Bool
prop_genericTextShow1 p x = showbPrecWith showb27Prec p x
                        == genericShowbPrecWith showb27Prec p x
