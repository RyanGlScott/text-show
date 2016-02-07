{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module:      Spec.Utils
Copyright:   (C) 2014-2016 Ryan Scott
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
#if !(MIN_VERSION_transformers(0,4,0)) || MIN_VERSION_transformers(0,5,0)
    , prop_matchesTextShow2
#endif
    , prop_genericTextShow
    , prop_genericTextShow1
    ) where

import           Data.Functor.Classes (Show1, showsPrec1)

import           Generics.Deriving.Base

#if MIN_VERSION_QuickCheck(2,7,0)
import qualified Test.QuickCheck as QC (ioProperty)
#else
import           Test.QuickCheck (morallyDubiousIOProperty)
#endif
import           Test.QuickCheck (Property, Testable)

import           TextShow (TextShow(..), TextShow1(..), showbPrec1, fromString)
import           TextShow.Generic

#if !(MIN_VERSION_transformers(0,4,0)) || MIN_VERSION_transformers(0,5,0)
import           Data.Functor.Classes (Show2, showsPrec2)
import           TextShow (TextShow2(..), showbPrec2)
#endif

ioProperty :: Testable prop => IO prop -> Property
#if MIN_VERSION_QuickCheck(2,7,0)
ioProperty = QC.ioProperty
#else
ioProperty = morallyDubiousIOProperty
#endif

-- | Verifies that a type's @Show@ instances coincide for both 'String's and 'Text',
-- irrespective of precedence.
prop_matchesTextShow :: (Show a, TextShow a) => Int -> a -> Bool
prop_matchesTextShow p x = fromString (showsPrec p x "") == showbPrec p x

-- | Verifies that a type's @Show1@ instances coincide for both 'String's and 'Text',
-- irrespective of precedence.
prop_matchesTextShow1 :: (Show1 f, Show a, TextShow1 f, TextShow a) => Int -> f a -> Bool
prop_matchesTextShow1 p x = fromString (showsPrec1 p x "") == showbPrec1 p x

#if !(MIN_VERSION_transformers(0,4,0)) || MIN_VERSION_transformers(0,5,0)
-- | Verifies that a type's @Show2@ instances coincide for both 'String's and 'Text',
-- irrespective of precedence.
prop_matchesTextShow2 :: (Show2 f, Show a, Show b, TextShow2 f, TextShow a, TextShow b)
                      => Int -> f a b -> Bool
prop_matchesTextShow2 p x = fromString (showsPrec2 p x "") == showbPrec2 p x
#endif

-- | Verifies that a type's 'TextShow' instance coincides with the output produced
-- by the equivalent 'Generic' functions.
prop_genericTextShow :: (TextShow a, Generic a, GTextShow (Rep a))
                     => Int -> a -> Bool
prop_genericTextShow p x = showbPrec p x == genericShowbPrec p x

-- | Verifies that a type's 'TextShow1' instance coincides with the output produced
-- by the equivalent 'Generic1' functions.
prop_genericTextShow1 :: (TextShow1 f, Generic1 f, GTextShow1 (Rep1 f), TextShow a)
                      => Int -> f a -> Bool
prop_genericTextShow1 p x =
    showbPrec1 p x == genericLiftShowbPrec showbPrec showbList p x
