{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE FlexibleContexts #-}
#endif
{-|
Module:      Properties.Utils
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ property-related utility functions.
-}
module Properties.Utils (prop_matchesShow, prop_genericShow) where

import           Prelude hiding (Show(..))

import           Test.Tasty.QuickCheck (Arbitrary)

import qualified Text.Show as S (Show)
import qualified Text.Show.Text as T (Show)
import           Text.Show.Text

#if __GLASGOW_HASKELL__ >= 702
import           GHC.Generics (Generic, Rep)
import           Text.Show.Text.Generic
#endif

-- | Verifies that a type's @Show@ instances coincide for both 'String's and 'Text',
-- irrespective of precedence.
prop_matchesShow :: (S.Show a, T.Show a, Arbitrary a) => Int -> a -> Bool
prop_matchesShow p x = showbPrec p (FromStringShow x) == showbPrec p x

#if __GLASGOW_HASKELL__ >= 702
-- | Verifies that a type's @Show@ instance coincides with the output produced
-- by the equivalent 'Generic' functions.
-- TODO: Add other generic functions
-- TODO: Put in tuples
prop_genericShow :: (S.Show a, T.Show a, Arbitrary a, Generic a, GShow (Rep a))
                 => Int -> a -> Bool
prop_genericShow p x = show             x == genericShow             x
                    && showLazy         x == genericShowLazy         x
                    && showPrec     p   x == genericShowPrec     p   x
                    && showPrecLazy p   x == genericShowPrecLazy p   x
                    && showList       [x] == genericShowList       [x]
                    && showListLazy   [x] == genericShowListLazy   [x]
                    && showb            x == genericShowb            x
                    && showbPrec    p   x == genericShowbPrec    p   x
                    && showbList      [x] == genericShowbList      [x]
#endif
