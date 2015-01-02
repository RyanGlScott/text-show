{-|
Module:      Properties.Utils
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ property-related utility functions.
-}
module Properties.Utils (prop_matchesShow) where

import           Prelude hiding (Show)

import           Test.Tasty.QuickCheck (Arbitrary)

import qualified Text.Show as S (Show)
import qualified Text.Show.Text as T (Show)
import           Text.Show.Text (showbPrec, FromStringShow(..))

-- | Verifies that a type's @Show@ instances coincide for both 'String's and 'Text',
-- irrespective of precedence.
prop_matchesShow :: (S.Show a, T.Show a, Arbitrary a) => Int -> a -> Bool
prop_matchesShow p x = showbPrec p (FromStringShow x) == showbPrec p x