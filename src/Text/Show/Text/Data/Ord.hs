{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Show.Text.Data.Ord (showbOrdering, showbDown) where

import Data.Monoid ((<>))
import Data.Ord (Down(..))
import Data.Text.Lazy.Builder (Builder)

import GHC.Show (appPrec, appPrec1)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showb, showbPrec), showbParen)

-- | Convert a 'Ordering' to a 'Builder'.
showbOrdering :: Ordering -> Builder
showbOrdering LT = "LT"
showbOrdering EQ = "EQ"
showbOrdering GT = "GT"
{-# INLINE showbOrdering #-}

-- | Convert a 'Down' value to a 'Builder' with the given precedence.
showbDown :: Show a => Int -> Down a -> Builder
showbDown p (Down d) = showbParen (p > appPrec) $ "Down " <> showbPrec appPrec1 d
{-# INLINE showbDown #-}

instance Show Ordering where
    showb = showbOrdering
    {-# INLINE showb #-}

instance Show a => Show (Down a) where
    showbPrec = showbDown
    {-# INLINE showbPrec #-}