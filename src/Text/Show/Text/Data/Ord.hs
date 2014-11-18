{-# LANGUAGE CPP, NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Data.Ord
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' functions for 'Ordering' and 'Down'.
----------------------------------------------------------------------------
module Text.Show.Text.Data.Ord (
      showbOrdering
#if MIN_VERSION_base(4,6,0)
    , showbDownPrec
#endif
    ) where

import Data.Text.Lazy.Builder (Builder)
import Prelude hiding (Show)
import Text.Show.Text.Class (Show(showb))

#if MIN_VERSION_base(4,6,0)
import Data.Ord (Down(..))
import Data.Monoid ((<>))
import GHC.Show (appPrec, appPrec1)
import Text.Show.Text.Class (showbPrec, showbParen)
#endif

-- | Convert a 'Ordering' to a 'Builder'.
showbOrdering :: Ordering -> Builder
showbOrdering LT = "LT"
showbOrdering EQ = "EQ"
showbOrdering GT = "GT"
{-# INLINE showbOrdering #-}

instance Show Ordering where
    showb = showbOrdering
    {-# INLINE showb #-}

#if MIN_VERSION_base(4,6,0)
-- | Convert a 'Down' value to a 'Builder' with the given precedence.
showbDownPrec :: Show a => Int -> Down a -> Builder
showbDownPrec p (Down d) = showbParen (p > appPrec) $ "Down " <> showbPrec appPrec1 d
{-# INLINE showbDownPrec #-}

instance Show a => Show (Down a) where
    showbPrec = showbDownPrec
    {-# INLINE showbPrec #-}
#endif