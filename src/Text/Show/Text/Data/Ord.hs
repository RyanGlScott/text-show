{-# LANGUAGE CPP, TemplateHaskell #-}
#if MIN_VERSION_base(4,6,0)
{-# LANGUAGE NoImplicitPrelude #-}
#endif
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

import Text.Show.Text.Class (showb)
import Text.Show.Text.TH.Internal (deriveShow)

#if MIN_VERSION_base(4,6,0)
import Data.Ord (Down)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showbPrec))
#endif

-- | Convert a 'Ordering' to a 'Builder'.
showbOrdering :: Ordering -> Builder
showbOrdering = showb
{-# INLINE showbOrdering #-}

$(deriveShow ''Ordering)

#if MIN_VERSION_base(4,6,0)
-- | Convert a 'Down' value to a 'Builder' with the given precedence.
showbDownPrec :: Show a => Int -> Down a -> Builder
showbDownPrec = showbPrec
{-# INLINE showbDownPrec #-}

$(deriveShow ''Down)
#endif