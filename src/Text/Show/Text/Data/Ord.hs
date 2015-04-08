{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Ord
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for 'Ordering' and 'Down'.

/Since: 0.3/
-}
module Text.Show.Text.Data.Ord (
      showbOrdering
    , showbDownPrec
    ) where

import Data.Ord.Compat (Down)
import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb, showbPrec), Show1(showbPrec1))
import Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowb,
                                   defaultInlineShowbPrec)

#include "inline.h"

-- | Convert a 'Ordering' to a 'Builder'.
-- 
-- /Since: 0.3/
showbOrdering :: Ordering -> Builder
showbOrdering = showb
{-# INLINE showbOrdering #-}

-- | Convert a 'Down' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.8/
showbDownPrec :: Show a => Int -> Down a -> Builder
showbDownPrec = showbPrec
{-# INLINE showbDownPrec #-}

$(deriveShowPragmas defaultInlineShowb ''Ordering)

$(deriveShowPragmas defaultInlineShowbPrec ''Down)

instance Show1 Down where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}
