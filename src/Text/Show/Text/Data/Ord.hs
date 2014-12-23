{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Ord
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for 'Ordering' and 'Down'.
-}
module Text.Show.Text.Data.Ord (
      showbOrdering
#if MIN_VERSION_base(4,6,0)
    , showbDownPrec
#endif
    ) where

import Data.Text.Lazy.Builder (Builder)

import Text.Show.Text.Classes (showb)
import Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowb)

#if MIN_VERSION_base(4,6,0)
import Data.Ord (Down)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showbPrec), Show1(showbPrec1))
import Text.Show.Text.TH.Internal (defaultInlineShowbPrec)
#endif

-- | Convert a 'Ordering' to a 'Builder'.
showbOrdering :: Ordering -> Builder
showbOrdering = showb
{-# INLINE showbOrdering #-}

$(deriveShowPragmas defaultInlineShowb ''Ordering)

#if MIN_VERSION_base(4,6,0)
-- | Convert a 'Down' value to a 'Builder' with the given precedence.
showbDownPrec :: Show a => Int -> Down a -> Builder
showbDownPrec = showbPrec
{-# INLINE showbDownPrec #-}

$(deriveShowPragmas defaultInlineShowbPrec ''Down)

instance Show1 Down where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}
#endif