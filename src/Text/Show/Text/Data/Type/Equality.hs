{-# LANGUAGE CPP             #-}

#if MIN_VERSION_base(4,7,0)
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      Text.Show.Text.Data.Type.Equality
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for propositional equality.
This module only exports functions if using @base-4.7.0.0@ or later.

/Since: 0.3/
-}
module Text.Show.Text.Data.Type.Equality (
#if !(MIN_VERSION_base(4,7,0))
    ) where
#else
      showbPropEquality
    ) where

import Data.Text.Lazy.Builder (Builder)
import Data.Type.Equality ((:~:)(..))

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb, showbPrec), Show1(showbPrec1))
import Text.Show.Text.TH.Internal (mkShowbPrec)

-- | Convert a propositional equality value to a 'Builder'.
-- This function is only available with @base-4.7.0.0@ or later.
-- 
-- /Since: 0.3/
showbPropEquality :: (a :~: b) -> Builder
showbPropEquality = showb
{-# INLINE showbPropEquality #-}

-- TODO: Derive with TH once it can detect phantom types properly
instance Show (a :~: b) where
    showbPrec = $(mkShowbPrec ''(:~:))
    {-# INLINE showb #-}

instance Show1 ((:~:) a) where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}
#endif
