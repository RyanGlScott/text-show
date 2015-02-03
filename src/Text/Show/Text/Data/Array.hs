{-# LANGUAGE CPP, FlexibleContexts, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Array
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Array' values.

/Since: 0.3/
-}
module Text.Show.Text.Data.Array (
      showbArrayPrec
    , showbUArrayPrec
    , showbIArrayPrec
    ) where

import qualified Data.Array as Array (assocs, bounds)
import           Data.Array (Array)
import qualified Data.Array.Base as IArray (assocs, bounds)
import           Data.Array.Base (IArray)
import           Data.Array.Unboxed (UArray)
import           Data.Ix (Ix)
import           Data.Text.Lazy.Builder (Builder)

import           GHC.Show (appPrec)

import           Prelude hiding (Show)

import           Text.Show.Text.Classes (Show(showb, showbPrec), showbParen, showbSpace)
import           Text.Show.Text.Data.List ()
import           Text.Show.Text.Data.Tuple ()
import           Text.Show.Text.Utils ((<>))

#include "inline.h"

-- | Convert an 'Array' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbArrayPrec :: (Show i, Show e, Ix i) => Int -> Array i e -> Builder
showbArrayPrec p a = showbParen (p > appPrec) $
       "array "
    <> showb (Array.bounds a)
    <> showbSpace
    <> showb (Array.assocs a)
{-# INLINE showbArrayPrec #-}

-- | Convert a 'UArray' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.7/
showbUArrayPrec :: (IArray UArray e, Ix i, Show i, Show e) => Int -> UArray i e -> Builder
showbUArrayPrec = showbIArrayPrec
{-# INLINE showbUArrayPrec #-}

{-# SPECIALIZE
    showbIArrayPrec :: (IArray UArray e, Ix i, Show i, Show e) =>
                        Int -> UArray i e -> Builder
  #-}
-- | Convert an 'IArray' instance to a 'Builder' with the given precedence.
-- 
-- /Since: 0.7/
showbIArrayPrec :: (IArray a e, Ix i, Show i, Show e) => Int -> a i e -> Builder
showbIArrayPrec p a = showbParen (p > 9) $
       "array "
    <> showb (IArray.bounds a)
    <> showbSpace
    <> showb (IArray.assocs a)

instance (Show i, Show e, Ix i) => Show (Array i e) where
    showbPrec = showbArrayPrec
    INLINE_INST_FUN(showbPrec)

instance (IArray UArray e, Ix i, Show i, Show e) => Show (UArray i e) where
    showbPrec = showbUArrayPrec
    INLINE_INST_FUN(showbPrec)