{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Array
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'Array' values.

/Since: 2/
-}
module TextShow.Data.Array (
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
import           Data.Monoid.Compat ((<>))
import           Data.Text.Lazy.Builder (Builder)

import           GHC.Show (appPrec)

import           Prelude ()
import           Prelude.Compat

import           TextShow.Classes (TextShow(..), showbParen, showbSpace)
import           TextShow.Data.List ()
import           TextShow.Data.Tuple ()

#include "inline.h"

-- | Convert an 'Array' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbArrayPrec :: (TextShow i, TextShow e, Ix i) => Int -> Array i e -> Builder
showbArrayPrec p a = showbParen (p > appPrec) $
       "array "
    <> showb (Array.bounds a)
    <> showbSpace
    <> showb (Array.assocs a)
{-# INLINE showbArrayPrec #-}

-- | Convert a 'UArray' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbUArrayPrec :: (IArray UArray e, Ix i, TextShow i, TextShow e) => Int -> UArray i e -> Builder
showbUArrayPrec = showbIArrayPrec
{-# INLINE showbUArrayPrec #-}

{-# SPECIALIZE
    showbIArrayPrec :: (IArray UArray e, Ix i, TextShow i, TextShow e) =>
                        Int -> UArray i e -> Builder
  #-}
-- | Convert an 'IArray' instance to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbIArrayPrec :: (IArray a e, Ix i, TextShow i, TextShow e) => Int -> a i e -> Builder
showbIArrayPrec p a = showbParen (p > 9) $
       "array "
    <> showb (IArray.bounds a)
    <> showbSpace
    <> showb (IArray.assocs a)

instance (TextShow i, TextShow e, Ix i) => TextShow (Array i e) where
    showbPrec = showbArrayPrec
    INLINE_INST_FUN(showbPrec)

instance (IArray UArray e, Ix i, TextShow i, TextShow e) => TextShow (UArray i e) where
    showbPrec = showbUArrayPrec
    INLINE_INST_FUN(showbPrec)
