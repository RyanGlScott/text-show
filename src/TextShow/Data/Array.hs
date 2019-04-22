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

Provides 'TextShow' instances for 'Array' types, as well as the
'showbIArrayPrec' function.

/Since: 2/
-}
module TextShow.Data.Array (showbIArrayPrec) where

import qualified Data.Array as Array (assocs, bounds)
import           Data.Array (Array)
import qualified Data.Array.Base as IArray (assocs, bounds)
import           Data.Array.Base (IArray)
import           Data.Array.Unboxed (UArray)
import           Data.Ix (Ix)
import           Data.Text.Lazy.Builder (Builder)

import           GHC.Show (appPrec)

import           Prelude ()
import           Prelude.Compat

import           TextShow.Classes (TextShow(..), showbParen, showbSpace)
import           TextShow.Data.List ()
import           TextShow.Data.Tuple ()

{-# SPECIALIZE
    showbIArrayPrec :: (IArray UArray e, Ix i, TextShow i, TextShow e) =>
                        Int -> UArray i e -> Builder
  #-}
-- | Convert an 'IArray' instance to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbIArrayPrec :: (IArray a e, Ix i, TextShow i, TextShow e) => Int -> a i e -> Builder
showbIArrayPrec p a = showbParen (p > arrayPrec) $
       "array "
    <> showb (IArray.bounds a)
    <> showbSpace
    <> showb (IArray.assocs a)
  where
    arrayPrec :: Int
#if MIN_VERSION_base(4,13,0)
    arrayPrec = appPrec
#else
    arrayPrec = 9
#endif

-- | /Since: 2/
instance (TextShow i, TextShow e, Ix i) => TextShow (Array i e) where
    showbPrec p a = showbParen (p > appPrec) $
           "array "
        <> showb (Array.bounds a)
        <> showbSpace
        <> showb (Array.assocs a)
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance (IArray UArray e, Ix i, TextShow i, TextShow e) => TextShow (UArray i e) where
    showbPrec = showbIArrayPrec
    {-# INLINE showbPrec #-}
