{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Void
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'Void' values.

/Since: 2/
-}
module TextShow.Data.Void (showbVoid) where

import Data.Text.Lazy.Builder (Builder)
import Data.Void (Void, absurd)

import Prelude ()

import TextShow.Classes (TextShow(..))

-- | Since 'Void' values logically don't exist, attempting to convert one to a
-- 'Builder' will never terminate.
--
-- /Since: 2/
showbVoid :: Void -> Builder
showbVoid = absurd

instance TextShow Void where
    showb = showbVoid
