{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Either
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'Either' values.

/Since: 2/
-}
module TextShow.Data.Either (showbEitherPrecWith2) where

import Data.Text.Lazy.Builder (Builder)

import TextShow.Classes (showbPrecWith2)
import TextShow.TH.Internal (deriveTextShow, deriveTextShow1, deriveTextShow2)

#include "inline.h"

-- | Convert a 'Either' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 2/
showbEitherPrecWith2 :: (Int -> a -> Builder) -> (Int -> b -> Builder)
                     -> Int -> Either a b -> Builder
showbEitherPrecWith2 = showbPrecWith2
{-# INLINE showbEitherPrecWith2 #-}

$(deriveTextShow  ''Either)
$(deriveTextShow1 ''Either)
$(deriveTextShow2 ''Either)
