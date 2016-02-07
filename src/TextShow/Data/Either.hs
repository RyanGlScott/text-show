{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Either
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'Either' values.

/Since: 2/
-}
module TextShow.Data.Either (liftShowbEitherPrec2) where

import Data.Text.Lazy.Builder (Builder)

import TextShow.Classes (liftShowbPrec2)
import TextShow.TH.Internal (deriveTextShow, deriveTextShow1, deriveTextShow2)

#include "inline.h"

-- | Convert a 'Either' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 3/
liftShowbEitherPrec2 :: (Int -> a -> Builder) -> (Int -> b -> Builder)
                     -> Int -> Either a b -> Builder
liftShowbEitherPrec2 sp1 sp2 = liftShowbPrec2 sp1 undefined sp2 undefined
{-# INLINE liftShowbEitherPrec2 #-}

$(deriveTextShow  ''Either)
$(deriveTextShow1 ''Either)
$(deriveTextShow2 ''Either)
