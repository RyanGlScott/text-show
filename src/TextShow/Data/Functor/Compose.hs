{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.Data.Functor.Compose
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'Compose'.

/Since: 3/
-}
module TextShow.Data.Functor.Compose (liftShowbComposePrec) where

import Data.Functor.Compose (Compose(..))
import Data.Text.Lazy.Builder (Builder)

import TextShow.Classes (TextShow(..), TextShow1(..), showbPrec1, showbUnaryWith)

#include "inline.h"

-- | Convert a 'Compose' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 3/
liftShowbComposePrec :: (TextShow1 f, TextShow1 g)
                     => (Int -> a -> Builder) -> ([a] -> Builder)
                     -> Int -> Compose f g a -> Builder
liftShowbComposePrec sp sl p (Compose x) =
    showbUnaryWith (liftShowbPrec (liftShowbPrec sp sl)
                                  (liftShowbList sp sl))
                   "Compose" p x

instance (TextShow1 f, TextShow1 g, TextShow a) => TextShow (Compose f g a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

instance (TextShow1 f, TextShow1 g) => TextShow1 (Compose f g) where
    liftShowbPrec = liftShowbComposePrec
    INLINE_INST_FUN(liftShowbPrec)
