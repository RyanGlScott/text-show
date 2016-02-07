{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.Data.Functor.Product
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'Product'.

/Since: 3/
-}
module TextShow.Data.Functor.Product (liftShowbProductPrec) where

import Data.Functor.Product (Product(..))
import Data.Text.Lazy.Builder (Builder)

import TextShow.Classes (TextShow(..), TextShow1(..), showbPrec1)
import TextShow.TH.Internal (deriveTextShow1)

#include "inline.h"

-- | Convert a 'Product' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 3/
liftShowbProductPrec :: (TextShow1 f, TextShow1 g)
                     => (Int -> a -> Builder) -> ([a] -> Builder)
                     -> Int -> Product f g a -> Builder
liftShowbProductPrec = liftShowbPrec
{-# INLINE liftShowbProductPrec #-}

instance (TextShow1 f, TextShow1 g, TextShow a) => TextShow (Product f g a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

$(deriveTextShow1 ''Product)
