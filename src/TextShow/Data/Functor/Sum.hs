{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.Data.Functor.Sum
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'Sum'.

/Since: 3/
-}
module TextShow.Data.Functor.Sum (liftShowbSumPrec) where

import Data.Functor.Sum (Sum(..))
import Data.Text.Lazy.Builder (Builder)

import TextShow.Classes (TextShow(..), TextShow1(..), showbPrec1)
import TextShow.TH.Internal (deriveTextShow1)

#include "inline.h"

-- | Convert a 'Sum' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 3/
liftShowbSumPrec :: (TextShow1 f, TextShow1 g)
                 => (Int -> a -> Builder) -> ([a] -> Builder)
                 -> Int -> Sum f g a -> Builder
liftShowbSumPrec = liftShowbPrec
{-# INLINE liftShowbSumPrec #-}

instance (TextShow1 f, TextShow1 g, TextShow a) => TextShow (Sum f g a) where
    showbPrec = showbPrec1
    INLINE_INST_FUN(showbPrec)

$(deriveTextShow1 ''Sum)
