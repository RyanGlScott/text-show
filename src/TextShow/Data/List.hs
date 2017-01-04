{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.List
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Exports 'showbListWith', 'showtListWith', and 'showtlListWith'.
-}
module TextShow.Data.List (showbListWith, showtListWith, showtlListWith) where

import TextShow.Classes (TextShow(..), TextShow1(..), showbListWith, showtListWith, showtlListWith)
import TextShow.Data.Char ()
import TextShow.Data.Integral ()

#include "inline.h"

instance TextShow a => TextShow [a] where
    {-# SPECIALIZE instance TextShow [String] #-}
    {-# SPECIALIZE instance TextShow String   #-}
    {-# SPECIALIZE instance TextShow [Int]    #-}
    showb = showbList
    INLINE_INST_FUN(showb)

instance TextShow1 [] where
    liftShowbPrec _ sl _ = sl
    INLINE_INST_FUN(liftShowbPrec)
