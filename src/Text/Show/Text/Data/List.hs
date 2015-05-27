{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.List
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Exports 'showbListWith'.
-}
module Text.Show.Text.Data.List (showbListWith) where

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb, showbList), showbListWith)
import Text.Show.Text.Data.Char ()
import Text.Show.Text.Data.Integral ()

#include "inline.h"

instance Show a => Show [a] where
    {-# SPECIALIZE instance Show [String] #-}
    {-# SPECIALIZE instance Show String   #-}
    {-# SPECIALIZE instance Show [Int]    #-}
    showb = showbList
    INLINE_INST_FUN(showb)

-- instance Show1 [] where
--     showbPrec1 = showbPrec
--     INLINE_INST_FUN(showbPrec1)
