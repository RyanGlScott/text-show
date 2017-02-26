{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.List
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Exports 'showbListWith', 'showtListWith', and 'showtlListWith',
and 'TextShow' instances for lists.
-}
module TextShow.Data.List (showbListWith, showtListWith, showtlListWith) where

import TextShow.Classes (TextShow(..), TextShow1(..), showbListWith, showtListWith, showtlListWith)
import TextShow.Data.Char ()
import TextShow.Data.Integral ()

-- | /Since: 2/
instance TextShow a => TextShow [a] where
    {-# SPECIALIZE instance TextShow [String] #-}
    {-# SPECIALIZE instance TextShow String   #-}
    {-# SPECIALIZE instance TextShow [Int]    #-}
    showb = showbList
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow1 [] where
    liftShowbPrec _ sl _ = sl
    {-# INLINE liftShowbPrec #-}
