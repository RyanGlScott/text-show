{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Vector
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC
-}
module TextShow.Data.Vector () where

import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Data.Text (Text)
import TextShow.Classes (TextShow(..), TextShow1(..))
import TextShow.Data.Char ()
import TextShow.Data.Integral ()
import TextShow.Data.Text ()

#include "inline.h"

instance TextShow a => TextShow (Vector a) where
    {-# SPECIALIZE instance TextShow (Vector Text) #-}
    {-# SPECIALIZE instance TextShow (Vector String) #-}
    {-# SPECIALIZE instance TextShow (Vector Int) #-}
    showb = showbList . Vector.toList
    INLINE_INST_FUN(showb)

instance TextShow1 Vector where
    liftShowbPrec _ sl _ = sl . Vector.toList
    INLINE_INST_FUN(liftShowbPrec)
