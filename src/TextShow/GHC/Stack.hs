{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.GHC.Stack
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for 'CallStack' and 'SrcLoc' values.

/Since: 3.0.1/
-}
module TextShow.GHC.Stack () where

import GHC.Stack (CallStack, SrcLoc, getCallStack)

import TextShow.Classes (TextShow(..))
import TextShow.Data.Char     ()
import TextShow.Data.Integral ()
import TextShow.Data.List     ()
import TextShow.Data.Tuple    ()
import TextShow.TH.Internal (deriveTextShow)

-- | /Since: 3.0.1/
$(deriveTextShow ''SrcLoc)

-- | /Since: 3.0.1/
instance TextShow CallStack where
    showb = showb . getCallStack
    {-# INLINE showb #-}
