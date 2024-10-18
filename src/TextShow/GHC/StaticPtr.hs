{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.GHC.StaticPtr
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'StaticPtrInfo'.

/Since: 2/
-}
module TextShow.GHC.StaticPtr () where

import GHC.StaticPtr (StaticPtrInfo)

import TextShow.Data.Char     ()
import TextShow.Data.Integral ()
import TextShow.Data.List     ()
import TextShow.Data.Tuple    ()
import TextShow.TH.Internal (deriveTextShow)

-- | /Since: 2/
$(deriveTextShow ''StaticPtrInfo)
