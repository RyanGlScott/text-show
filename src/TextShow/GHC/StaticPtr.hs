{-# LANGUAGE CPP             #-}

#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      TextShow.GHC.StaticPtr
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'StaticPtrInfo'.
Only provided if using @base-4.8.0.0@ or later.

/Since: 2/
-}
module TextShow.GHC.StaticPtr () where

#if MIN_VERSION_base(4,8,0)
import GHC.StaticPtr (StaticPtrInfo)

import TextShow.Data.Char     ()
import TextShow.Data.Integral ()
import TextShow.Data.List     ()
import TextShow.Data.Tuple    ()
import TextShow.TH.Internal (deriveTextShow)

-- | /Since: 2/
$(deriveTextShow ''StaticPtrInfo)
#endif
