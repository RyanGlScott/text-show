{-# LANGUAGE CPP             #-}

#if !(MIN_VERSION_base(4,11,0))
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}
#endif

{-|
Module:      TextShow.GHC.Stats
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'GCStats'.

/Since: 2/
-}
module TextShow.GHC.Stats () where

#if !(MIN_VERSION_base(4,11,0))
import GHC.Stats (GCStats)

import TextShow.Data.Integral ()
import TextShow.Data.Floating ()
import TextShow.TH.Internal (deriveTextShow)

-- /Since: 2/
$(deriveTextShow ''GCStats)
#endif
