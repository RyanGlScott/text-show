{-# LANGUAGE CPP               #-}

#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      TextShow.GHC.RTS.Flags
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the 'GHC.RTS.Flags' module.
Only provided if using @base-4.8.0.0@ or later.

/Since: 2/
-}
module TextShow.GHC.RTS.Flags () where

#if MIN_VERSION_base(4,8,0)
import GHC.RTS.Flags

import TextShow.Data.Bool     ()
import TextShow.Data.Char     ()
import TextShow.Data.Floating ()
import TextShow.Data.Integral ()
import TextShow.Data.List     ()
import TextShow.Data.Maybe    ()
import TextShow.TH.Internal (deriveTextShow)
import TextShow.TH.Names (giveGCStatsTypeName, doCostCentresTypeName,
                          doHeapProfileTypeName, doTraceTypeName)

-- | /Since: 2.1/
$(deriveTextShow giveGCStatsTypeName)
-- | /Since: 2.1/
$(deriveTextShow doCostCentresTypeName)
-- | /Since: 2.1/
$(deriveTextShow doHeapProfileTypeName)
-- | /Since: 2.1/
$(deriveTextShow doTraceTypeName)

-- | /Since: 2/
$(deriveTextShow ''GCFlags)
-- | /Since: 2/
$(deriveTextShow ''ConcFlags)
-- | /Since: 2/
$(deriveTextShow ''MiscFlags)
-- | /Since: 2/
$(deriveTextShow ''DebugFlags)
-- | /Since: 2/
$(deriveTextShow ''CCFlags)
-- | /Since: 2/
$(deriveTextShow ''ProfFlags)
-- | /Since: 2/
$(deriveTextShow ''TraceFlags)
-- | /Since: 2/
$(deriveTextShow ''TickyFlags)
# if MIN_VERSION_base(4,10,0)
-- | Only available with @base-4.10.0.0@ or later.
--
-- /Since: 3.3/
$(deriveTextShow ''ParFlags)
# endif
-- | /Since: 2/
$(deriveTextShow ''RTSFlags)
#endif
