{-# LANGUAGE CPP                   #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{-|
Module:      TextShow.TH.Names
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Template Haskell names to eliminate some boilerplate.
-}
module TextShow.TH.Names (
    evtCloseValName,
    eventIsValName,
    fdKeyTypeName,
    uniqueTypeName,
    asInt64ValName,
    giveGCStatsTypeName,
    doCostCentresTypeName,
    doHeapProfileTypeName,
    doTraceTypeName,
    ) where

import GHC.RTS.Flags (GiveGCStats, DoCostCentres, DoHeapProfile, DoTrace)
import Language.Haskell.TH.Syntax

-------------------------------------------------------------------------------

-- | Creates a 'Name' for a value from the "GHC.Event.Internal" module.
mkEventName_v :: String -> Name
#if MIN_VERSION_base(4,20,0)
mkEventName_v = mkNameG_v "ghc-internal" "GHC.Internal.Event.Internal.Types"
#elif MIN_VERSION_base(4,15,0)
mkEventName_v = mkNameG_v "base" "GHC.Event.Internal.Types"
#else
mkEventName_v = mkNameG_v "base" "GHC.Event.Internal"
#endif

-- | The 'Name' of 'evtClose'.
evtCloseValName :: Name
evtCloseValName = mkEventName_v "evtClose"

-- | The 'Name' of 'eventIs'.
eventIsValName :: Name
eventIsValName = mkEventName_v "eventIs"

-- | The 'Name' of 'FdKey'.
fdKeyTypeName :: Name
#if MIN_VERSION_base(4,20,0)
fdKeyTypeName = mkNameG_tc "ghc-internal" "GHC.Internal.Event.Manager" "FdKey"
#else
fdKeyTypeName = mkNameG_tc "base" "GHC.Event.Manager" "FdKey"
#endif

-- | The 'Name' of 'Unique'.
uniqueTypeName :: Name
#if MIN_VERSION_base(4,20,0)
uniqueTypeName = mkNameG_tc "ghc-internal" "GHC.Internal.Event.Unique" "Unique"
#else
uniqueTypeName = mkNameG_tc "base" "GHC.Event.Unique" "Unique"
#endif

-- | The 'Name' of 'asInt64' (or, 'asInt' on @base-4.10.0.0@ or later).
asInt64ValName :: Name
#if MIN_VERSION_base(4,20,0)
asInt64ValName = mkNameG_fld "ghc-internal" "GHC.Internal.Event.Unique" "Unique" "asInt"
#elif MIN_VERSION_base(4,19,0)
asInt64ValName = mkNameG_fld "base" "GHC.Event.Unique" "Unique" "asInt"
#elif MIN_VERSION_base(4,10,0)
asInt64ValName = mkNameG_v "base" "GHC.Event.Unique" "asInt"
#else
asInt64ValName = mkNameG_v "base" "GHC.Event.Unique" "asInt64"
#endif

-- | The 'Name' of 'GiveGCStats'.
giveGCStatsTypeName :: Name
giveGCStatsTypeName = ''GiveGCStats

-- | The 'Name' of 'DoCostCentres'.
doCostCentresTypeName :: Name
doCostCentresTypeName = ''DoCostCentres

-- | The 'Name' of 'DoHeapProfile'.
doHeapProfileTypeName :: Name
doHeapProfileTypeName = ''DoHeapProfile

-- | The 'Name' of 'DoTrace'.
doTraceTypeName :: Name
doTraceTypeName = ''DoTrace
