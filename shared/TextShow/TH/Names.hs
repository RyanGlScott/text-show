{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

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
#if MIN_VERSION_base(4,8,0)
    giveGCStatsTypeName,
    doCostCentresTypeName,
    doHeapProfileTypeName,
    doTraceTypeName,
#endif
    ) where

import Language.Haskell.TH.Syntax

#if MIN_VERSION_base(4,8,2)
import GHC.RTS.Flags (GiveGCStats, DoCostCentres, DoHeapProfile, DoTrace)
#endif

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

#if MIN_VERSION_base(4,8,0)
-- | The 'Name' of 'GiveGCStats'.
giveGCStatsTypeName :: Name
# if MIN_VERSION_base(4,8,2)
giveGCStatsTypeName = ''GiveGCStats
# else
giveGCStatsTypeName = mkFlagsName_tc "GiveGCStats"
# endif

-- | The 'Name' of 'DoCostCentres'.
doCostCentresTypeName :: Name
# if MIN_VERSION_base(4,8,2)
doCostCentresTypeName = ''DoCostCentres
# else
doCostCentresTypeName = mkFlagsName_tc "DoCostCentres"
# endif

-- | The 'Name' of 'DoHeapProfile'.
doHeapProfileTypeName :: Name
# if MIN_VERSION_base(4,8,2)
doHeapProfileTypeName = ''DoHeapProfile
# else
doHeapProfileTypeName = mkFlagsName_tc "DoHeapProfile"
# endif

-- | The 'Name' of 'DoTrace'.
doTraceTypeName :: Name
# if MIN_VERSION_base(4,8,2)
doTraceTypeName = ''DoTrace
# else
doTraceTypeName = mkFlagsName_tc "DoTrace"
# endif

-- | Creates a 'Name' for a type from the "GHC.RTS.Flags" module.
# if !(MIN_VERSION_base(4,8,2))
mkFlagsName_tc :: String -> Name
mkFlagsName_tc = mkNameG_tc "base" "GHC.RTS.Flags"
# endif
#endif
