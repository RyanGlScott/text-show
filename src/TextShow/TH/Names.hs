{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module:      TextShow.TH.Names
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Template Haskell names to eliminate some boilerplate.
-}
module TextShow.TH.Names (
#if MIN_VERSION_base(4,4,0)
    evtCloseValName,
    eventIsValName,
    fdKeyTypeName,
    uniqueTypeName,
    asInt64ValName,
#endif
#if MIN_VERSION_base(4,6,0)
    numberTypeName,
#endif
#if MIN_VERSION_base(4,8,0)
    giveGCStatsTypeName,
    doCostCentresTypeName,
    doHeapProfileTypeName,
    doTraceTypeName,
#endif
    ) where

import Language.Haskell.TH.Syntax

#if MIN_VERSION_base(4,7,0)
import Text.Read.Lex (Number)
#endif

#if __GLASGOW_HASKELL__ >= 711
import GHC.RTS.Flags (GiveGCStats, DoCostCentres, DoHeapProfile, DoTrace)
#endif

-------------------------------------------------------------------------------

#if MIN_VERSION_base(4,4,0)
mkEventName_v :: String -> Name
mkEventName_v = mkNameG_v "base" "GHC.Event.Internal"

evtCloseValName :: Name
evtCloseValName = mkEventName_v "evtClose"

eventIsValName :: Name
eventIsValName = mkEventName_v "eventIs"

fdKeyTypeName :: Name
fdKeyTypeName = mkNameG_tc "base" "GHC.Event.Manager" "FdKey"

uniqueTypeName :: Name
uniqueTypeName = mkNameG_tc "base" "GHC.Event.Unique" "Unique"

asInt64ValName :: Name
asInt64ValName = mkNameG_v "base" "GHC.Event.Unique" "asInt64"
#endif

#if MIN_VERSION_base(4,6,0)
numberTypeName :: Name
# if MIN_VERSION_base(4,7,0)
numberTypeName = ''Number
# else
numberTypeName = mkNameG_tc "base" "Text.Read.Lex" "Number"
# endif
#endif

#if MIN_VERSION_base(4,8,0)
giveGCStatsTypeName :: Name
# if __GLASGOW_HASKELL__ >= 711
giveGCStatsTypeName = ''GiveGCStats
# else
giveGCStatsTypeName = mkFlagsName_tc "GiveGCStats"
# endif

doCostCentresTypeName :: Name
# if __GLASGOW_HASKELL__ >= 711
doCostCentresTypeName = ''DoCostCentres
# else
doCostCentresTypeName = mkFlagsName_tc "DoCostCentres"
# endif

doHeapProfileTypeName :: Name
# if __GLASGOW_HASKELL__ >= 711
doHeapProfileTypeName = ''DoHeapProfile
# else
doHeapProfileTypeName = mkFlagsName_tc "DoHeapProfile"
# endif

doTraceTypeName :: Name
# if __GLASGOW_HASKELL__ >= 711
doTraceTypeName = ''DoTrace
# else
doTraceTypeName = mkFlagsName_tc "DoTrace"
# endif

# if __GLASGOW_HASKELL__ < 711
mkFlagsName_tc :: String -> Name
mkFlagsName_tc = mkNameG_tc "base" "GHC.RTS.Flags"
# endif
#endif
