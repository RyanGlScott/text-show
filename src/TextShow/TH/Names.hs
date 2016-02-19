{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module:      TextShow.TH.Names
Copyright:   (C) 2014-2016 Ryan Scott
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

#if MIN_VERSION_base(4,4,0)
import Language.Haskell.TH.Syntax
#endif

#if MIN_VERSION_base(4,7,0)
import Text.Read.Lex (Number)
#endif

#if __GLASGOW_HASKELL__ >= 711
import GHC.RTS.Flags (GiveGCStats, DoCostCentres, DoHeapProfile, DoTrace)
#endif

-------------------------------------------------------------------------------

#if MIN_VERSION_base(4,4,0)
-- | Creates a 'Name' for a value from the "GHC.Event.Internal" module.
mkEventName_v :: String -> Name
mkEventName_v = mkNameG_v "base" "GHC.Event.Internal"

-- | The 'Name' of 'evtClose'.
evtCloseValName :: Name
evtCloseValName = mkEventName_v "evtClose"

-- | The 'Name' of 'eventIs'.
eventIsValName :: Name
eventIsValName = mkEventName_v "eventIs"

-- | The 'Name' of 'FdKey'.
fdKeyTypeName :: Name
fdKeyTypeName = mkNameG_tc "base" "GHC.Event.Manager" "FdKey"

-- | The 'Name' of 'Unique'.
uniqueTypeName :: Name
uniqueTypeName = mkNameG_tc "base" "GHC.Event.Unique" "Unique"

-- | The 'Name' of 'asInt64'.
asInt64ValName :: Name
asInt64ValName = mkNameG_v "base" "GHC.Event.Unique" "asInt64"
#endif

#if MIN_VERSION_base(4,6,0)
-- | The 'Name' of 'Number'.
numberTypeName :: Name
# if MIN_VERSION_base(4,7,0)
numberTypeName = ''Number
# else
numberTypeName = mkNameG_tc "base" "Text.Read.Lex" "Number"
# endif
#endif

#if MIN_VERSION_base(4,8,0)
-- | The 'Name' of 'GiveGCStats'.
giveGCStatsTypeName :: Name
# if __GLASGOW_HASKELL__ >= 711
giveGCStatsTypeName = ''GiveGCStats
# else
giveGCStatsTypeName = mkFlagsName_tc "GiveGCStats"
# endif

-- | The 'Name' of 'DoCostCentres'.
doCostCentresTypeName :: Name
# if __GLASGOW_HASKELL__ >= 711
doCostCentresTypeName = ''DoCostCentres
# else
doCostCentresTypeName = mkFlagsName_tc "DoCostCentres"
# endif

-- | The 'Name' of 'DoHeapProfile'.
doHeapProfileTypeName :: Name
# if __GLASGOW_HASKELL__ >= 711
doHeapProfileTypeName = ''DoHeapProfile
# else
doHeapProfileTypeName = mkFlagsName_tc "DoHeapProfile"
# endif

-- | The 'Name' of 'DoTrace'.
doTraceTypeName :: Name
# if __GLASGOW_HASKELL__ >= 711
doTraceTypeName = ''DoTrace
# else
doTraceTypeName = mkFlagsName_tc "DoTrace"
# endif

-- | Creates a 'Name' for a type from the "GHC.RTS.Flags" module.
# if __GLASGOW_HASKELL__ < 711
mkFlagsName_tc :: String -> Name
mkFlagsName_tc = mkNameG_tc "base" "GHC.RTS.Flags"
# endif
#endif
