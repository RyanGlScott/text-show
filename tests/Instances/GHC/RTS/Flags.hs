{-# LANGUAGE CPP             #-}

#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      Instances.GHC.RTS.Flags
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "GHC.RTS.Flags" module.
-}
module Instances.GHC.RTS.Flags (
#if !(MIN_VERSION_base(4,8,0))
    ) where
#else
      GiveGCStats'
    , DoCostCentres'
    , DoHeapProfile'
    , DoTrace'
    ) where

import qualified Generics.Deriving.TH as Generics (deriveAll0)
import           GHC.RTS.Flags
import           Instances.Utils.GenericArbitrary (genericArbitrary)
import           Language.Haskell.TH.Lib (conT)
import           Test.QuickCheck (Arbitrary(..))
import           TextShow.TH.Names

instance Arbitrary RTSFlags where
    arbitrary = genericArbitrary

instance Arbitrary GCFlags where
    arbitrary = genericArbitrary

instance Arbitrary ConcFlags where
    arbitrary = genericArbitrary

instance Arbitrary MiscFlags where
    arbitrary = genericArbitrary

instance Arbitrary DebugFlags where
    arbitrary = genericArbitrary

instance Arbitrary CCFlags where
    arbitrary = genericArbitrary

instance Arbitrary ProfFlags where
    arbitrary = genericArbitrary

instance Arbitrary TraceFlags where
    arbitrary = genericArbitrary

instance Arbitrary TickyFlags where
    arbitrary = genericArbitrary

# if MIN_VERSION_base(4,10,0)
instance Arbitrary ParFlags where
    arbitrary = genericArbitrary
# endif

type GiveGCStats'   = $(conT giveGCStatsTypeName)
type DoCostCentres' = $(conT doCostCentresTypeName)
type DoHeapProfile' = $(conT doHeapProfileTypeName)
type DoTrace'       = $(conT doTraceTypeName)

-- TODO: Perhaps we should add deriving (Enum, Bounded) to deriving-compat
-- and use that instead of Generic?
instance Arbitrary GiveGCStats' where
    arbitrary = genericArbitrary

instance Arbitrary DoCostCentres' where
    arbitrary = genericArbitrary

instance Arbitrary DoHeapProfile' where
    arbitrary = genericArbitrary

instance Arbitrary DoTrace' where
    arbitrary = genericArbitrary

$(Generics.deriveAll0 ''RTSFlags)
$(Generics.deriveAll0 ''GCFlags)
$(Generics.deriveAll0 ''ConcFlags)
$(Generics.deriveAll0 ''MiscFlags)
$(Generics.deriveAll0 ''DebugFlags)
$(Generics.deriveAll0 ''CCFlags)
$(Generics.deriveAll0 ''ProfFlags)
$(Generics.deriveAll0 ''TraceFlags)
$(Generics.deriveAll0 ''TickyFlags)
# if MIN_VERSION_base(4,10,0)
$(Generics.deriveAll0 ''ParFlags)
# endif

$(Generics.deriveAll0 giveGCStatsTypeName)
$(Generics.deriveAll0 doCostCentresTypeName)
$(Generics.deriveAll0 doHeapProfileTypeName)
$(Generics.deriveAll0 doTraceTypeName)
#endif
