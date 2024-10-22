{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ == 802
-- See Note [Increased simpl-tick-factor on old GHCs] in TextShow.Data.Complex
{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}
#endif

{-|
Module:      Instances.GHC.RTS.Flags
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "GHC.RTS.Flags" module.
-}
module Instances.GHC.RTS.Flags
    ( GiveGCStats'
    , DoCostCentres'
    , DoHeapProfile'
    , DoTrace'
    ) where

import qualified Generics.Deriving.TH as Generics (deriveAll0)
import           GHC.RTS.Flags
#if MIN_VERSION_base(4,21,0)
import qualified GHC.IO.SubSystem as SubSystem
#elif MIN_VERSION_base(4,15,0)
import qualified GHC.RTS.Flags as SubSystem
#endif
import           Instances.Utils.GenericArbitrary (genericArbitrary)
import           Language.Haskell.TH.Lib (conT)
import           Test.QuickCheck (Arbitrary(..))
#if MIN_VERSION_base(4,21,0)
import           Test.QuickCheck (arbitraryBoundedEnum)
#endif
import           TextShow.TH.Names

#if !(MIN_VERSION_base(4,15,0))
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

#if MIN_VERSION_base(4,15,0)
$(Generics.deriveAll0 ''SubSystem.IoSubSystem)
#endif

instance Arbitrary RTSFlags where
    arbitrary = genericArbitrary

instance Arbitrary GCFlags where
    arbitrary = genericArbitrary

instance Arbitrary ConcFlags where
    arbitrary = genericArbitrary

#if MIN_VERSION_base(4,15,0)
instance Arbitrary SubSystem.IoSubSystem where
    arbitrary = genericArbitrary
#endif

#if MIN_VERSION_base(4,21,0)
deriving instance Bounded IoManagerFlag
instance Arbitrary IoManagerFlag where
    arbitrary = arbitraryBoundedEnum
#endif

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

#if MIN_VERSION_base(4,10,0)
instance Arbitrary ParFlags where
    arbitrary = genericArbitrary
#endif

#if MIN_VERSION_base(4,20,0)
instance Arbitrary HpcFlags where
    arbitrary = genericArbitrary
#endif

type GiveGCStats'   = $(conT giveGCStatsTypeName)
type DoCostCentres' = $(conT doCostCentresTypeName)
type DoHeapProfile' = $(conT doHeapProfileTypeName)
type DoTrace'       = $(conT doTraceTypeName)

instance Arbitrary GiveGCStats' where
    arbitrary = genericArbitrary

instance Arbitrary DoCostCentres' where
    arbitrary = genericArbitrary

instance Arbitrary DoHeapProfile' where
    arbitrary = genericArbitrary

instance Arbitrary DoTrace' where
    arbitrary = genericArbitrary
