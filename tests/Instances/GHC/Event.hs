{-# LANGUAGE CPP                #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module:      Instances.GHC.Event
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "GHC.Event" module.
-}
module Instances.GHC.Event () where

#if !defined(__GHCJS__) && !defined(ghcjs_HOST_OS) && !defined(mingw32_HOST_OS)
import GHC.Event (Event, Lifetime(..), evtRead, evtWrite)

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, oneof)

instance Arbitrary Event where
    arbitrary = oneof $ map pure [evtRead, evtWrite]

-- TODO: instance Arbitrary FdKey

deriving instance Bounded Lifetime
deriving instance Enum Lifetime
instance Arbitrary Lifetime where
    arbitrary = arbitraryBoundedEnum
#endif
