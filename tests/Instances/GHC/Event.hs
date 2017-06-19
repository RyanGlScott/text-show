{-# LANGUAGE CPP                #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

#if !defined(__GHCJS__) && !defined(mingw32_HOST_OS)
import GHC.Event (Event, evtRead, evtWrite)

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), oneof)

# if MIN_VERSION_base(4,8,1)
import GHC.Event (Lifetime(..))
import Test.QuickCheck (arbitraryBoundedEnum)
# endif

instance Arbitrary Event where
    arbitrary = oneof $ map pure [evtRead, evtWrite]

-- TODO: instance Arbitrary FdKey

# if MIN_VERSION_base(4,8,1)
deriving instance Bounded Lifetime
deriving instance Enum Lifetime
instance Arbitrary Lifetime where
    arbitrary = arbitraryBoundedEnum
# endif
#endif
