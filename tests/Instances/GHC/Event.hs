{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.GHC.Event
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

'Arbitrary' instances for data types in the "GHC.Event" module.
-}
module Instances.GHC.Event () where

#if !defined(__GHCJS__) && !defined(mingw32_HOST_OS) && MIN_VERSION_base(4,4,0)
import GHC.Event (Event, evtRead, evtWrite)

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary Event where
    arbitrary = oneof $ map pure [evtRead, evtWrite]

-- TODO: instance Arbitrary FdKey
#endif
