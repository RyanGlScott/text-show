{-# LANGUAGE CPP                #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.GHC.Conc.Windows
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'ConsoleEvent'.
-}
module Instances.GHC.Conc.Windows () where

#if !defined(__GHCJS__) && defined(mingw32_HOST_OS)
import GHC.Conc.Windows (ConsoleEvent(..))
import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

deriving instance Bounded ConsoleEvent
instance Arbitrary ConsoleEvent where
    arbitrary = arbitraryBoundedEnum
#endif
