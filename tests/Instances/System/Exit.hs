{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.System.Exit
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'ExitCode'.
-}
module Instances.System.Exit () where

import Prelude ()
import Prelude.Compat

import System.Exit (ExitCode(..))

import Test.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary ExitCode where
    arbitrary = oneof [pure ExitSuccess, ExitFailure <$> arbitrary]
