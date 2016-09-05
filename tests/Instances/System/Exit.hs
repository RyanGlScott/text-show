{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.System.Exit
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'ExitCode'.
-}
module Instances.System.Exit () where

import Data.Orphans ()
import Generics.Deriving.Base ()
import System.Exit (ExitCode(..))
import Test.QuickCheck (Arbitrary(..), genericArbitrary)

instance Arbitrary ExitCode where
    arbitrary = genericArbitrary
