{-# LANGUAGE CPP #-}

#if !(MIN_VERSION_QuickCheck(2,9,0))
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      Instances.Data.Version
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'Version'.
-}
module Instances.Data.Version () where

#if !(MIN_VERSION_QuickCheck(2,9,0))
import Data.Version (Version(..))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

instance Arbitrary Version where
    arbitrary = Version <$> arbitrary <*> arbitrary
#endif
