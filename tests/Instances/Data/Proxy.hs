{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Proxy
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

'Arbitrary' instances for 'Proxy'.
-}
module Instances.Data.Proxy () where

import Data.Proxy (Proxy(..))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

instance Arbitrary (Proxy s) where
    arbitrary = pure Proxy
