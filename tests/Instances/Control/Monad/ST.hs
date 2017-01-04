{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Control.Monad.ST
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'ST'.
-}
module Instances.Control.Monad.ST () where

import Control.Monad.ST (ST, fixST)

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

instance Arbitrary (ST s a) where
    arbitrary = pure $ fixST undefined
