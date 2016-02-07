{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Control.Applicative
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "Control.Applicative" module.
-}
module Instances.Control.Applicative () where

import Control.Applicative (Const(..), ZipList(..))
import Test.QuickCheck (Arbitrary)

deriving instance Arbitrary a => Arbitrary (Const a b)
deriving instance Arbitrary a => Arbitrary (ZipList a)
