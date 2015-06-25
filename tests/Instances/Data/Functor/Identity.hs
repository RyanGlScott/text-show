{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Functor.Identity
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'Identity'.
-}
module Instances.Data.Functor.Identity () where

import Data.Functor.Identity (Identity(..))
import Test.QuickCheck (Arbitrary)

deriving instance Arbitrary a => Arbitrary (Identity a)
