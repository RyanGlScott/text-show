{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Ord
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'Down'.
-}
module Instances.Data.Ord () where

import GHC.Exts (Down(..))
import Test.QuickCheck (Arbitrary)

deriving instance Arbitrary a => Arbitrary (Down a)
