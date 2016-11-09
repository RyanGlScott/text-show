{-# LANGUAGE CPP                #-}

#if MIN_VERSION_base(4,5,0)
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      Instances.GHC.Stats
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'GCStats'.
-}
module Instances.GHC.Stats () where

#if MIN_VERSION_base(4,5,0)
import GHC.Generics (Generic)
import GHC.Stats (GCStats(..))

import Test.QuickCheck (Arbitrary(..), genericArbitrary)

deriving instance Generic GCStats
instance Arbitrary GCStats where
    arbitrary = genericArbitrary
#endif
