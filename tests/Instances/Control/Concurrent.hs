{-# LANGUAGE CPP                #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric      #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Control.Concurrent
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "Control.Concurrent" module.
-}
module Instances.Control.Concurrent () where

#if __GLASGOW_HASKELL__ >= 704
import           GHC.Generics (Generic)
#else
import qualified Generics.Deriving.TH as Generics (deriveAll0)
#endif

import           GHC.Conc (BlockReason(..), ThreadStatus(..))
import           Instances.Utils.GenericArbitrary (genericArbitrary)
import           Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

deriving instance Bounded BlockReason
deriving instance Enum BlockReason
instance Arbitrary BlockReason where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary ThreadStatus where
    arbitrary = genericArbitrary

#if __GLASGOW_HASKELL__ >= 704
deriving instance Generic ThreadStatus
#else
$(Generics.deriveAll0 ''ThreadStatus)
#endif
