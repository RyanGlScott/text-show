{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Foreign.C.Types
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "Foreign.C.Types" module.
-}
module Instances.Foreign.C.Types () where

#if MIN_VERSION_base(4,10,0)
import Foreign.C.Types
import Test.QuickCheck (Arbitrary(..))

deriving instance Arbitrary CBool
#endif
