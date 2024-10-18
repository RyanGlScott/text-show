{-# LANGUAGE CPP                #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module:      Instances.Data.Floating
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'FPFormat'.
-}
module Instances.Data.Floating () where

import Data.Text.Lazy.Builder.RealFloat (FPFormat(..))
import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

#if !(MIN_VERSION_text(2,0,0))
deriving instance Bounded FPFormat
#endif
instance Arbitrary FPFormat where
    arbitrary = arbitraryBoundedEnum
