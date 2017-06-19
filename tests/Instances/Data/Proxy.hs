{-# LANGUAGE CPP             #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds       #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Proxy
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for 'Proxy'.
-}
module Instances.Data.Proxy () where

import Data.Proxy (Proxy(..))
import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

instance Arbitrary (Proxy s) where
    arbitrary = arbitraryBoundedEnum
