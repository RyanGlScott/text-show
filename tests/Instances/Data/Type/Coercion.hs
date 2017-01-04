{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Type.Coercion
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'Coercion'.
-}
module Instances.Data.Type.Coercion () where

#if MIN_VERSION_base(4,7,0)
import Data.Coerce (Coercible)
import Data.Type.Coercion (Coercion(..))

import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

instance Coercible a b => Arbitrary (Coercion a b) where
    arbitrary = arbitraryBoundedEnum
#endif
