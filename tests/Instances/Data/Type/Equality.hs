{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Type.Equality
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

'Arbitrary' instance for '(:~:)'.
-}
module Instances.Data.Type.Equality () where

#if MIN_VERSION_base(4,7,0)
import Data.Type.Equality ((:~:)(..))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

instance a ~ b => Arbitrary (a :~: b) where
    arbitrary = pure Refl
#endif
