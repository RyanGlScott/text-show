{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

{-|
Module:      Instances.Data.Functor.Sum
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'Sum'.
-}
module Instances.Data.Functor.Sum () where

import Data.Functor.Sum (Sum(..))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), oneof)

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Sum f g a) where
    arbitrary = oneof [InL <$> arbitrary, InR <$> arbitrary]
