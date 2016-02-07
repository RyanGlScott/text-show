{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

{-|
Module:      Instances.Data.Functor.Product
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'Product'.
-}
module Instances.Data.Functor.Product () where

import Data.Functor.Product (Product(..))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Product f g a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary
