{-|
Module:      Spec.Data.Functor.ProductSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'Product'.
-}
module Spec.Data.Functor.ProductSpec (main, spec) where

import Control.Monad.Trans.Instances ()

import Data.Functor.Product (Product)
import Data.Proxy.Compat (Proxy(..))

import Spec.Utils (matchesTextShow1Spec)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.QuickCheck.Instances ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Product Maybe Maybe Int" $
    matchesTextShow1Spec (Proxy :: Proxy (Product Maybe Maybe Int))
