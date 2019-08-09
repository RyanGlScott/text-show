{-|
Module:      Spec.Data.ProxySpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Proxy'.
-}
module Spec.Data.ProxySpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))

import Generics.Deriving.Base ()

import Spec.Utils (matchesTextShowSpec, genericTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.QuickCheck.Instances ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Proxy Int" $ do
    let p :: Proxy (Proxy Int)
        p = Proxy
    matchesTextShowSpec p
    genericTextShowSpec p
