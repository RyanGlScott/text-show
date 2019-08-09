{-|
Module:      Spec.Data.MaybeSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Maybe'.
-}
module Spec.Data.MaybeSpec (main, spec) where

import Data.Orphans ()
import Data.Proxy.Compat (Proxy(..))

import Spec.Utils (matchesTextShow1Spec, genericTextShowSpec, genericTextShow1Spec)

import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Maybe Int" $ do
    let p :: Proxy (Maybe Int)
        p = Proxy
    matchesTextShow1Spec p
    genericTextShowSpec  p
    genericTextShow1Spec p
