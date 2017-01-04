{-|
Module:      Spec.GenericSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'ConType'.
-}
module Spec.GenericSpec (main, spec) where

import Data.Proxy (Proxy(..))
import Instances.Generic ()
import Spec.Utils (matchesTextShowSpec, genericTextShowSpec)
import Test.Hspec (Spec, describe, hspec, parallel)
import TextShow.Generic (ConType)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "ConType" $ do
    let p :: Proxy ConType
        p = Proxy
    matchesTextShowSpec p
    genericTextShowSpec p
