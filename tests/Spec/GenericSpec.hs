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

import Data.Proxy.Compat (Proxy(..))
import Instances.Generic ()
import Instances.Utils (GenericExample)
import Spec.Utils (matchesTextShowSpec, matchesTextShow1Spec, genericTextShowSpec)
import Test.Hspec (Spec, describe, hspec, parallel)
import TextShow.Generic (ConType)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "ConType" $ do
        let p :: Proxy ConType
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "GenericExample Int" $ do
        let p :: Proxy (GenericExample Int)
            p = Proxy
        matchesTextShowSpec  p
        matchesTextShow1Spec p
