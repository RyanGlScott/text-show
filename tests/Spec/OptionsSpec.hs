{-|
Module:      Spec.OptionsSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Options' and related datatypes.
-}
module Spec.OptionsSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Instances.Options ()
import Spec.Utils (matchesTextShowSpec, genericTextShowSpec)
import Test.Hspec (Spec, describe, hspec, parallel)
import TextShow.TH (Options, GenTextMethods)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Options" $ do
        let p :: Proxy Options
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "GenTextMethods" $ do
        let p :: Proxy GenTextMethods
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
