{-|
Module:      Spec.Data.EitherSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Either'.
-}
module Spec.Data.EitherSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Generics.Deriving.Instances ()
import Spec.Utils (matchesTextShow1Spec, genericTextShowSpec, genericTextShow1Spec)
import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Either Int Int" $ do
    let p :: Proxy (Either Int Int)
        p = Proxy
    matchesTextShow1Spec p
    genericTextShowSpec  p
    genericTextShow1Spec p
