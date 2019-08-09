{-|
Module:      Spec.Data.Functor.IdentitySpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'Identity'.
-}
module Spec.Data.Functor.IdentitySpec (main, spec) where

import Control.Monad.Trans.Instances ()

import Data.Functor.Identity (Identity)
import Data.Proxy.Compat (Proxy(..))

import Spec.Utils (matchesTextShow1Spec)

import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Identity Int" $
    matchesTextShow1Spec (Proxy :: Proxy (Identity Int))
