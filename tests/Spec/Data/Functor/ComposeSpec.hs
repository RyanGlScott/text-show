{-|
Module:      Spec.Data.Functor.ComposeSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'Compose'.
-}
module Spec.Data.Functor.ComposeSpec (main, spec) where

import Control.Monad.Trans.Instances ()

import Data.Functor.Compose (Compose)
import Data.Proxy.Compat (Proxy(..))

import Spec.Utils (matchesTextShow1Spec)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.QuickCheck.Instances ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Compose Maybe Maybe Int" $
    matchesTextShow1Spec (Proxy :: Proxy (Compose Maybe Maybe Int))
