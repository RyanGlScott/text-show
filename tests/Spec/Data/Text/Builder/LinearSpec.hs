{-|
Module:      Spec.Data.Text.Builder.LinearSpec
Copyright:   (C) 2026 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @text-builder-linear@ library.
-}
module Spec.Data.Text.Builder.LinearSpec (main, spec) where

import           Data.Proxy (Proxy(..))
import           Data.Text.Builder.Linear (Builder)

import           Instances.Data.Text.Builder.Linear ()

import           Spec.Utils (matchesTextShowSpec)

import           Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Builder" $
        matchesTextShowSpec (Proxy :: Proxy Builder)
