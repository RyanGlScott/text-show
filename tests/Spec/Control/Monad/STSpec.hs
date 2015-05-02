module Spec.Control.Monad.STSpec (main, spec) where

import Control.Monad.ST

import Instances.Control.Monad.ST ()

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Control.Monad.ST" $
    prop "ST instance" (prop_matchesShow :: Int -> ST Int Int -> Bool)
