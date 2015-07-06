module Spec.Control.Monad.STSpec (main, spec) where

import Control.Monad.ST

import Instances.Control.Monad.ST ()

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "ST Int Int" $
    prop "TextShow instance" (prop_matchesTextShow :: Int -> ST Int Int -> Bool)
