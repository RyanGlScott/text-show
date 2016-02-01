{-|
Module:      Spec.Control.ApplicativeSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Control.Applicative" module.
-}
module Spec.Control.ApplicativeSpec (main, spec) where

import Control.Applicative (Const, ZipList)
import Control.Monad.Trans.Instances ()

import Data.Orphans ()

import Generics.Deriving.Instances ()

import Instances.Control.Applicative ()

import Spec.Utils (prop_matchesTextShow, prop_matchesTextShow1,
                   prop_genericTextShow, prop_genericTextShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Const Int Int" $
        prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> Const Int Int -> Bool)
    describe "ZipList Int" $ do
        prop "TextShow instance"  (prop_matchesTextShow  :: Int -> ZipList Int -> Bool)
        prop "generic TextShow"   (prop_genericTextShow  :: Int -> ZipList Int -> Bool)
        prop "generic TextShow1"  (prop_genericTextShow1 :: Int -> ZipList Int -> Bool)
