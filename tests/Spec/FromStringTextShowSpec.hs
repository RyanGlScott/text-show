{-# LANGUAGE CPP #-}

{-|
Module:      Spec.FromStringTextShowSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'FromStringShow' and 'FromTextShow'.
-}
module Spec.FromStringTextShowSpec (main, spec) where

import Instances.FromStringTextShow ()

import Spec.Utils (prop_matchesTextShow, prop_matchesTextShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import TextShow (FromStringShow(..), FromTextShow(..))

#if !(MIN_VERSION_transformers(0,4,0)) || MIN_VERSION_transformers(0,5,0)
import Spec.Utils (prop_matchesTextShow2)
import TextShow (FromStringShow1(..), FromStringShow2(..),
                 FromTextShow1(..), FromTextShow2(..))
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "FromStringShow Int" $ do
        prop "TextShow instance"  (prop_matchesTextShow  :: Int -> FromStringShow Int -> Bool)
        prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> FromStringShow Int -> Bool)
    describe "FromStringShow String" $ do
        prop "TextShow instance"  (prop_matchesTextShow  :: Int -> FromStringShow String -> Bool)
        prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> FromStringShow String -> Bool)
    describe "FromTextShow Int" $ do
        prop "TextShow instance"  (prop_matchesTextShow  :: Int -> FromTextShow Int -> Bool)
        prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> FromTextShow Int -> Bool)
    describe "FromTextShow String" $ do
        prop "TextShow instance"  (prop_matchesTextShow  :: Int -> FromTextShow String -> Bool)
        prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> FromTextShow String -> Bool)
#if !(MIN_VERSION_transformers(0,4,0)) || MIN_VERSION_transformers(0,5,0)
    describe "FromStringShow1 Maybe Int" $ do
        prop "TextShow instance"  (prop_matchesTextShow  :: Int -> FromStringShow1 Maybe Int -> Bool)
        prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> FromStringShow1 Maybe Int -> Bool)
    describe "FromTextShow1 Maybe Int" $ do
        prop "TextShow instance"  (prop_matchesTextShow  :: Int -> FromTextShow1 Maybe Int -> Bool)
        prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> FromTextShow1 Maybe Int -> Bool)
    describe "FromStringShow2 Either Char Int" $ do
        prop "TextShow instance"  (prop_matchesTextShow  :: Int -> FromStringShow2 Either Char Int -> Bool)
        prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> FromStringShow2 Either Char Int -> Bool)
        prop "TextShow2 instance" (prop_matchesTextShow2 :: Int -> FromStringShow2 Either Char Int -> Bool)
    describe "FromTextShow2 Either Char Int" $ do
        prop "TextShow instance"  (prop_matchesTextShow  :: Int -> FromTextShow2 Either Char Int -> Bool)
        prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> FromTextShow2 Either Char Int -> Bool)
        prop "TextShow2 instance" (prop_matchesTextShow2 :: Int -> FromTextShow2 Either Char Int -> Bool)
#endif
