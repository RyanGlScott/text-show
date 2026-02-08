{-|
Module:      Spec.BuilderSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for functions that manipulate 'Builder's.
-}
module Spec.BuilderSpec (main, spec) where

import Data.Text.Builder.Linear (runBuilder)

import Instances.Data.Text ()
import Instances.Data.Text.Builder.Linear ()

import Spec.Utils (builderShouldBe)

import Test.Hspec (Expectation, Spec, describe, hspec, parallel, shouldBe)
import Test.Hspec.QuickCheck (prop)

import TextShow (Builder, fromString, fromLazyText, fromText, lengthB,
                 runBuilderLazy, runBuilderString, unlinesB, unwordsB)

main :: IO ()
main = hspec spec

-- | Verifies 'lengthB' and 'length' produce the same output.
prop_lengthB :: String -> Expectation
prop_lengthB s = fromIntegral (lengthB $ fromString s) `shouldBe` length s

-- | Verifies @fromText . runBuilder = id@.
prop_runBuilder :: Builder -> Expectation
prop_runBuilder b = fromText (runBuilder b) `builderShouldBe` b

-- | Verifies @fromLazyText . runBuilderLazy = id@.
prop_runBuilderLazy :: Builder -> Expectation
prop_runBuilderLazy b = fromLazyText (runBuilderLazy b) `builderShouldBe` b

-- | Verifies @fromString . runBuilderString = id@.
prop_runBuilderString :: Builder -> Expectation
prop_runBuilderString b = fromString (runBuilderString b) `builderShouldBe` b

-- | Verifies 'unlinesB' and 'unlines' produce the same output.
prop_unlinesB :: [String] -> Expectation
prop_unlinesB strs = unlinesB (map fromString strs) `builderShouldBe` fromString (unlines strs)

-- | Verifies 'unwordsB' and 'unwords' produce the same output.
prop_unwordsB :: [String] -> Expectation
prop_unwordsB strs = unwordsB (map fromString strs) `builderShouldBe` fromString (unwords strs)

spec :: Spec
spec = parallel $ do
    describe "lengthB" $
        prop "has the same output as length"      prop_lengthB
    describe "runBuilder" $
        prop "fromText . runBuilder = id"         prop_runBuilder
    describe "runBuilderLazy" $
        prop "fromString . runBuilderLazy = id"   prop_runBuilderLazy
    describe "runBuilderString" $
        prop "fromString . runBuilderString = id" prop_runBuilderString
    describe "unlinesB" $
        prop "has the same output as unlines"     prop_unlinesB
    describe "unwordsB" $
        prop "has the same output as unwords"     prop_unwordsB
