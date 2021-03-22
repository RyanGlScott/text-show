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

import Instances.Data.Text ()

import Test.Hspec (Expectation, Spec, describe, hspec, parallel, shouldBe)
import Test.Hspec.QuickCheck (prop)

import TextShow (Builder, fromString, fromText, lengthB,
                       toString, toText, unlinesB, unwordsB)

main :: IO ()
main = hspec spec

-- | Verifies 'lengthB' and 'length' produce the same output.
prop_lengthB :: String -> Expectation
prop_lengthB s = fromIntegral (lengthB $ fromString s) `shouldBe` length s

-- | Verifies @fromText . toText = id@.
prop_toText :: Builder -> Expectation
prop_toText b = fromText (toText b) `shouldBe` b

-- | Verifies @fromString . toString = id@.
prop_toString :: Builder -> Expectation
prop_toString b = fromString (toString b) `shouldBe` b

-- | Verifies 'unlinesB' and 'unlines' produce the same output.
prop_unlinesB :: [String] -> Expectation
prop_unlinesB strs = unlinesB (map fromString strs) `shouldBe` fromString (unlines strs)

-- | Verifies 'unwordsB' and 'unwords' produce the same output.
prop_unwordsB :: [String] -> Expectation
prop_unwordsB strs = unwordsB (map fromString strs) `shouldBe` fromString (unwords strs)

spec :: Spec
spec = parallel $ do
    describe "lengthB" $
        prop "has the same output as length"  prop_lengthB
    describe "toString" $
        prop "fromString . toString = id"     prop_toString
    describe "toText" $
        prop "fromText . toText = id"         prop_toText
    describe "unlinesB" $
        prop "has the same output as unlines" prop_unlinesB
    describe "unwordsB" $
        prop "has the same output as unwords" prop_unwordsB
