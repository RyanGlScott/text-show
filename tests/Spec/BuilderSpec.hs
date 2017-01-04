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

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import TextShow (Builder, fromString, fromText, lengthB,
                       toString, toText, unlinesB, unwordsB)

main :: IO ()
main = hspec spec

-- | Verifies 'lengthB' and 'length' produce the same output.
prop_lengthB :: String -> Bool
prop_lengthB s = fromIntegral (lengthB $ fromString s) == length s

-- | Verifies @fromText . toText = id@.
prop_toText :: Builder -> Bool
prop_toText b = fromText (toText b) == b

-- | Verifies @fromString . toString = id@.
prop_toString :: Builder -> Bool
prop_toString b = fromString (toString b) == b

-- | Verifies 'unlinesB' and 'unlines' produce the same output.
prop_unlinesB :: [String] -> Bool
prop_unlinesB strs = unlinesB (map fromString strs) == fromString (unlines strs)

-- | Verifies 'unwordsB' and 'unwords' produce the same output.
prop_unwordsB :: [String] -> Bool
prop_unwordsB strs = unwordsB (map fromString strs) == fromString (unwords strs)

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
