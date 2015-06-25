{-|
Module:      Spec.BuilderSpec
Copyright:   (C) 2014-2015 Ryan Scott
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

import Text.Show.Text (Builder, fromString, fromText, lengthB,
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
spec = parallel . describe "Builder-related functions" $ do
    prop "lengthB output"             prop_lengthB
    prop "fromString . toString = id" prop_toString
    prop "fromText . toText = id"     prop_toText
    prop "unlinesB output"            prop_unlinesB
    prop "unwordsB output"            prop_unwordsB
