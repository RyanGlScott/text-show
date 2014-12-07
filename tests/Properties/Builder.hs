-----------------------------------------------------------------------------
-- |
-- Module      :  Properties
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- @QuickCheck@ properties for functions that manipulate 'Builder's.
----------------------------------------------------------------------------
module Properties.Builder (builderTests) where

import Instances.BaseAndFriends ()

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text (Builder, fromString, lengthB, replicateB,
                       singleton, toString, unlinesB, unwordsB)

-- | Verifies 'lengthB' and 'length' produce the same output.
prop_lengthB :: String -> Bool
prop_lengthB s = fromIntegral (lengthB $ fromString s) == length s

-- | Verifies 'replicateB' and 'replicate' produce the same output.
prop_replicateB :: Int -> Char -> Bool
prop_replicateB i c = replicateB (fromIntegral i) (singleton c) == fromString (replicate i c)

-- | Verifies @fromString . toString = id@.
prop_toString :: Builder -> Bool
prop_toString b = fromString (toString b) == b

-- | Verifies 'unlinesB' and 'unlines' produce the same output.
prop_unlinesB :: [String] -> Bool
prop_unlinesB strs = unlinesB (map fromString strs) == fromString (unlines strs)

-- | Verifies 'unwordsB' and 'unwords' produce the same output.
prop_unwordsB :: [String] -> Bool
prop_unwordsB strs = unwordsB (map fromString strs) == fromString (unwords strs)

builderTests :: [TestTree]
builderTests =
    [ testGroup "Builder-related functions"
        [ testProperty "lengthB output"    prop_lengthB
        , testProperty "replicateB output" prop_replicateB
        , testProperty "fromString . toString = id" prop_toString
        , testProperty "unlinesB output"   prop_unlinesB
        , testProperty "unwordsB output"   prop_unwordsB
        ]
    ]