{-# LANGUAGE CPP #-}
{-|
Module:      Properties.Builder
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for functions that manipulate 'Builder's.
-}
module Properties.Builder (builderTests) where

import Instances.BaseAndFriends ()

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text (Builder, fromString, fromText, lengthB,
                       toString, toText, unlinesB, unwordsB)
#if !defined(mingw32_HOST_OS)
import Text.Show.Text (replicateB, singleton)
#endif

-- | Verifies 'lengthB' and 'length' produce the same output.
prop_lengthB :: String -> Bool
prop_lengthB s = fromIntegral (lengthB $ fromString s) == length s

#if !defined(mingw32_HOST_OS)
-- | Verifies 'replicateB' and 'replicate' produce the same output.
prop_replicateB :: Int -> Char -> Bool
prop_replicateB i c = replicateB (fromIntegral i) (singleton c) == fromString (replicate i c)
#endif

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

builderTests :: [TestTree]
builderTests =
    [ testGroup "Builder-related functions"
        [ testProperty "lengthB output"             prop_lengthB
#if !defined(mingw32_HOST_OS)
-- TODO: Figure out why this diverges on Windows
        , testProperty "replicateB output"          prop_replicateB
#endif
        , testProperty "fromString . toString = id" prop_toString
        , testProperty "fromText . toText = id"     prop_toText
        , testProperty "unlinesB output"            prop_unlinesB
        , testProperty "unwordsB output"            prop_unwordsB
        ]
    ]