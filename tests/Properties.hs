-----------------------------------------------------------------------------
-- |
-- Module      :  Properties
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- @QuickCheck@ tests for @text-show@.
----------------------------------------------------------------------------
module Main (main) where

import           Data.Array (Array)
import           Data.Complex (Complex)
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.IntMap (IntMap)
import           Data.IntSet (IntSet)
import           Data.Map (Map)
import           Data.Ratio (Ratio)
import           Data.Sequence (Seq)
import           Data.Set (Set)
import qualified Data.Text as T
import           Data.Text.Lazy (unpack)
import qualified Data.Text as TL
import           Data.Tree (Tree)
import           Data.Word (Word, Word8, Word16, Word32, Word64)

import           Foreign.C.Types
import           Foreign.Ptr (FunPtr, IntPtr, Ptr, WordPtr)

import           Instances ()

import qualified Prelude as P
import           Prelude hiding (Show)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Test.Framework (Test, defaultMain, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Show.Text as T
import           Text.Show.Text hiding (Show)

-- | Verifies that a type's @Show@ instances coincide for both 'String's and 'Text',
--   irrespective of precedence.
prop_matchesShow :: (P.Show a, T.Show a, Arbitrary a) => Int -> a -> Bool
prop_matchesShow k x = showsPrec k x "" == unpack (toLazyText $ showbPrec k x)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "QuickCheck Text.Show.Text"
            [ testProperty "Bool"
                (prop_matchesShow :: Int -> Bool -> Bool)
            , testProperty "Char"
                (prop_matchesShow :: Int -> Char -> Bool)
            , testProperty "Float"
                (prop_matchesShow :: Int -> Float -> Bool)
            , testProperty "Double"
                (prop_matchesShow :: Int -> Double -> Bool)
            , testProperty "Int"
                (prop_matchesShow :: Int -> Int -> Bool)
            , testProperty "Int8"
                (prop_matchesShow :: Int -> Int8 -> Bool)
            , testProperty "Int16"
                (prop_matchesShow :: Int -> Int16 -> Bool)
            , testProperty "Int32"
                (prop_matchesShow :: Int -> Int32 -> Bool)
            , testProperty "Int64"
                (prop_matchesShow :: Int -> Int64 -> Bool)
            , testProperty "Integer"
                (prop_matchesShow :: Int -> Integer -> Bool)
            , testProperty "Ordering"
                (prop_matchesShow :: Int -> Ordering -> Bool)
            , testProperty "Word"
                (prop_matchesShow :: Int -> Word -> Bool)
            , testProperty "Word8"
                (prop_matchesShow :: Int -> Word8 -> Bool)
            , testProperty "Word16"
                (prop_matchesShow :: Int -> Word16 -> Bool)
            , testProperty "Word32"
                (prop_matchesShow :: Int -> Word32 -> Bool)
            , testProperty "Word64"
                (prop_matchesShow :: Int -> Word64 -> Bool)
            , testProperty "()"
                (prop_matchesShow :: Int -> () -> Bool)
            , testProperty "Builder"
                (prop_matchesShow :: Int -> Builder -> Bool)
            , testProperty "String"
                (prop_matchesShow :: Int -> String -> Bool)
            , testProperty "strict Text"
                (prop_matchesShow :: Int -> T.Text -> Bool)
            , testProperty "lazy Text"
                (prop_matchesShow :: Int -> TL.Text -> Bool)
            , testProperty "[String]"
                (prop_matchesShow :: Int -> [String] -> Bool)
            , testProperty "[Int]"
                (prop_matchesShow :: Int -> [Int] -> Bool)
            , testProperty "Ratio Int"
                (prop_matchesShow :: Int -> Ratio Int -> Bool)
            , testProperty "Complex Double"
                (prop_matchesShow :: Int -> Complex Double -> Bool)
            , testProperty "Maybe Int"
                (prop_matchesShow :: Int -> Maybe Int -> Bool)
            , testProperty "Either Int Double"
                (prop_matchesShow :: Int -> Either Int Double -> Bool)
            , testProperty "(Int, Double)"
                (prop_matchesShow :: Int -> (Int, Double) -> Bool)
            , testProperty "(Int, Double, Char)"
                (prop_matchesShow :: Int -> (Int, Double, Char) -> Bool)
            , testProperty "(Int, Double, Char, String)"
                (prop_matchesShow :: Int -> (Int, Double, Char, String) -> Bool)
            , testProperty "(Int, Double, Char, String, Text)"
                (prop_matchesShow :: Int -> (Int, Double, Char, String, T.Text) -> Bool)
            , testProperty "Array Int Text"
                (prop_matchesShow :: Int -> Array Int T.Text -> Bool)
            , testProperty "IntMap Text"
                (prop_matchesShow :: Int -> IntMap T.Text -> Bool)
            , testProperty "IntSet"
                (prop_matchesShow :: Int -> IntSet -> Bool)
            , testProperty "Map Int Text"
                (prop_matchesShow :: Int -> Map Int T.Text -> Bool)
            , testProperty "Sequence Int"
                (prop_matchesShow :: Int -> Seq Int -> Bool)
            , testProperty "Set Int"
                (prop_matchesShow :: Int -> Set Int -> Bool)
            , testProperty "Tree Int"
                (prop_matchesShow :: Int -> Tree Int -> Bool)
            , testProperty "Ptr Int"
                (prop_matchesShow :: Int -> Ptr Int -> Bool)
            , testProperty "FunPtr Int"
                (prop_matchesShow :: Int -> FunPtr Int -> Bool)
            , testProperty "IntPtr"
                (prop_matchesShow :: Int -> IntPtr -> Bool)
            , testProperty "WordPtr"
                (prop_matchesShow :: Int -> WordPtr -> Bool)
            , testProperty "CChar"
                (prop_matchesShow :: Int -> CChar -> Bool)
            , testProperty "CSChar"
                (prop_matchesShow :: Int -> CSChar -> Bool)
            , testProperty "CUChar"
                (prop_matchesShow :: Int -> CUChar -> Bool)
            ]
         ]