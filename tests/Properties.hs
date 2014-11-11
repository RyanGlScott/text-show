{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Data.Array (Array)
import           Data.Complex (Complex)
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.Map (Map)
import           Data.Ratio (Ratio)
import           Data.Set (Set)
import qualified Data.Text as T
import           Data.Text (unpack)
import qualified Data.Text as TL
import           Data.Word (Word, Word8, Word16, Word32, Word64)

import qualified Prelude as P
import           Prelude hiding (Show(..))

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import qualified Text.Show.Text as T
import           Text.Show.Text (Builder, fromString)

prop_matchesShow :: (P.Show a, T.Show a, Arbitrary a) => a -> Bool
prop_matchesShow x = P.show x == unpack (T.show x)

instance Arbitrary Builder where
    arbitrary = fmap fromString arbitrary

main :: IO ()
main = do
    quickCheck (prop_matchesShow :: Bool                                -> Bool)
    quickCheck (prop_matchesShow :: Char                                -> Bool)
    quickCheck (prop_matchesShow :: Float                               -> Bool)
    quickCheck (prop_matchesShow :: Double                              -> Bool)
    quickCheck (prop_matchesShow :: Int                                 -> Bool)
    quickCheck (prop_matchesShow :: Int8                                -> Bool)
    quickCheck (prop_matchesShow :: Int16                               -> Bool)
    quickCheck (prop_matchesShow :: Int32                               -> Bool)
    quickCheck (prop_matchesShow :: Int64                               -> Bool)
    quickCheck (prop_matchesShow :: Integer                             -> Bool)
    quickCheck (prop_matchesShow :: Ordering                            -> Bool)
    quickCheck (prop_matchesShow :: Word                                -> Bool)
    quickCheck (prop_matchesShow :: Word8                               -> Bool)
    quickCheck (prop_matchesShow :: Word16                              -> Bool)
    quickCheck (prop_matchesShow :: Word32                              -> Bool)
    quickCheck (prop_matchesShow :: Word64                              -> Bool)
    quickCheck (prop_matchesShow :: ()                                  -> Bool)
    quickCheck (prop_matchesShow :: Builder                             -> Bool)
    quickCheck (prop_matchesShow :: String                              -> Bool)
    quickCheck (prop_matchesShow :: T.Text                              -> Bool)
    quickCheck (prop_matchesShow :: TL.Text                             -> Bool)
    quickCheck (prop_matchesShow :: [Int]                               -> Bool)
    quickCheck (prop_matchesShow :: Ratio Int                           -> Bool)
    quickCheck (prop_matchesShow :: Complex Double                      -> Bool)
    quickCheck (prop_matchesShow :: (Int, Double)                       -> Bool)
    quickCheck (prop_matchesShow :: (Int, Double, Char)                 -> Bool)
    quickCheck (prop_matchesShow :: (Int, Double, Char, String)         -> Bool)
    quickCheck (prop_matchesShow :: (Int, Double, Char, String, T.Text) -> Bool)
    quickCheck (prop_matchesShow :: Array Int T.Text                    -> Bool)
    quickCheck (prop_matchesShow :: Map Int T.Text                      -> Bool)
    quickCheck (prop_matchesShow :: Set Int                             -> Bool)