{-# LANGUAGE CPP #-}
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

import           Control.Applicative (ZipList(..))
import           Control.Exception

import           Data.Array (Array)
import qualified Data.ByteString      as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
#if MIN_VERSION_bytestring(0,10,4)
import           Data.ByteString.Short (ShortByteString)
#endif
import           Data.Char (GeneralCategory)
import           Data.Complex (Complex)
import           Data.Data (Constr, ConstrRep, DataRep, DataType, Fixity)
import           Data.Dynamic (Dynamic)
import           Data.Fixed (Fixed, E0, E1, E2, E3, E6, E9, E12)
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.IntMap (IntMap)
import           Data.IntSet (IntSet)
import           Data.Map (Map)
import           Data.Monoid (All(..), Any(..), Dual(..), First(..),
                              Last(..), Product(..), Sum(..))
#if MIN_VERSION_base(4,6,0)
import           Data.Ord (Down(..))
#endif
#if MIN_VERSION_base(4,7,0)
import           Data.Proxy (Proxy)
#endif
import           Data.Ratio (Ratio)
import           Data.Sequence (Seq)
import           Data.Set (Set)
import qualified Data.Text as T
import           Data.Text.Lazy (unpack)
import qualified Data.Text as TL
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (DiffTime, UTCTime, NominalDiffTime)
import           Data.Time.Clock.TAI (AbsoluteTime)
import           Data.Time.LocalTime (TimeZone, TimeOfDay, LocalTime)
import           Data.Tree (Tree)
#if MIN_VERSION_base(4,4,0)
import           Data.Typeable.Internal (TyCon, TypeRep, Fingerprint)
#endif
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Data.Version (Version)

import           Foreign.C.Types
import           Foreign.Ptr (FunPtr, IntPtr, Ptr, WordPtr)

import           Instances ()

import qualified Prelude as P
import           Prelude hiding (Show)

import           System.Exit (ExitCode)
import           System.Posix.Types

import           Test.QuickCheck hiding (Fixed)
import           Test.QuickCheck.Instances ()

import           Test.Framework (Test, defaultMain, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Text.Show.Functions ()
import qualified Text.Show.Text as T
import           Text.Show.Text hiding (Show)
import           Text.Show.Text.Functions ()

-- | Verifies that a type's @Show@ instances coincide for both 'String's and 'Text',
--   irrespective of precedence.
prop_matchesShow :: (P.Show a, T.Show a, Arbitrary a) => Int -> a -> Bool
prop_matchesShow k x = showsPrec k x "" == unpack (toLazyText $ showbPrec k x)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "QuickCheck properties"
            [ testGroup "Text.Show.Text.Control.Applicative"
                [ testProperty "ZipList Int"               (prop_matchesShow :: Int -> ZipList Int -> Bool)
                ]
            , testGroup "Text.Show.Text.Control.Exception"
                [ -- testProperty "SomeException"             (prop_matchesShow :: Int -> SomeException -> Bool)
--                 , testProperty "IOException"               (prop_matchesShow :: Int -> IOException -> Bool)
                  testProperty "ArithException"            (prop_matchesShow :: Int -> ArithException -> Bool)
                , testProperty "ArrayException"            (prop_matchesShow :: Int -> ArrayException -> Bool)
                , testProperty "AssertionFailed"           (prop_matchesShow :: Int -> AssertionFailed -> Bool)
-- #if MIN_VERSION_base(4,7,0)
--                 , testProperty "SomeAsyncException"        (prop_matchesShow :: Int -> SomeAsyncException -> Bool)
-- #endif
                , testProperty "AsyncException"            (prop_matchesShow :: Int -> AsyncException -> Bool)
                , testProperty "NonTermination"            (prop_matchesShow :: Int -> NonTermination -> Bool)
                , testProperty "NestedAtomically"          (prop_matchesShow :: Int -> NestedAtomically -> Bool)
                , testProperty "BlockedIndefinitelyOnMVar" (prop_matchesShow :: Int -> BlockedIndefinitelyOnMVar -> Bool)
                , testProperty "BlockedIndefinitelyOnSTM"  (prop_matchesShow :: Int -> BlockedIndefinitelyOnSTM -> Bool)
                , testProperty "Deadlock"                  (prop_matchesShow :: Int -> Deadlock -> Bool)
                , testProperty "NoMethodError"             (prop_matchesShow :: Int -> NoMethodError -> Bool)
                , testProperty "PatternMatchFail"          (prop_matchesShow :: Int -> PatternMatchFail -> Bool)
                , testProperty "RecConError"               (prop_matchesShow :: Int -> RecConError -> Bool)
                , testProperty "RecSelError"               (prop_matchesShow :: Int -> RecSelError -> Bool)
                , testProperty "RecUpdError"               (prop_matchesShow :: Int -> RecUpdError -> Bool)
                , testProperty "ErrorCall"                 (prop_matchesShow :: Int -> ErrorCall -> Bool)
                ]
            , testGroup "Text.Show.Text.Data.Array"
                [ testProperty "Array Int Int"             (prop_matchesShow :: Int -> Array Int Int -> Bool)
                ]
            , testGroup "Text.Show.Text.Data.Bool"
                [ testProperty "Bool"                      (prop_matchesShow :: Int -> Bool -> Bool)
                ]
            , testGroup "Text.Show.Text.Data.ByteString"
                [ testProperty "strict ByteString"         (prop_matchesShow :: Int -> BS.ByteString -> Bool)
                , testProperty "lazy ByteString"           (prop_matchesShow :: Int -> BL.ByteString -> Bool)
#if MIN_VERSION_bytestring(0,10,4)
                , testProperty "ShortByteString"           (prop_matchesShow :: Int -> ShortByteString -> Bool)
#endif
                ]
            , testGroup "Text.Show.Text.Data.Char"
                [ testProperty "Char"                      (prop_matchesShow :: Int -> Char -> Bool)
                , testProperty "GeneralCategory"           (prop_matchesShow :: Int -> GeneralCategory -> Bool)
                ]
            , testGroup "Text.Show.Text.Data.Containers"
                [ testProperty "IntMap Int"                (prop_matchesShow :: Int -> IntMap Int -> Bool)
                , testProperty "IntSet"                    (prop_matchesShow :: Int -> IntSet -> Bool)
                , testProperty "Map Int Int"               (prop_matchesShow :: Int -> Map Int Int -> Bool)
                , testProperty "Sequence Int"              (prop_matchesShow :: Int -> Seq Int -> Bool)
                , testProperty "Set Int"                   (prop_matchesShow :: Int -> Set Int -> Bool)
                , testProperty "Tree Int"                  (prop_matchesShow :: Int -> Tree Int -> Bool)
                ]
            , testGroup "Text.Show.Text.Data.Data"
                [ testProperty "Constr"                    (prop_matchesShow :: Int -> Constr -> Bool)
                , testProperty "ConstrRep"                 (prop_matchesShow :: Int -> ConstrRep -> Bool)
                , testProperty "DataRep"                   (prop_matchesShow :: Int -> DataRep -> Bool)
                , testProperty "DataType"                  (prop_matchesShow :: Int -> DataType -> Bool)
                , testProperty "Fixity"                    (prop_matchesShow :: Int -> Fixity -> Bool)
                ]
            , testGroup "Text.Show.Text.Data.Dynamic"
                [ testProperty "Dynamic"                   (prop_matchesShow :: Int -> Dynamic -> Bool)
                ]
            , testGroup "Text.Show.Text.Data.Either"
                [ testProperty "Either Int Int"            (prop_matchesShow :: Int -> Either Int Int -> Bool)
                ]
            , testGroup "Text.Show.Text.Data.Fixed"
                [ testProperty "Fixed E0"                  (prop_matchesShow :: Int -> Fixed E0 -> Bool)
                , testProperty "Fixed E1"                  (prop_matchesShow :: Int -> Fixed E1 -> Bool)
                , testProperty "Fixed E2"                  (prop_matchesShow :: Int -> Fixed E2 -> Bool)
                , testProperty "Fixed E3"                  (prop_matchesShow :: Int -> Fixed E3 -> Bool)
                , testProperty "Fixed E6"                  (prop_matchesShow :: Int -> Fixed E6 -> Bool)
                , testProperty "Fixed E9"                  (prop_matchesShow :: Int -> Fixed E9 -> Bool)
                , testProperty "Fixed E12"                 (prop_matchesShow :: Int -> Fixed E12 -> Bool)
                ]
            , testGroup "Text.Show.Text.Data.Floating"
                [ testProperty "Float"                     (prop_matchesShow :: Int -> Float -> Bool)
                , testProperty "Double"                    (prop_matchesShow :: Int -> Double -> Bool)
                , testProperty "Complex Double"            (prop_matchesShow :: Int -> Complex Double -> Bool)
                ]
            , testGroup "Text.Show.Text.Data.Functions"
                [ testProperty "Int -> Int"                (prop_matchesShow :: Int -> (Int -> Int) -> Bool)
                ]
            , testGroup "Text.Show.Text.Data.Integral"
                [ testProperty "Int"                       (prop_matchesShow :: Int -> Int -> Bool)
                , testProperty "Int8"                      (prop_matchesShow :: Int -> Int8 -> Bool)
                , testProperty "Int16"                     (prop_matchesShow :: Int -> Int16 -> Bool)
                , testProperty "Int32"                     (prop_matchesShow :: Int -> Int32 -> Bool)
                , testProperty "Int64"                     (prop_matchesShow :: Int -> Int64 -> Bool)
                , testProperty "Integer"                   (prop_matchesShow :: Int -> Integer -> Bool)
                , testProperty "Word"                      (prop_matchesShow :: Int -> Word -> Bool)
                , testProperty "Word8"                     (prop_matchesShow :: Int -> Word8 -> Bool)
                , testProperty "Word16"                    (prop_matchesShow :: Int -> Word16 -> Bool)
                , testProperty "Word32"                    (prop_matchesShow :: Int -> Word32 -> Bool)
                , testProperty "Word64"                    (prop_matchesShow :: Int -> Word64 -> Bool)
                , testProperty "Ratio Int"                 (prop_matchesShow :: Int -> Ratio Int -> Bool)
                ]
            , testGroup "Text.Show.Text.Data.List"
                [ testProperty "String"                    (prop_matchesShow :: Int -> String -> Bool)
                , testProperty "[String]"                  (prop_matchesShow :: Int -> [String] -> Bool)
                , testProperty "[Int]"                     (prop_matchesShow :: Int -> [Int] -> Bool)
                ]
            , testGroup "Text.Show.Text.Data.Maybe"
                [ testProperty "Maybe Int"                 (prop_matchesShow :: Int -> Maybe Int -> Bool)
                ]
            , testGroup "Text.Show.Text.Data.Monoid"
                [ testProperty "All"                       (prop_matchesShow :: Int -> All -> Bool)
                , testProperty "Any"                       (prop_matchesShow :: Int -> Any -> Bool)
                , testProperty "Dual Int"                  (prop_matchesShow :: Int -> Dual Int -> Bool)
                , testProperty "First (Maybe Int)"         (prop_matchesShow :: Int -> First (Maybe Int) -> Bool)
                , testProperty "Last (Maybe Int)"          (prop_matchesShow :: Int -> Last (Maybe Int) -> Bool)
                , testProperty "Product Int"               (prop_matchesShow :: Int -> Product Int -> Bool)
                , testProperty "Sum Int"                   (prop_matchesShow :: Int -> Sum Int -> Bool)
                ]
            , testGroup "Text.Show.Text.Data.Ord"
                [ testProperty "Ordering"                  (prop_matchesShow :: Int -> Ordering -> Bool)
#if MIN_VERSION_base(4,6,0)
                , testProperty "Down Int"                  (prop_matchesShow :: Int -> Down Int -> Bool)
#endif
                ]
            , testGroup "Text.Show.Text.Data.Text"
                [ testProperty "Builder"                   (prop_matchesShow :: Int -> Builder -> Bool)
                , testProperty "strict Text"               (prop_matchesShow :: Int -> T.Text -> Bool)
                , testProperty "lazy Text"                 (prop_matchesShow :: Int -> TL.Text -> Bool)
                ]
            , testGroup "Text.Show.Text.Data.Time"
                [ testProperty "Day"                       (prop_matchesShow :: Int -> Day -> Bool)
                , testProperty "DiffTime"                  (prop_matchesShow :: Int -> DiffTime -> Bool)
                , testProperty "UTCTime"                   (prop_matchesShow :: Int -> UTCTime -> Bool)
                , testProperty "NominalDiffTime"           (prop_matchesShow :: Int -> NominalDiffTime -> Bool)
                , testProperty "AbsoluteTime"              (prop_matchesShow :: Int -> AbsoluteTime -> Bool)
                , testProperty "TimeZone"                  (prop_matchesShow :: Int -> TimeZone -> Bool)
                , testProperty "TimeOfDay"                 (prop_matchesShow :: Int -> TimeOfDay -> Bool)
                , testProperty "LocalTime"                 (prop_matchesShow :: Int -> LocalTime -> Bool)
                ]
            , testGroup "Text.Show.Text.Data.Tuple"
                [ testProperty "()"                        (prop_matchesShow :: Int -> () -> Bool)
                , testProperty "(Int, Int)"                (prop_matchesShow :: Int -> (Int, Int) -> Bool)
                , testProperty "(Int, Int, Int)"           (prop_matchesShow :: Int -> (Int, Int, Int) -> Bool)
                , testProperty "(Int, Int, Int, Int)"      (prop_matchesShow :: Int -> (Int, Int, Int, Int) -> Bool)
                , testProperty "(Int, Int, Int, Int, Int)" (prop_matchesShow :: Int -> (Int, Int, Int, Int, Int) -> Bool)
                ]
#if MIN_VERSION_base(4,4,0)
            , testGroup "Text.Show.Text.Data.Typeable"
                [ testProperty "TypeRep"                   (prop_matchesShow :: Int -> TypeRep -> Bool)
                , testProperty "TyCon"                     (prop_matchesShow :: Int -> TyCon -> Bool)
                , testProperty "Fingerprint"               (prop_matchesShow :: Int -> Fingerprint -> Bool)
#if MIN_VERSION_base(4,7,0)
                , testProperty "Proxy Int"                 (prop_matchesShow :: Int -> Proxy Int -> Bool)
#endif
                ]
#endif
            , testGroup "Text.Show.Text.Data.Version"
                [ testProperty "Version"                   (prop_matchesShow :: Int -> Version -> Bool)
                ]
            , testGroup "Text.Show.Text.Foreign.C.Types"
                [ testProperty "CChar"
                (prop_matchesShow :: Int -> CChar -> Bool)
                , testProperty "CSChar"                    (prop_matchesShow :: Int -> CSChar -> Bool)
                , testProperty "CUChar"                    (prop_matchesShow :: Int -> CUChar -> Bool)
                , testProperty "CShort"                    (prop_matchesShow :: Int -> CShort -> Bool)
                , testProperty "CUShort"                   (prop_matchesShow :: Int -> CUShort -> Bool)
                , testProperty "CInt"                      (prop_matchesShow :: Int -> CInt -> Bool)
                , testProperty "CUInt"                     (prop_matchesShow :: Int -> CUInt -> Bool)
                , testProperty "CLong"                     (prop_matchesShow :: Int -> CLong -> Bool)
                , testProperty "CULong"                    (prop_matchesShow :: Int -> CULong -> Bool)
                , testProperty "CPtrdiff"                  (prop_matchesShow :: Int -> CPtrdiff -> Bool)
                , testProperty "CSize"                     (prop_matchesShow :: Int -> CSize -> Bool)
                , testProperty "CWchar"                    (prop_matchesShow :: Int -> CWchar -> Bool)
                , testProperty "CSigAtomic"                (prop_matchesShow :: Int -> CSigAtomic -> Bool)
                , testProperty "CLLong"                    (prop_matchesShow :: Int -> CLLong -> Bool)
                , testProperty "CULLong"                   (prop_matchesShow :: Int -> CULLong -> Bool)
                , testProperty "CIntPtr"                   (prop_matchesShow :: Int -> CIntPtr -> Bool)
                , testProperty "CUIntPtr"                  (prop_matchesShow :: Int -> CUIntPtr -> Bool)
                , testProperty "CIntMax"                   (prop_matchesShow :: Int -> CIntMax -> Bool)
                , testProperty "CUIntPtr"                  (prop_matchesShow :: Int -> CUIntPtr -> Bool)
                , testProperty "CIntMax"                   (prop_matchesShow :: Int -> CIntMax -> Bool)
                , testProperty "CUIntMax"                  (prop_matchesShow :: Int -> CUIntMax -> Bool)
                , testProperty "CClock"                    (prop_matchesShow :: Int -> CClock -> Bool)
                , testProperty "CTime"                     (prop_matchesShow :: Int -> CTime -> Bool)
                , testProperty "CUSeconds"                 (prop_matchesShow :: Int -> CUSeconds -> Bool)
                , testProperty "CSUSeconds"                (prop_matchesShow :: Int -> CSUSeconds -> Bool)
                , testProperty "CFloat"                    (prop_matchesShow :: Int -> CFloat -> Bool)
                , testProperty "CDouble"                   (prop_matchesShow :: Int -> CUChar -> Bool)
                ]
            , testGroup "Text.Show.Text.Foreign.Ptr"
                [ testProperty "Ptr Int"                   (prop_matchesShow :: Int -> Ptr Int -> Bool)
                , testProperty "FunPtr Int"                (prop_matchesShow :: Int -> FunPtr Int -> Bool)
                , testProperty "IntPtr"                    (prop_matchesShow :: Int -> IntPtr -> Bool)
                , testProperty "WordPtr"                   (prop_matchesShow :: Int -> WordPtr -> Bool)
--                 , testProperty "ForeignPtr"                (prop_matchesShow :: Int -> ForeignPtr Int -> Bool)
                ]
            , testGroup "Text.Show.Text.System.Exit"
                [ testProperty "ExitCode"                  (prop_matchesShow :: Int -> ExitCode -> Bool)
                ]
            , testGroup "Text.Show.Text.System.Posix.Types"
                [ testProperty "CDev"                      (prop_matchesShow :: Int -> CDev -> Bool)
                , testProperty "CIno"                      (prop_matchesShow :: Int -> CIno -> Bool)
                , testProperty "CMode"                     (prop_matchesShow :: Int -> CMode -> Bool)
                , testProperty "COff"                      (prop_matchesShow :: Int -> COff -> Bool)
                , testProperty "CPid"                      (prop_matchesShow :: Int -> CPid -> Bool)
                , testProperty "CSsize"                    (prop_matchesShow :: Int -> CSsize -> Bool)
                , testProperty "CGid"                      (prop_matchesShow :: Int -> CGid -> Bool)
                , testProperty "CNlink"                    (prop_matchesShow :: Int -> CNlink -> Bool)
                , testProperty "CUid"                      (prop_matchesShow :: Int -> CUid -> Bool)
                , testProperty "CCc"                       (prop_matchesShow :: Int -> CCc -> Bool)
                , testProperty "CSpeed"                    (prop_matchesShow :: Int -> CSpeed -> Bool)
                , testProperty "CTcflag"                   (prop_matchesShow :: Int -> CTcflag -> Bool)
                , testProperty "CRLim"                     (prop_matchesShow :: Int -> CRLim -> Bool)
                , testProperty "Fd"                        (prop_matchesShow :: Int -> Fd -> Bool)
                ]
--             , testGroup "Text.Show.Text.Text.Read.Lex"
--                 [ testProperty "Lexeme"                    (prop_matchesShow :: Int -> Lexeme -> Bool)
--                 , testProperty "Number"                    (prop_matchesShow :: Int -> Number -> Bool)
--                 ]
            ]
         ]