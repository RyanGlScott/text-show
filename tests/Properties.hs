{-# LANGUAGE CPP, NoImplicitPrelude #-}
#if MIN_VERSION_base(4,4,0)
{-# LANGUAGE FlexibleContexts, TypeOperators #-}
#endif
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

import           Control.Applicative (ZipList(..), liftA2)
import           Control.Exception
import           Control.Monad.ST

import           Data.Array (Array)
import qualified Data.ByteString      as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
#if MIN_VERSION_bytestring(0,10,4)
import           Data.ByteString.Short (ShortByteString)
#endif
import           Data.Char (GeneralCategory, intToDigit)
import           Data.Complex (Complex)
import qualified Data.Data as D (Fixity)
import           Data.Data (Constr, ConstrRep, DataRep, DataType)
import           Data.Dynamic (Dynamic)
import           Data.Fixed (Fixed, E0, E1, E2, E3, E6, E9, E12, showFixed)
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
import qualified Data.Text as TS
import           Data.Text.Lazy (unpack)
import qualified Data.Text as TL
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (DiffTime, UTCTime, NominalDiffTime)
import           Data.Time.Clock.TAI (AbsoluteTime)
import           Data.Time.LocalTime (TimeZone, TimeOfDay, LocalTime)
import           Data.Tree (Tree)
#if MIN_VERSION_base(4,7,0)
import           Data.Type.Coercion (Coercion)
import           Data.Type.Equality ((:~:))
#endif
#if MIN_VERSION_base(4,4,0)
import           Data.Typeable.Internal (TyCon, TypeRep)
import           GHC.Fingerprint.Type (Fingerprint)
#endif
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Data.Version (Version, showVersion)

import           Foreign.C.Types
import           Foreign.Ptr (FunPtr, IntPtr, Ptr, WordPtr)

import           GHC.Conc (BlockReason, ThreadStatus)
import qualified GHC.Generics as G (Fixity)
import           GHC.Generics (U1, Par1, Rec1, K1, M1, (:+:), (:*:), (:.:),
                               Associativity, Arity)
#if MIN_VERSION_base(4,4,0)
import           GHC.IO.Encoding.Failure (CodingFailureMode)
import           GHC.IO.Encoding.Types (CodingProgress)
#endif
#if MIN_VERSION_base(4,5,0)
import           GHC.Stats (GCStats)
#endif

import           Instances ()

import           Numeric (showIntAtBase, showEFloat, showFFloat, showGFloat)

import qualified Prelude as P
import           Prelude hiding (Show)

import           System.Exit (ExitCode)
import           System.IO (BufferMode, IOMode, Newline, NewlineMode, SeekMode)
import           System.Posix.Types

import           Test.QuickCheck hiding (Fixed)
import           Test.QuickCheck.Instances ()
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Text.Show.Functions ()
import qualified Text.Show.Text as T
import           Text.Show.Text hiding (Show)
import           Text.Show.Text.Functions ()
import           Text.Show.Text.Data.Fixed (showbFixed)
import           Text.Show.Text.Data.Floating (showbEFloat, showbFFloat, showbGFloat)
import           Text.Show.Text.Data.Integral (showbIntAtBase)
import           Text.Show.Text.Data.Version (showbVersionConcrete)

main :: IO ()
main = defaultMain tests

-- | Verifies that a type's @Show@ instances coincide for both 'String's and 'Text',
--   irrespective of precedence.
prop_matchesShow :: (P.Show a, T.Show a, Arbitrary a) => Int -> a -> Bool
prop_matchesShow k x = showsPrec k x "" == unpack (toLazyText $ showbPrec k x)

-- | Verifies 'showFixed' and 'showbFixed' generate the same output.
prop_showFixed :: Bool -> Fixed E12 -> Bool
prop_showFixed b f = fromString (showFixed b f) == showbFixed b f

-- | Verifies 'showIntAtBase' and 'showbIntAtBase' generate the same output.
prop_showIntAtBase :: Gen Bool
prop_showIntAtBase = do
    base <- arbitrary `suchThat` (liftA2 (&&) (> 1) (<= 16))
    i    <- arbitrary `suchThat` (>= 0) :: Gen Int
    return $ fromString (showIntAtBase base intToDigit i "") == showbIntAtBase base intToDigit i

-- | Verifies @showXFloat@ and @showbXFloat@ generate the same output (where @X@
--   is one of E, F, or G).
prop_showXFloat :: (Maybe Int -> Double -> ShowS) -> (Maybe Int -> Double -> Builder) -> Maybe Int -> Double -> Bool
prop_showXFloat f1 f2 digs val = fromString (f1 digs val "") == f2 digs val

-- | Verifies 'showVersion' and 'showbVersion' generate the same output.
prop_showVersion :: Version -> Bool
prop_showVersion v = fromString (showVersion v) == showbVersionConcrete v

tests :: TestTree
tests = testGroup "QuickCheck properties"
          [ testGroup "Text.Show.Text.Control.Applicative"
              [ testProperty "ZipList Int instance"               (prop_matchesShow :: Int -> ZipList Int -> Bool)
              ]
          , testGroup "Text.Show.Text.Control.Concurrent"
              [ testProperty "BlockReason instance"               (prop_matchesShow :: Int -> BlockReason -> Bool)
--               , testProperty "ThreadId instance"                  (prop_matchesShow :: Int -> ThreadId -> Bool)
              , testProperty "ThreadStatus instance"              (prop_matchesShow :: Int -> ThreadStatus -> Bool)
              ]
          , testGroup "Text.Show.Text.Control.Exception"
              [ testProperty "SomeException instance"             (prop_matchesShow :: Int -> SomeException -> Bool)
--               , testProperty "IOException instance"               (prop_matchesShow :: Int -> IOException -> Bool)
              , testProperty "ArithException instance"            (prop_matchesShow :: Int -> ArithException -> Bool)
              , testProperty "ArrayException instance"            (prop_matchesShow :: Int -> ArrayException -> Bool)
              , testProperty "AssertionFailed instance"           (prop_matchesShow :: Int -> AssertionFailed -> Bool)
#if MIN_VERSION_base(4,7,0)
              , testProperty "SomeAsyncException instance"        (prop_matchesShow :: Int -> SomeAsyncException -> Bool)
#endif
              , testProperty "AsyncException instance"            (prop_matchesShow :: Int -> AsyncException -> Bool)
              , testProperty "NonTermination instance"            (prop_matchesShow :: Int -> NonTermination -> Bool)
              , testProperty "NestedAtomically instance"          (prop_matchesShow :: Int -> NestedAtomically -> Bool)
              , testProperty "BlockedIndefinitelyOnMVar instance" (prop_matchesShow :: Int -> BlockedIndefinitelyOnMVar -> Bool)
              , testProperty "BlockedIndefinitelyOnSTM instance"  (prop_matchesShow :: Int -> BlockedIndefinitelyOnSTM -> Bool)
              , testProperty "Deadlock instance"                  (prop_matchesShow :: Int -> Deadlock -> Bool)
              , testProperty "NoMethodError instance"             (prop_matchesShow :: Int -> NoMethodError -> Bool)
              , testProperty "PatternMatchFail instance"          (prop_matchesShow :: Int -> PatternMatchFail -> Bool)
              , testProperty "RecConError instance"               (prop_matchesShow :: Int -> RecConError -> Bool)
              , testProperty "RecSelError instance"               (prop_matchesShow :: Int -> RecSelError -> Bool)
              , testProperty "RecUpdError instance"               (prop_matchesShow :: Int -> RecUpdError -> Bool)
              , testProperty "ErrorCall instance"                 (prop_matchesShow :: Int -> ErrorCall -> Bool)
              ]
          , testGroup "Text.Show.Text.Control.Monad.ST"
              [ testProperty "ST instance"                        (prop_matchesShow :: Int -> ST Int Int -> Bool)
              ]
          , testGroup "Text.Show.Text.Data.Array"
              [ testProperty "Array Int Int instance"             (prop_matchesShow :: Int -> Array Int Int -> Bool)
              ]
          , testGroup "Text.Show.Text.Data.Bool"
              [ testProperty "Bool instance"                      (prop_matchesShow :: Int -> Bool -> Bool)
              ]
          , testGroup "Text.Show.Text.Data.ByteString"
              [ testProperty "strict ByteString instance"         (prop_matchesShow :: Int -> BS.ByteString -> Bool)
              , testProperty "lazy ByteString instance"           (prop_matchesShow :: Int -> BL.ByteString -> Bool)
#if MIN_VERSION_bytestring(0,10,4)
              , testProperty "ShortByteString instance"           (prop_matchesShow :: Int -> ShortByteString -> Bool)
#endif
              ]
          , testGroup "Text.Show.Text.Data.Char"
              [ testProperty "Char instance"                      (prop_matchesShow :: Int -> Char -> Bool)
              , testProperty "GeneralCategory instance"           (prop_matchesShow :: Int -> GeneralCategory -> Bool)
              ]
          , testGroup "Text.Show.Text.Data.Containers"
              [ testProperty "IntMap Int instance"                (prop_matchesShow :: Int -> IntMap Int -> Bool)
              , testProperty "IntSet instance"                    (prop_matchesShow :: Int -> IntSet -> Bool)
              , testProperty "Map Int Int instance"               (prop_matchesShow :: Int -> Map Int Int -> Bool)
              , testProperty "Sequence Int"                       (prop_matchesShow :: Int -> Seq Int -> Bool)
              , testProperty "Set Int instance"                   (prop_matchesShow :: Int -> Set Int -> Bool)
              , testProperty "Tree Int instance"                  (prop_matchesShow :: Int -> Tree Int -> Bool)
              ]
          , testGroup "Text.Show.Text.Data.Data"
              [ testProperty "Constr instance"                    (prop_matchesShow :: Int -> Constr -> Bool)
              , testProperty "ConstrRep instance"                 (prop_matchesShow :: Int -> ConstrRep -> Bool)
              , testProperty "DataRep instance"                   (prop_matchesShow :: Int -> DataRep -> Bool)
              , testProperty "DataType instance"                  (prop_matchesShow :: Int -> DataType -> Bool)
              , testProperty "Fixity instance"                    (prop_matchesShow :: Int -> D.Fixity -> Bool)
              ]
          , testGroup "Text.Show.Text.Data.Dynamic"
              [ testProperty "Dynamic instance"                   (prop_matchesShow :: Int -> Dynamic -> Bool)
              ]
          , testGroup "Text.Show.Text.Data.Either"
              [ testProperty "Either Int Int instance"            (prop_matchesShow :: Int -> Either Int Int -> Bool)
              ]
          , testGroup "Text.Show.Text.Data.Fixed"
              [ testProperty "Fixed E0 instance"                  (prop_matchesShow :: Int -> Fixed E0 -> Bool)
              , testProperty "Fixed E1 instance"                  (prop_matchesShow :: Int -> Fixed E1 -> Bool)
              , testProperty "Fixed E2 instance"                  (prop_matchesShow :: Int -> Fixed E2 -> Bool)
              , testProperty "Fixed E3 instance"                  (prop_matchesShow :: Int -> Fixed E3 -> Bool)
              , testProperty "Fixed E6 instance"                  (prop_matchesShow :: Int -> Fixed E6 -> Bool)
              , testProperty "Fixed E9 instance"                  (prop_matchesShow :: Int -> Fixed E9 -> Bool)
              , testProperty "Fixed E12 instance"                 (prop_matchesShow :: Int -> Fixed E12 -> Bool)
              , testProperty "showFixed output"                   prop_showFixed
              ]
          , testGroup "Text.Show.Text.Data.Floating"
              [ testProperty "Float instance"                     (prop_matchesShow :: Int -> Float -> Bool)
              , testProperty "Double instance"                    (prop_matchesShow :: Int -> Double -> Bool)
              , testProperty "Complex Double instance"            (prop_matchesShow :: Int -> Complex Double -> Bool)
              , testProperty "showbEFloat output" $               prop_showXFloat showEFloat showbEFloat
              , testProperty "showbFFloat output" $               prop_showXFloat showFFloat showbFFloat
              , testProperty "showbGFloat output" $               prop_showXFloat showGFloat showbGFloat
              ]
          , testGroup "Text.Show.Text.Data.Functions"
              [ testProperty "Int -> Int instance"                (prop_matchesShow :: Int -> (Int -> Int) -> Bool)
              ]
          , testGroup "Text.Show.Text.Data.Integral"
              [ testProperty "Int instance"                       (prop_matchesShow :: Int -> Int -> Bool)
              , testProperty "Int8 instance"                      (prop_matchesShow :: Int -> Int8 -> Bool)
              , testProperty "Int16 instance"                     (prop_matchesShow :: Int -> Int16 -> Bool)
              , testProperty "Int32 instance"                     (prop_matchesShow :: Int -> Int32 -> Bool)
              , testProperty "Int64 instance"                     (prop_matchesShow :: Int -> Int64 -> Bool)
              , testProperty "Integer instance"                   (prop_matchesShow :: Int -> Integer -> Bool)
              , testProperty "Word instance"                      (prop_matchesShow :: Int -> Word -> Bool)
              , testProperty "Word8 instance"                     (prop_matchesShow :: Int -> Word8 -> Bool)
              , testProperty "Word16 instance"                    (prop_matchesShow :: Int -> Word16 -> Bool)
              , testProperty "Word32 instance"                    (prop_matchesShow :: Int -> Word32 -> Bool)
              , testProperty "Word64 instance"                    (prop_matchesShow :: Int -> Word64 -> Bool)
              , testProperty "Ratio Int instance"                 (prop_matchesShow :: Int -> Ratio Int -> Bool)
              , testProperty "showbIntAtBase output"              prop_showIntAtBase
              ]
          , testGroup "Text.Show.Text.Data.List"
              [ testProperty "String instance"                    (prop_matchesShow :: Int -> String -> Bool)
              , testProperty "[String] instance"                  (prop_matchesShow :: Int -> [String] -> Bool)
              , testProperty "[Int] instance"                     (prop_matchesShow :: Int -> [Int] -> Bool)
              ]
          , testGroup "Text.Show.Text.Data.Maybe"
              [ testProperty "Maybe Int instance"                 (prop_matchesShow :: Int -> Maybe Int -> Bool)
              ]
          , testGroup "Text.Show.Text.Data.Monoid"
              [ testProperty "All instance"                       (prop_matchesShow :: Int -> All -> Bool)
              , testProperty "Any instance"                       (prop_matchesShow :: Int -> Any -> Bool)
              , testProperty "Dual Int instance"                  (prop_matchesShow :: Int -> Dual Int -> Bool)
              , testProperty "First (Maybe Int) instance"         (prop_matchesShow :: Int -> First (Maybe Int) -> Bool)
              , testProperty "Last (Maybe Int) instance"          (prop_matchesShow :: Int -> Last (Maybe Int) -> Bool)
              , testProperty "Product Int instance"               (prop_matchesShow :: Int -> Product Int -> Bool)
              , testProperty "Sum Int instance"                   (prop_matchesShow :: Int -> Sum Int -> Bool)
              ]
          , testGroup "Text.Show.Text.Data.Ord"
              [ testProperty "Ordering instance"                  (prop_matchesShow :: Int -> Ordering -> Bool)
#if MIN_VERSION_base(4,6,0)
              , testProperty "Down Int instance"                  (prop_matchesShow :: Int -> Down Int -> Bool)
#endif
              ]
          , testGroup "Text.Show.Text.Data.Text"
              [ testProperty "Builder instance"                   (prop_matchesShow :: Int -> Builder -> Bool)
              , testProperty "strict Text instance"               (prop_matchesShow :: Int -> TS.Text -> Bool)
              , testProperty "lazy Text instance"                 (prop_matchesShow :: Int -> TL.Text -> Bool)
              ]
          , testGroup "Text.Show.Text.Data.Time"
              [ testProperty "Day instance"                       (prop_matchesShow :: Int -> Day -> Bool)
              , testProperty "DiffTime instance"                  (prop_matchesShow :: Int -> DiffTime -> Bool)
              , testProperty "UTCTime instance"                   (prop_matchesShow :: Int -> UTCTime -> Bool)
              , testProperty "NominalDiffTime instance"           (prop_matchesShow :: Int -> NominalDiffTime -> Bool)
              , testProperty "AbsoluteTime instance"              (prop_matchesShow :: Int -> AbsoluteTime -> Bool)
              , testProperty "TimeZone instance"                  (prop_matchesShow :: Int -> TimeZone -> Bool)
              , testProperty "TimeOfDay instance"                 (prop_matchesShow :: Int -> TimeOfDay -> Bool)
              , testProperty "LocalTime instance"                 (prop_matchesShow :: Int -> LocalTime -> Bool)
              ]
          , testGroup "Text.Show.Text.Data.Tuple"
              [ testProperty "() instance"                        (prop_matchesShow :: Int -> () -> Bool)
              , testProperty "(Int, Int) instance"                (prop_matchesShow :: Int -> (Int, Int) -> Bool)
              , testProperty "(Int, Int, Int) instance"           (prop_matchesShow :: Int -> (Int, Int, Int) -> Bool)
              , testProperty "(Int, Int, Int, Int) instance"      (prop_matchesShow :: Int -> (Int, Int, Int, Int) -> Bool)
              , testProperty "(Int, Int, Int, Int, Int) instance" (prop_matchesShow :: Int -> (Int, Int, Int, Int, Int) -> Bool)
              ]
#if MIN_VERSION_base(4,7,0)
          , testGroup "Text.Show.Text.Data.Type.Coercion"
              [ testProperty "Coercion instance"                  (prop_matchesShow :: Int -> Coercion All Bool -> Bool)
              ]
          , testGroup "Text.Show.Text.Data.Type.Equality"
              [ testProperty "(:~:) instance"                     (prop_matchesShow :: Int -> Int :~: Int -> Bool)
              ]
#endif
#if MIN_VERSION_base(4,4,0)
          , testGroup "Text.Show.Text.Data.Typeable"
              [ testProperty "TypeRep instance"                   (prop_matchesShow :: Int -> TypeRep -> Bool)
              , testProperty "TyCon instance"                     (prop_matchesShow :: Int -> TyCon -> Bool)
              , testProperty "Fingerprint instance"               (prop_matchesShow :: Int -> Fingerprint -> Bool)
#if MIN_VERSION_base(4,7,0)
              , testProperty "Proxy Int instance"                 (prop_matchesShow :: Int -> Proxy Int -> Bool)
#endif
              ]
#endif
          , testGroup "Text.Show.Text.Data.Version"
              [ testProperty "Version instance"                   (prop_matchesShow :: Int -> Version -> Bool)
              , testProperty "showbVersionConcrete output"        prop_showVersion
              ]
          , testGroup "Text.Show.Text.Foreign.C.Types"
              [ testProperty "CChar"
              (prop_matchesShow :: Int -> CChar -> Bool)
              , testProperty "CSChar instance"                    (prop_matchesShow :: Int -> CSChar -> Bool)
              , testProperty "CUChar instance"                    (prop_matchesShow :: Int -> CUChar -> Bool)
              , testProperty "CShort instance"                    (prop_matchesShow :: Int -> CShort -> Bool)
              , testProperty "CUShort instance"                   (prop_matchesShow :: Int -> CUShort -> Bool)
              , testProperty "CInt instance"                      (prop_matchesShow :: Int -> CInt -> Bool)
              , testProperty "CUInt instance"                     (prop_matchesShow :: Int -> CUInt -> Bool)
              , testProperty "CLong instance"                     (prop_matchesShow :: Int -> CLong -> Bool)
              , testProperty "CULong instance"                    (prop_matchesShow :: Int -> CULong -> Bool)
              , testProperty "CPtrdiff instance"                  (prop_matchesShow :: Int -> CPtrdiff -> Bool)
              , testProperty "CSize instance"                     (prop_matchesShow :: Int -> CSize -> Bool)
              , testProperty "CWchar instance"                    (prop_matchesShow :: Int -> CWchar -> Bool)
              , testProperty "CSigAtomic instance"                (prop_matchesShow :: Int -> CSigAtomic -> Bool)
              , testProperty "CLLong instance"                    (prop_matchesShow :: Int -> CLLong -> Bool)
              , testProperty "CULLong instance"                   (prop_matchesShow :: Int -> CULLong -> Bool)
              , testProperty "CIntPtr instance"                   (prop_matchesShow :: Int -> CIntPtr -> Bool)
              , testProperty "CUIntPtr instance"                  (prop_matchesShow :: Int -> CUIntPtr -> Bool)
              , testProperty "CIntMax instance"                   (prop_matchesShow :: Int -> CIntMax -> Bool)
              , testProperty "CUIntPtr instance"                  (prop_matchesShow :: Int -> CUIntPtr -> Bool)
              , testProperty "CIntMax instance"                   (prop_matchesShow :: Int -> CIntMax -> Bool)
              , testProperty "CUIntMax instance"                  (prop_matchesShow :: Int -> CUIntMax -> Bool)
              , testProperty "CClock instance"                    (prop_matchesShow :: Int -> CClock -> Bool)
              , testProperty "CTime instance"                     (prop_matchesShow :: Int -> CTime -> Bool)
              , testProperty "CUSeconds instance"                 (prop_matchesShow :: Int -> CUSeconds -> Bool)
              , testProperty "CSUSeconds instance"                (prop_matchesShow :: Int -> CSUSeconds -> Bool)
              , testProperty "CFloat instance"                    (prop_matchesShow :: Int -> CFloat -> Bool)
              , testProperty "CDouble instance"                   (prop_matchesShow :: Int -> CUChar -> Bool)
              ]
          , testGroup "Text.Show.Text.Foreign.Ptr"
              [ testProperty "Ptr Int instance"                   (prop_matchesShow :: Int -> Ptr Int -> Bool)
              , testProperty "FunPtr Int instance"                (prop_matchesShow :: Int -> FunPtr Int -> Bool)
              , testProperty "IntPtr instance"                    (prop_matchesShow :: Int -> IntPtr -> Bool)
              , testProperty "WordPtr instance"                   (prop_matchesShow :: Int -> WordPtr -> Bool)
--               , testProperty "ForeignPtr instance"                (prop_matchesShow :: Int -> ForeignPtr Int -> Bool)
              ]
-- #if MIN_VERSION_base(4,4,0)
--           , testGroup "Text.Show.Text.GHC.Event"
--               [ testProperty "Event instance"                     (prop_matchesShow :: Int -> Event -> Bool)
--               , testProperty "FdKey instance"                     (prop_matchesShow :: Int -> FdKey -> Bool)
--               ]
-- #endif
          , testGroup "Text.Show.Text.GHC.Generics"
              [ testProperty "U1 Int instance"                    (prop_matchesShow :: Int -> U1 Int -> Bool)
              , testProperty "Par1 Int instance"                  (prop_matchesShow :: Int -> Par1 Int -> Bool)
              , testProperty "Rec1 Maybe Int instance"            (prop_matchesShow :: Int -> Rec1 Maybe Int -> Bool)
              , testProperty "K1 () Int () instance"              (prop_matchesShow :: Int -> K1 () Int () -> Bool)
              , testProperty "M1 () () Maybe Int instance"        (prop_matchesShow :: Int -> M1 () () Maybe Int -> Bool)
              , testProperty "(Maybe :+: Maybe) Int instance"     (prop_matchesShow :: Int -> (Maybe :+: Maybe) Int -> Bool)
              , testProperty "(Maybe :*: Maybe) Int instance"     (prop_matchesShow :: Int -> (Maybe :*: Maybe) Int -> Bool)
              , testProperty "(Maybe :.: Maybe) Int instance"     (prop_matchesShow :: Int -> (Maybe :.: Maybe) Int -> Bool)
              , testProperty "Fixity instance"                    (prop_matchesShow :: Int -> G.Fixity -> Bool)
              , testProperty "Associativity instance"             (prop_matchesShow :: Int -> Associativity -> Bool)
              , testProperty "Arity instance"                     (prop_matchesShow :: Int -> Arity -> Bool)
              ]
          , testGroup "Text.Show.Text.GHC.Stats"
              [ testProperty "GCStats instance"                   (prop_matchesShow :: Int -> GCStats -> Bool)
              ]
          , testGroup "Text.Show.Text.System.Exit"
              [ testProperty "ExitCode instance"                  (prop_matchesShow :: Int -> ExitCode -> Bool)
              ]
          , testGroup "Text.Show.Text.System.IO"
              [ -- testProperty "Handle instance"                    (prop_matchesShow :: Int -> Handle -> Bool)
                testProperty "IOMode instance"                    (prop_matchesShow :: Int -> IOMode -> Bool)
              , testProperty "BufferMode instance"                (prop_matchesShow :: Int -> BufferMode -> Bool)
--               , testProperty "HandlePosn instance"                (prop_matchesShow :: Int -> HandlePosn -> Bool)
              , testProperty "SeekMode instance"                  (prop_matchesShow :: Int -> SeekMode -> Bool)
-- #if MIN_VERSION_base(4,3,0)
--               , testProperty "TextEncoding"                       (prop_matchesShow :: Int -> TextEncoding -> Bool)
-- #endif
#if MIN_VERSION_base(4,4,0)
              , testProperty "CodingProgress instance"            (prop_matchesShow :: Int -> CodingProgress -> Bool)
              , testProperty "CodingFailureMode instance"         (prop_matchesShow :: Int -> CodingFailureMode -> Bool)
#endif
              , testProperty "Newline instance"                   (prop_matchesShow :: Int -> Newline -> Bool)
              , testProperty "NewlineMode instance"               (prop_matchesShow :: Int -> NewlineMode -> Bool)
              ]
          , testGroup "Text.Show.Text.System.Posix.Types"
              [ testProperty "CDev instance"                      (prop_matchesShow :: Int -> CDev -> Bool)
              , testProperty "CIno instance"                      (prop_matchesShow :: Int -> CIno -> Bool)
              , testProperty "CMode instance"                     (prop_matchesShow :: Int -> CMode -> Bool)
              , testProperty "COff instance"                      (prop_matchesShow :: Int -> COff -> Bool)
              , testProperty "CPid instance"                      (prop_matchesShow :: Int -> CPid -> Bool)
              , testProperty "CSsize instance"                    (prop_matchesShow :: Int -> CSsize -> Bool)
              , testProperty "CGid instance"                      (prop_matchesShow :: Int -> CGid -> Bool)
              , testProperty "CNlink instance"                    (prop_matchesShow :: Int -> CNlink -> Bool)
              , testProperty "CUid instance"                      (prop_matchesShow :: Int -> CUid -> Bool)
              , testProperty "CCc instance"                       (prop_matchesShow :: Int -> CCc -> Bool)
              , testProperty "CSpeed instance"                    (prop_matchesShow :: Int -> CSpeed -> Bool)
              , testProperty "CTcflag instance"                   (prop_matchesShow :: Int -> CTcflag -> Bool)
              , testProperty "CRLim instance"                     (prop_matchesShow :: Int -> CRLim -> Bool)
              , testProperty "Fd instance"                        (prop_matchesShow :: Int -> Fd -> Bool)
              ]
--           , testGroup "Text.Show.Text.Text.Read.Lex"
--               [ testProperty "Lexeme instance"                    (prop_matchesShow :: Int -> Lexeme -> Bool)
--               , testProperty "Number instance"                    (prop_matchesShow :: Int -> Number -> Bool)
--               ]
          ]
         