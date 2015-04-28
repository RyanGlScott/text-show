{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
#endif
{-|
Module:      Spec.BaseAndFriendsSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ tests for data types located in @base@ and other
common libraries.
-}
module Spec.BaseAndFriendsSpec (main, spec) where

import           Control.Applicative (Const, ZipList, liftA2)
import           Control.Concurrent (myThreadId)
import           Control.Exception
import           Control.Monad.ST

#if !defined(mingw32_HOST_OS) && MIN_VERSION_text(1,0,0)
import           Data.Array (Array)
import           Data.Array.Unboxed (UArray)
#endif
import           Data.Array (elems)
import qualified Data.ByteString      as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import           Data.ByteString.Short (ShortByteString)
import           Data.Char (GeneralCategory, intToDigit)
import           Data.Complex (Complex)
import qualified Data.Data as D (Fixity)
import           Data.Data (Constr, ConstrRep, DataRep, DataType)
import           Data.Dynamic (Dynamic)
import           Data.Fixed (Fixed, E0, E1, E2, E3, E6, E9, E12, showFixed)
import           Data.Functor.Identity (Identity)
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.Monoid (All(..), Any(..), Dual(..), First(..),
                              Last(..), Product(..), Sum(..))
#if MIN_VERSION_base(4,8,0)
import           Data.Monoid (Alt(..))
#endif
#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
import qualified Data.OldTypeable as OldT (TyCon, TypeRep)
#endif
#if MIN_VERSION_base(4,6,0)
import           Data.Ord (Down(..))
#endif
import           Data.Orphans ()
import           Data.Proxy (Proxy)
import           Data.Ratio (Ratio)
import qualified Data.Text as TS
import qualified Data.Text as TL
#if MIN_VERSION_text(1,0,0)
import           Data.Text.Encoding (Decoding)
#endif
import           Data.Text.Encoding.Error (UnicodeException)
import           Data.Text.Foreign (I16)
#if MIN_VERSION_text(1,1,0)
import           Data.Text.Internal.Fusion.Size (Size)
#endif
import           Data.Text.Lazy.Builder.RealFloat (FPFormat)
#if MIN_VERSION_base(4,7,0)
import           Data.Type.Coercion (Coercion)
import           Data.Type.Equality ((:~:))
#endif
import qualified Data.Typeable as NewT (TyCon, TypeRep)
import           Data.Word (Word8, Word16, Word32, Word64)
import           Data.Version (Version, showVersion)

-- import qualified Debug.Trace as S (traceShow)

import           Foreign.C.Types
import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Ptr (FunPtr, IntPtr, Ptr, WordPtr)

import           GHC.Conc (BlockReason, ThreadStatus)
#if !(defined(__GHCJS__))
# if defined(mingw32_HOST_OS)
import           GHC.Conc.Windows (ConsoleEvent)
# elif MIN_VERSION_base(4,4,0)
import           GHC.Event (Event)
# endif
#endif
#if MIN_VERSION_base(4,4,0)
import           GHC.Fingerprint.Type (Fingerprint)
#endif
#if __GLASGOW_HASKELL__ >= 702
import qualified GHC.Generics as G (Fixity)
import           GHC.Generics (U1, Par1, Rec1, K1, M1, (:+:), (:*:), (:.:),
                               Associativity, Arity)
#endif
import           GHC.IO.Encoding.Failure (CodingFailureMode)
import           GHC.IO.Encoding.Types (CodingProgress)
#if MIN_VERSION_base(4,8,0)
import           GHC.RTS.Flags
import           GHC.StaticPtr (StaticPtrInfo)
#endif
#if MIN_VERSION_base(4,5,0)
import           GHC.Stats (GCStats)
#endif
import           GHC.Show (asciiTab)
#if MIN_VERSION_base(4,7,0)
import           GHC.TypeLits (SomeNat, SomeSymbol)
#endif

import           Instances.BaseAndFriends ()

import           Numeric (showIntAtBase, showEFloat, showFFloat, showGFloat)
#if MIN_VERSION_base(4,7,0)
import           Numeric (showFFloatAlt, showGFloatAlt)
#endif
import           Numeric.Natural (Natural)

import           Prelude ()
import           Prelude.Compat hiding (Show)

import           Spec.Utils

import           System.Exit (ExitCode)
-- import qualified System.IO as S (print)
import           System.IO (BufferMode, IOMode, HandlePosn, Newline,
                            NewlineMode, SeekMode, Handle, mkTextEncoding)
-- import           System.IO.Silently (capture_, hCapture_)
import           System.Posix.Types

import           Test.Hspec (Spec, describe, hspec, it, parallel, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Gen, Property, arbitrary, generate, oneof, suchThat)
import           Test.QuickCheck.Instances ()

import           Text.Read.Lex (Lexeme)
#if MIN_VERSION_base(4,7,0)
import           Text.Read.Lex (Number)
#endif
import           Text.Show (showListWith)
import           Text.Show.Functions ()
-- import qualified Text.Show.Text as T (print)
import           Text.Show.Text hiding (Show, print)
import           Text.Show.Text.Data.Char (asciiTabB)
import           Text.Show.Text.Data.Fixed (showbFixed)
import           Text.Show.Text.Data.Floating (showbEFloat, showbFFloat, showbGFloat)
#if MIN_VERSION_base(4,7,0)
import           Text.Show.Text.Data.Floating (showbFFloatAlt, showbGFloatAlt)
#endif
import           Text.Show.Text.Data.Integral (showbIntAtBase)
import           Text.Show.Text.Data.List (showbListWith)
import           Text.Show.Text.Data.Version (showbVersionConcrete)
-- import qualified Text.Show.Text.Debug.Trace as T (traceShow)
import           Text.Show.Text.Functions ()
import           Text.Show.Text.Generic (ConType)

#include "HsBaseConfig.h"

main :: IO ()
main = hspec spec

-- -- | Verifies the 'print' functions for 'String' and 'TS.Text' @Show@ display
-- -- the same output.
-- prop_print :: String -> Property
-- prop_print str = ioProperty $ do
--     sRes <- capture_ $ S.print str *> hFlush stdout
--     tRes <- capture_ $ T.print str *> hFlush stdout
--     pure $ sRes == tRes

-- | Verifies 'showFixed' and 'showbFixed' generate the same output.
prop_showFixed :: Bool -> Fixed E12 -> Bool
prop_showFixed b f = fromString (showFixed b f) == showbFixed b f

-- | Verifies the 'Show' instance for 'ForeignPtr' is accurate.
prop_showForeignPtr :: Int -> Ptr Int -> Property
prop_showForeignPtr p ptr = ioProperty $ do
    fptr <- newForeignPtr_ ptr
    pure $ prop_matchesShow p fptr

-- | Verifies 'showIntAtBase' and 'showbIntAtBase' generate the same output.
#if !defined(mingw32_HOST_OS) && MIN_VERSION_text(1,0,0)
prop_showIntAtBase :: Gen Bool
prop_showIntAtBase = do
    base <- arbitrary `suchThat` liftA2 (&&) (> 1) (<= 16)
    i    <- arbitrary `suchThat` (>= 0) :: Gen Int
    pure $ fromString (showIntAtBase base intToDigit i "") == showbIntAtBase base intToDigit i
#endif

-- | Verifies 'showListWith' and 'showbListWith' generate the same output.
prop_showListWith :: String -> Bool
prop_showListWith str = fromString (showListWith shows str "") == showbListWith showb str

-- | Verifies the 'Show' instance for 'TextEncoding' is accurate.
prop_showTextEncoding :: Int -> Property
prop_showTextEncoding p = ioProperty $ do
    -- Based on this description:
    -- http://hackage.haskell.org/package/base-4.7.0.2/docs/System-IO.html#v:mkTextEncoding
    utf <- generate . oneof $ map pure [ "UTF-8"
                                       , "UTF-16", "UTF-16BE", "UTF-16LE"
                                       , "UTF-32", "UTF-32BE", "UTF-32LE"
                                       ]
    tenc <- mkTextEncoding utf
    pure $ prop_matchesShow p tenc

-- | Verifies the 'Show' instance for 'ThreadId' is accurate.
prop_showThreadId :: Int -> Property
prop_showThreadId p = ioProperty $ do
    tid <- myThreadId
    pure $ prop_matchesShow p tid

-- | Verifies @showXFloat@ and @showbXFloat@ generate the same output (where @X@
-- is one of E, F, or G).
prop_showXFloat :: (Maybe Int -> Double -> ShowS) -> (Maybe Int -> Double -> Builder) -> Double -> Gen Bool
prop_showXFloat f1 f2 val = do
    digs <- arbitrary `suchThat` (<= 10)
    pure $ fromString (f1 (Just digs) val "") == f2 (Just digs) val

-- | Verifies 'showVersion' and 'showbVersion' generate the same output.
prop_showVersion :: Version -> Bool
prop_showVersion v = fromString (showVersion v) == showbVersionConcrete v

#if MIN_VERSION_base(4,8,0)
-- | Verifies that the 'Show' instance for 'RTSFlags' is accurate.
prop_showRTSFlags :: Int -> Property
prop_showRTSFlags p = ioProperty $ do
    rtsflags <- getRTSFlags
    pure $ prop_matchesShow p rtsflags

-- | Verifies that the 'Show' instance for 'GCFlags' is accurate.
prop_showGCFlags :: Int -> Property
prop_showGCFlags p = ioProperty $ do
    gcflags <- getGCFlags
    pure $ prop_matchesShow p gcflags

-- | Verifies that the 'Show' instance for 'CCFlags' is accurate.
prop_showCCFlags :: Int -> Property
prop_showCCFlags p = ioProperty $ do
    ccflags <- getCCFlags
    pure $ prop_matchesShow p ccflags

-- | Verifies that the 'Show' instance for 'ProfFlags' is accurate.
prop_showProfFlags :: Int -> Property
prop_showProfFlags p = ioProperty $ do
    profflags <- getProfFlags
    pure $ prop_matchesShow p profflags

-- | Verifies that the 'Show' instance for 'TraceFlags' is accurate.
prop_showTraceFlags :: Int -> Property
prop_showTraceFlags p = ioProperty $ do
    traceflags <- getTraceFlags
    pure $ prop_matchesShow p traceflags
#endif

-- -- | Verifies the 'traceShow' functions for 'String' and 'TS.Text' @Show@ display
-- -- the same output.
-- prop_traceShow :: String -> Property
-- prop_traceShow str = ioProperty $ do
--     let handles = [stdout, stderr]
--     sRes <- hCapture_ handles $ S.traceShow str (pure ()) *> mapM_ hFlush handles
--     tRes <- hCapture_ handles $ T.traceShow str (pure ()) *> mapM_ hFlush handles
--     pure $ sRes == tRes

spec :: Spec
spec = parallel $ do
    describe "Text.Show.Text" $ do
        prop "FromStringShow Int instance"             (prop_matchesShow :: Int -> FromStringShow Int -> Bool)
        prop "FromStringShow Int: read . show = id"    (prop_readShow :: Int -> FromStringShow Int -> Bool)
        prop "FromStringShow Int = Int" $              prop_showEq (FromStringShow :: Int -> FromStringShow Int)
        prop "FromStringShow String instance"          (prop_matchesShow :: Int -> FromStringShow String -> Bool)
        prop "FromStringShow String: read . show = id" (prop_readShow :: Int -> FromStringShow String -> Bool)
        prop "FromStringShow String = String" $        prop_showEq (FromStringShow :: String -> FromStringShow String)
        prop "FromTextShow Int instance"               (prop_matchesShow :: Int -> FromTextShow Int -> Bool)
        prop "FromTextShow Int: read . show = id"      (prop_readShow :: Int -> FromTextShow Int -> Bool)
        prop "FromTextShow Int = Int" $                prop_showEq (FromTextShow :: Int -> FromTextShow Int)
        prop "FromTextShow String instance"            (prop_matchesShow :: Int -> FromTextShow String -> Bool)
        prop "FromTextShow String: read . show = id"   (prop_readShow :: Int -> FromTextShow String -> Bool)
        prop "FromTextShow String = String" $          prop_showEq (FromTextShow :: String -> FromTextShow String)
        -- TODO: Figure out why these fail on NixOS
--         prop "print behavior"                          prop_print
--         prop "traceShow behavior"                      prop_traceShow

    describe "Text.Show.Text.Control.Applicative" $ do
        prop "Const Int Int instance"                  (prop_matchesShow :: Int -> Const Int Int -> Bool)
        prop "ZipList Int instance"                    (prop_matchesShow :: Int -> ZipList Int -> Bool)
#if __GLASGOW_HASKELL__ >= 706
        prop "ZipList Int generic show"                (prop_genericShow :: Int -> ZipList Int -> Bool)
#endif

    describe "Text.Show.Text.Control.Concurrent" $ do
        prop "BlockReason instance"                    (prop_matchesShow :: Int -> BlockReason -> Bool)
        prop "ThreadId instance"                       prop_showThreadId
        prop "ThreadStatus instance"                   (prop_matchesShow :: Int -> ThreadStatus -> Bool)

    describe "Text.Show.Text.Control.Exception" $ do
        prop "SomeException instance"                  (prop_matchesShow :: Int -> SomeException -> Bool)
        prop "IOException instance"                    (prop_matchesShow :: Int -> IOException -> Bool)
        prop "ArithException instance"                 (prop_matchesShow :: Int -> ArithException -> Bool)
        prop "ArrayException instance"                 (prop_matchesShow :: Int -> ArrayException -> Bool)
        prop "AssertionFailed instance"                (prop_matchesShow :: Int -> AssertionFailed -> Bool)
#if MIN_VERSION_base(4,7,0)
        prop "SomeAsyncException instance"             (prop_matchesShow :: Int -> SomeAsyncException -> Bool)
#endif
        prop "AsyncException instance"                 (prop_matchesShow :: Int -> AsyncException -> Bool)
        prop "NonTermination instance"                 (prop_matchesShow :: Int -> NonTermination -> Bool)
        prop "NestedAtomically instance"               (prop_matchesShow :: Int -> NestedAtomically -> Bool)
        prop "BlockedIndefinitelyOnMVar instance"      (prop_matchesShow :: Int -> BlockedIndefinitelyOnMVar -> Bool)
        prop "BlockedIndefinitelyOnSTM instance"       (prop_matchesShow :: Int -> BlockedIndefinitelyOnSTM -> Bool)
#if MIN_VERSION_base(4,8,0)
        prop "AllocationLimitExceeded instance"        (prop_matchesShow :: Int -> AllocationLimitExceeded -> Bool)
#endif
        prop "Deadlock instance"                       (prop_matchesShow :: Int -> Deadlock -> Bool)
        prop "NoMethodError instance"                  (prop_matchesShow :: Int -> NoMethodError -> Bool)
        prop "PatternMatchFail instance"               (prop_matchesShow :: Int -> PatternMatchFail -> Bool)
        prop "RecConError instance"                    (prop_matchesShow :: Int -> RecConError -> Bool)
        prop "RecSelError instance"                    (prop_matchesShow :: Int -> RecSelError -> Bool)
        prop "RecUpdError instance"                    (prop_matchesShow :: Int -> RecUpdError -> Bool)
        prop "ErrorCall instance"                      (prop_matchesShow :: Int -> ErrorCall -> Bool)
        prop "MaskingState instance"                   (prop_matchesShow :: Int -> MaskingState -> Bool)

    describe "Text.Show.Text.Control.Monad.ST" $
        prop "ST instance"                             (prop_matchesShow :: Int -> ST Int Int -> Bool)

#if !defined(mingw32_HOST_OS) && MIN_VERSION_text(1,0,0)
-- TODO: Figure out why this test diverges on Windows
    describe "Text.Show.Text.Data.Array" $ do
        prop "Array Int Int instance"                  (prop_matchesShow :: Int -> Array Int Int -> Bool)
        prop "UArray Int Int instance"                 (prop_matchesShow :: Int -> UArray Int Int -> Bool)
#endif

    describe "Text.Show.Text.Data.Bool" $ do
        prop "Bool instance"                           (prop_matchesShow :: Int -> Bool -> Bool)
#if __GLASGOW_HASKELL__ >= 702
        prop "Bool generic show"                       (prop_genericShow :: Int -> Bool -> Bool)
#endif

    describe "Text.Show.Text.Data.ByteString" $ do
        prop "strict ByteString instance"              (prop_matchesShow :: Int -> BS.ByteString -> Bool)
        prop "lazy ByteString instance"                (prop_matchesShow :: Int -> BL.ByteString -> Bool)
        prop "ShortByteString instance"                (prop_matchesShow :: Int -> ShortByteString -> Bool)

    describe "Text.Show.Text.Data.Char" $ do
        prop "Char instance"                           (prop_matchesShow :: Int -> Char -> Bool)
        prop "GeneralCategory instance"                (prop_matchesShow :: Int -> GeneralCategory -> Bool)
        it   "asciiTab = asciiTabB" $                  map fromString asciiTab `shouldBe` elems asciiTabB

    describe "Text.Show.Text.Data.Complex" $
        prop "Complex Double instance"                 (prop_matchesShow :: Int -> Complex Double -> Bool)

    describe "Text.Show.Text.Data.Data" $ do
        prop "Constr instance"                         (prop_matchesShow :: Int -> Constr -> Bool)
        prop "ConstrRep instance"                      (prop_matchesShow :: Int -> ConstrRep -> Bool)
        prop "DataRep instance"                        (prop_matchesShow :: Int -> DataRep -> Bool)
        prop "DataType instance"                       (prop_matchesShow :: Int -> DataType -> Bool)
        prop "Fixity instance"                         (prop_matchesShow :: Int -> D.Fixity -> Bool)

    describe "Text.Show.Text.Data.Dynamic" $
        prop "Dynamic instance"                        (prop_matchesShow :: Int -> Dynamic -> Bool)

    describe "Text.Show.Text.Data.Either" $ do
        prop "Either Int Int instance"                 (prop_matchesShow :: Int -> Either Int Int -> Bool)
#if __GLASGOW_HASKELL__ >= 702
        prop "Either Int Int generic show"             (prop_genericShow :: Int -> Either Int Int -> Bool)
#endif

    describe "Text.Show.Text.Data.Fixed" $ do
        prop "Fixed E0 instance"                       (prop_matchesShow :: Int -> Fixed E0 -> Bool)
        prop "Fixed E1 instance"                       (prop_matchesShow :: Int -> Fixed E1 -> Bool)
        prop "Fixed E2 instance"                       (prop_matchesShow :: Int -> Fixed E2 -> Bool)
        prop "Fixed E3 instance"                       (prop_matchesShow :: Int -> Fixed E3 -> Bool)
        prop "Fixed E6 instance"                       (prop_matchesShow :: Int -> Fixed E6 -> Bool)
        prop "Fixed E9 instance"                       (prop_matchesShow :: Int -> Fixed E9 -> Bool)
        prop "Fixed E12 instance"                      (prop_matchesShow :: Int -> Fixed E12 -> Bool)
        prop "showFixed output"                        prop_showFixed

    describe "Text.Show.Text.Data.Floating" $ do
        prop "Float instance"                          (prop_matchesShow :: Int -> Float -> Bool)
        prop "Double instance"                         (prop_matchesShow :: Int -> Double -> Bool)
        prop "showbEFloat output" $                    prop_showXFloat showEFloat showbEFloat
        prop "showbFFloat output" $                    prop_showXFloat showFFloat showbFFloat
        prop "showbGFloat output" $                    prop_showXFloat showGFloat showbGFloat
#if MIN_VERSION_base(4,7,0)
        prop "showbFFloatAlt output" $                 prop_showXFloat showFFloatAlt showbFFloatAlt
        prop "showbGFloatAlt output" $                 prop_showXFloat showGFloatAlt showbGFloatAlt
#endif
        prop "FPFormat instance"                       (prop_matchesShow :: Int -> FPFormat -> Bool)

    describe "Text.Show.Text.Data.Functions" $
        prop "Int -> Int instance"                     (prop_matchesShow :: Int -> (Int -> Int) -> Bool)

    describe "Text.Show.Text.Data.Functor.Identity" $
        prop "Identity Int instance"                   (prop_matchesShow :: Int -> Identity Int -> Bool)

    describe "Text.Show.Text.Data.Integral" $ do
        prop "Int instance"                            (prop_matchesShow :: Int -> Int -> Bool)
        prop "Int8 instance"                           (prop_matchesShow :: Int -> Int8 -> Bool)
        prop "Int16 instance"                          (prop_matchesShow :: Int -> Int16 -> Bool)
        prop "Int32 instance"                          (prop_matchesShow :: Int -> Int32 -> Bool)
        prop "Int64 instance"                          (prop_matchesShow :: Int -> Int64 -> Bool)
        prop "Integer instance"                        (prop_matchesShow :: Int -> Integer -> Bool)
        prop "Word instance"                           (prop_matchesShow :: Int -> Word -> Bool)
        prop "Word8 instance"                          (prop_matchesShow :: Int -> Word8 -> Bool)
        prop "Word16 instance"                         (prop_matchesShow :: Int -> Word16 -> Bool)
        prop "Word32 instance"                         (prop_matchesShow :: Int -> Word32 -> Bool)
        prop "Word64 instance"                         (prop_matchesShow :: Int -> Word64 -> Bool)
#if !defined(mingw32_HOST_OS) && MIN_VERSION_text(1,0,0)
-- TODO: Figure out why this diverges on Windows
        prop "showbIntAtBase output"                   prop_showIntAtBase
#endif

    describe "Text.Show.Text.Data.List" $ do
        prop "String instance"                         (prop_matchesShow :: Int -> String -> Bool)
        prop "[String] instance"                       (prop_matchesShow :: Int -> [String] -> Bool)
        prop "[Int] instance"                          (prop_matchesShow :: Int -> [Int] -> Bool)
        prop "showbListWith output"                    prop_showListWith

    describe "Text.Show.Text.Data.Maybe" $
        prop "Maybe Int instance"                      (prop_matchesShow :: Int -> Maybe Int -> Bool)

    describe "Text.Show.Text.Data.Monoid" $ do
        prop "All instance"                            (prop_matchesShow :: Int -> All -> Bool)
        prop "Any instance"                            (prop_matchesShow :: Int -> Any -> Bool)
        prop "Dual Int instance"                       (prop_matchesShow :: Int -> Dual Int -> Bool)
        prop "First (Maybe Int) instance"              (prop_matchesShow :: Int -> First (Maybe Int) -> Bool)
        prop "Last (Maybe Int) instance"               (prop_matchesShow :: Int -> Last (Maybe Int) -> Bool)
        prop "Product Int instance"                    (prop_matchesShow :: Int -> Product Int -> Bool)
        prop "Sum Int instance"                        (prop_matchesShow :: Int -> Sum Int -> Bool)
#if MIN_VERSION_base(4,8,0)
        prop "Alt Maybe Int instance"                  (prop_matchesShow :: Int -> Alt Maybe Int -> Bool)
#endif
#if __GLASGOW_HASKELL__ >= 706
        prop "All generic show"                        (prop_genericShow :: Int -> All -> Bool)
        prop "Any generic show"                        (prop_genericShow :: Int -> Any -> Bool)
        prop "Dual Int generic show"                   (prop_genericShow :: Int -> Dual Int -> Bool)
        prop "First (Maybe Int) generic show"          (prop_genericShow :: Int -> First (Maybe Int) -> Bool)
        prop "Last (Maybe Int) generic show"           (prop_genericShow :: Int -> Last (Maybe Int) -> Bool)
        prop "Product Int generic show"                (prop_genericShow :: Int -> Product Int -> Bool)
        prop "Sum Int generic show"                    (prop_genericShow :: Int -> Sum Int -> Bool)
#endif
#if MIN_VERSION_base(4,8,0)
        prop "Alt Maybe Int generic show"              (prop_genericShow :: Int -> Alt Maybe Int -> Bool)
#endif

#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
    describe "Text.Show.Text.Data.OldTypeable" $ do
        prop "TypeRep instance"                        (prop_matchesShow :: Int -> OldT.TypeRep -> Bool)
        prop "TyCon instance"                          (prop_matchesShow :: Int -> OldT.TyCon -> Bool)
#endif

    describe "Text.Show.Text.Data.Ord" $ do
        prop "Ordering instance"                       (prop_matchesShow :: Int -> Ordering -> Bool)
#if __GLASGOW_HASKELL__ >= 702
        prop "Ordering generic show"                   (prop_genericShow :: Int -> Ordering -> Bool)
#endif
#if MIN_VERSION_base(4,6,0)
        prop "Down Int instance"                       (prop_matchesShow :: Int -> Down Int -> Bool)
#endif

    describe "Text.Show.Text.Data.Proxy" $ do
        prop "Proxy Int instance"                      (prop_matchesShow :: Int -> Proxy Int -> Bool)
--         prop "Proxy Int generic show"                  (prop_genericShow :: Int -> Proxy Int -> Bool)

    describe "Text.Show.Text.Data.Ratio" $ do
        prop "Ratio Int instance"                      (prop_matchesShow :: Int -> Ratio Int -> Bool)

    describe "Text.Show.Text.Data.Text" $ do
        prop "Builder instance"                        (prop_matchesShow :: Int -> Builder -> Bool)
        prop "strict Text instance"                    (prop_matchesShow :: Int -> TS.Text -> Bool)
        prop "lazy Text instance"                      (prop_matchesShow :: Int -> TL.Text -> Bool)
        prop "I16 instance"                            (prop_matchesShow :: Int -> I16 -> Bool)
        prop "UnicodeException instance"               (prop_matchesShow :: Int -> UnicodeException -> Bool)
#if MIN_VERSION_text(1,0,0)
        prop "Decoding instance"                       (prop_matchesShow :: Int -> Decoding -> Bool)
#endif
#if MIN_VERSION_text(1,1,0)
        prop "Size instance"                           (prop_matchesShow :: Int -> Size -> Bool)
#endif

    describe "Text.Show.Text.Data.Tuple" $ do
        prop "() instance"                             (prop_matchesShow :: Int -> () -> Bool)
        prop "(Int, Int) instance"                     (prop_matchesShow :: Int -> (Int, Int) -> Bool)
        prop "(Int, Int, Int) instance"                (prop_matchesShow :: Int -> (Int, Int, Int) -> Bool)
        prop "(Int, Int, Int, Int) instance"           (prop_matchesShow :: Int -> (Int, Int, Int, Int) -> Bool)
        prop "(Int, Int, Int, Int, Int) instance"      (prop_matchesShow :: Int -> (Int, Int, Int, Int, Int) -> Bool)
#if __GLASGOW_HASKELL__ >= 702
        prop "() generic show"                         (prop_genericShow :: Int -> () -> Bool)
        prop "(Int, Int) generic show"                 (prop_genericShow :: Int -> (Int, Int) -> Bool)
        prop "(Int, Int, Int) generic show"            (prop_genericShow :: Int -> (Int, Int, Int) -> Bool)
        prop "(Int, Int, Int, Int) generic show"       (prop_genericShow :: Int -> (Int, Int, Int, Int) -> Bool)
        prop "(Int, Int, Int, Int, Int) generic show"  (prop_genericShow :: Int -> (Int, Int, Int, Int, Int) -> Bool)
#endif

#if MIN_VERSION_base(4,7,0)
    describe "Text.Show.Text.Data.Type.Coercion" $
        prop "Coercion instance"                       (prop_matchesShow :: Int -> Coercion All Bool -> Bool)

    describe "Text.Show.Text.Data.Type.Equality" $
        prop "(:~:) instance"                          (prop_matchesShow :: Int -> Int :~: Int -> Bool)
#endif

    describe "Text.Show.Text.Data.Typeable" $ do
        prop "TypeRep instance"                        (prop_matchesShow :: Int -> NewT.TypeRep -> Bool)
        prop "TyCon instance"                          (prop_matchesShow :: Int -> NewT.TyCon -> Bool)

    describe "Text.Show.Text.Data.Version" $ do
        prop "Version instance"                        (prop_matchesShow :: Int -> Version -> Bool)
        prop "showbVersionConcrete output"             prop_showVersion

    describe "Text.Show.Text.Foreign.C.Types" $ do
        prop "CChar"                                   (prop_matchesShow :: Int -> CChar -> Bool)
        prop "CSChar instance"                         (prop_matchesShow :: Int -> CSChar -> Bool)
        prop "CUChar instance"                         (prop_matchesShow :: Int -> CUChar -> Bool)
        prop "CShort instance"                         (prop_matchesShow :: Int -> CShort -> Bool)
        prop "CUShort instance"                        (prop_matchesShow :: Int -> CUShort -> Bool)
        prop "CInt instance"                           (prop_matchesShow :: Int -> CInt -> Bool)
        prop "CUInt instance"                          (prop_matchesShow :: Int -> CUInt -> Bool)
        prop "CLong instance"                          (prop_matchesShow :: Int -> CLong -> Bool)
        prop "CULong instance"                         (prop_matchesShow :: Int -> CULong -> Bool)
        prop "CPtrdiff instance"                       (prop_matchesShow :: Int -> CPtrdiff -> Bool)
        prop "CSize instance"                          (prop_matchesShow :: Int -> CSize -> Bool)
        prop "CWchar instance"                         (prop_matchesShow :: Int -> CWchar -> Bool)
        prop "CSigAtomic instance"                     (prop_matchesShow :: Int -> CSigAtomic -> Bool)
        prop "CLLong instance"                         (prop_matchesShow :: Int -> CLLong -> Bool)
        prop "CULLong instance"                        (prop_matchesShow :: Int -> CULLong -> Bool)
        prop "CIntPtr instance"                        (prop_matchesShow :: Int -> CIntPtr -> Bool)
        prop "CUIntPtr instance"                       (prop_matchesShow :: Int -> CUIntPtr -> Bool)
        prop "CIntMax instance"                        (prop_matchesShow :: Int -> CIntMax -> Bool)
        prop "CUIntPtr instance"                       (prop_matchesShow :: Int -> CUIntPtr -> Bool)
        prop "CIntMax instance"                        (prop_matchesShow :: Int -> CIntMax -> Bool)
        prop "CUIntMax instance"                       (prop_matchesShow :: Int -> CUIntMax -> Bool)
        prop "CClock instance"                         (prop_matchesShow :: Int -> CClock -> Bool)
        prop "CTime instance"                          (prop_matchesShow :: Int -> CTime -> Bool)
#if MIN_VERSION_base(4,4,0)
        prop "CUSeconds instance"                      (prop_matchesShow :: Int -> CUSeconds -> Bool)
        prop "CSUSeconds instance"                     (prop_matchesShow :: Int -> CSUSeconds -> Bool)
#endif
        prop "CFloat instance"                         (prop_matchesShow :: Int -> CFloat -> Bool)
        prop "CDouble instance"                        (prop_matchesShow :: Int -> CUChar -> Bool)

    describe "Text.Show.Text.Foreign.Ptr" $ do
        prop "Ptr Int instance"                        (prop_matchesShow :: Int -> Ptr Int -> Bool)
        prop "FunPtr Int instance"                     (prop_matchesShow :: Int -> FunPtr Int -> Bool)
        prop "IntPtr instance"                         (prop_matchesShow :: Int -> IntPtr -> Bool)
        prop "WordPtr instance"                        (prop_matchesShow :: Int -> WordPtr -> Bool)
        prop "ForeignPtr instance"                     prop_showForeignPtr

    describe "Text.Show.Text.Generic" $ do
        prop "ConType instance"                        (prop_matchesShow :: Int -> ConType -> Bool)
        prop "ConType generic show"                    (prop_genericShow :: Int -> ConType -> Bool)

#if !(defined(__GHCJS__))
# if defined(mingw32_HOST_OS)
    describe "Text.Show.Text.GHC.Conc.Windows" $
        prop "ConsoleEvent instance"                   (prop_matchesShow :: Int -> ConsoleEvent -> Bool)
# elif MIN_VERSION_base(4,4,0)
    describe "Text.Show.Text.GHC.Event" $ do
        prop "Event instance"                          (prop_matchesShow :: Int -> Event -> Bool)
--         prop "FdKey instance"                          (prop_matchesShow :: Int -> FdKey -> Bool)
# endif
#endif

#if MIN_VERSION_base(4,4,0)
    describe "Text.Show.Text.GHC.Fingerprint" $
        prop "Fingerprint instance"                    (prop_matchesShow :: Int -> Fingerprint -> Bool)
#endif

#if __GLASGOW_HASKELL__ >= 702
    describe "Text.Show.Text.GHC.Generics" $ do
        prop "Fixity instance"                         (prop_matchesShow :: Int -> G.Fixity -> Bool)
        prop "Associativity instance"                  (prop_matchesShow :: Int -> Associativity -> Bool)
        prop "Arity instance"                          (prop_matchesShow :: Int -> Arity -> Bool)
        prop "U1 Int instance"                         (prop_matchesShow :: Int -> U1 Int -> Bool)
        prop "Par1 Int instance"                       (prop_matchesShow :: Int -> Par1 Int -> Bool)
        prop "Rec1 Maybe Int instance"                 (prop_matchesShow :: Int -> Rec1 Maybe Int -> Bool)
        prop "K1 () Int () instance"                   (prop_matchesShow :: Int -> K1 () Int () -> Bool)
        prop "M1 () () Maybe Int instance"             (prop_matchesShow :: Int -> M1 () () Maybe Int -> Bool)
        prop "(Maybe :+: Maybe) Int instance"          (prop_matchesShow :: Int -> (Maybe :+: Maybe) Int -> Bool)
        prop "(Maybe :*: Maybe) Int instance"          (prop_matchesShow :: Int -> (Maybe :*: Maybe) Int -> Bool)
        prop "(Maybe :.: Maybe) Int instance"          (prop_matchesShow :: Int -> (Maybe :.: Maybe) Int -> Bool)
        prop "Fixity generic show"                     (prop_genericShow :: Int -> G.Fixity -> Bool)
        prop "Associativity generic show"              (prop_genericShow :: Int -> Associativity -> Bool)
        prop "Arity generic show"                      (prop_genericShow :: Int -> Arity -> Bool)
        prop "U1 Int generic show"                     (prop_genericShow :: Int -> U1 Int -> Bool)
# if __GLASGOW_HASKELL__ >= 706
        prop "Par1 Int generic show"                   (prop_genericShow :: Int -> Par1 Int -> Bool)
        prop "Rec1 Maybe Int generic show"             (prop_genericShow :: Int -> Rec1 Maybe Int -> Bool)
        prop "K1 () Int () generic show"               (prop_genericShow :: Int -> K1 () Int () -> Bool)
        prop "M1 () () Maybe Int generic show"         (prop_genericShow :: Int -> M1 () () Maybe Int -> Bool)
        prop "(Maybe :+: Maybe) Int generic show"      (prop_genericShow :: Int -> (Maybe :+: Maybe) Int -> Bool)
        prop "(Maybe :*: Maybe) Int generic show"      (prop_genericShow :: Int -> (Maybe :*: Maybe) Int -> Bool)
        prop "(Maybe :.: Maybe) Int generic show"      (prop_genericShow :: Int -> (Maybe :.: Maybe) Int -> Bool)
# endif
#endif

#if MIN_VERSION_base(4,8,0)
    describe "Text.Show.Text.GHC.RTS.Flags" $ do
        prop "RTSFlags instance"                       prop_showRTSFlags
        prop "GCFlags instance"                        prop_showGCFlags
        prop "ConcFlags instance"                      (prop_matchesShow :: Int -> ConcFlags -> Bool)
        prop "MiscFlags instance"                      (prop_matchesShow :: Int -> MiscFlags -> Bool)
        prop "DebugFlags instance"                     (prop_matchesShow :: Int -> DebugFlags -> Bool)
        prop "CCFlags instance"                        prop_showCCFlags
        prop "ProfFlags instance"                      prop_showProfFlags
        prop "TraceFlags instance"                     prop_showTraceFlags
        prop "TickyFlags instance"                     (prop_matchesShow :: Int -> TickyFlags -> Bool)

    describe "Text.Show.Text.GHC.StaticPtr" $
        prop "StaticPtrInfo instance"                  (prop_matchesShow :: Int -> StaticPtrInfo -> Bool)
#endif

#if MIN_VERSION_base(4,5,0)
    describe "Text.Show.Text.GHC.Stats" $
        prop "GCStats instance"                        (prop_matchesShow :: Int -> GCStats -> Bool)
#endif

#if MIN_VERSION_base(4,6,0)
    describe "Text.Show.Text.GHC.TypeLits" $ do
# if MIN_VERSION_base(4,7,0)
        prop "SomeNat instance"                        (prop_matchesShow :: Int -> SomeNat -> Bool)
        prop "SomeSymbol instance"                     (prop_matchesShow :: Int -> SomeSymbol -> Bool)
-- # else
--         prop "IsEven instance"                         (prop_matchesShow :: Int -> IsEven -> Bool)
--         prop "IsZero instance"                         (prop_matchesShow :: Int -> IsZero -> Bool)
# endif
#endif

    describe "Text.Show.Text.Numeric.Natural" $
        prop "Natural instance"                        (prop_matchesShow :: Int -> Natural -> Bool)

    describe "Text.Show.Text.System.Exit" $
        prop "ExitCode instance"                       (prop_matchesShow :: Int -> ExitCode -> Bool)

    describe "Text.Show.Text.System.IO" $ do
        prop "Handle instance"                         (prop_matchesShow :: Int -> Handle -> Bool)
        prop "IOMode instance"                         (prop_matchesShow :: Int -> IOMode -> Bool)
        prop "BufferMode instance"                     (prop_matchesShow :: Int -> BufferMode -> Bool)
        prop "HandlePosn instance"                     (prop_matchesShow :: Int -> HandlePosn -> Bool)
        prop "SeekMode instance"                       (prop_matchesShow :: Int -> SeekMode -> Bool)
        prop "TextEncoding instance"                   prop_showTextEncoding
        prop "CodingProgress instance"                 (prop_matchesShow :: Int -> CodingProgress -> Bool)
        prop "CodingFailureMode instance"              (prop_matchesShow :: Int -> CodingFailureMode -> Bool)
        prop "Newline instance"                        (prop_matchesShow :: Int -> Newline -> Bool)
        prop "NewlineMode instance"                    (prop_matchesShow :: Int -> NewlineMode -> Bool)

    describe "Text.Show.Text.System.Posix.Types" $ do
        prop "Fd instance"                             (prop_matchesShow :: Int -> Fd -> Bool)
#if defined(HTYPE_DEV_T)
        prop "CDev instance"                           (prop_matchesShow :: Int -> CDev -> Bool)
#endif
#if defined(HTYPE_INO_T)
        prop "CIno instance"                           (prop_matchesShow :: Int -> CIno -> Bool)
#endif
#if defined(HTYPE_MODE_T)
        prop "CMode instance"                          (prop_matchesShow :: Int -> CMode -> Bool)
#endif
#if defined(HTYPE_OFF_T)
        prop "COff instance"                           (prop_matchesShow :: Int -> COff -> Bool)
#endif
#if defined(HTYPE_PID_T)
        prop "CPid instance"                           (prop_matchesShow :: Int -> CPid -> Bool)
#endif
#if defined(HTYPE_SSIZE_T)
        prop "CSsize instance"                         (prop_matchesShow :: Int -> CSsize -> Bool)
#endif
#if defined(HTYPE_GID_T)
        prop "CGid instance"                           (prop_matchesShow :: Int -> CGid -> Bool)
#endif
#if defined(HTYPE_NLINK_T)
        prop "CNlink instance"                         (prop_matchesShow :: Int -> CNlink -> Bool)
#endif
#if defined(HTYPE_UID_T)
        prop "CUid instance"                           (prop_matchesShow :: Int -> CUid -> Bool)
#endif
#if defined(HTYPE_CC_T)
        prop "CCc instance"                            (prop_matchesShow :: Int -> CCc -> Bool)
#endif
#if defined(HTYPE_SPEED_T)
        prop "CSpeed instance"                         (prop_matchesShow :: Int -> CSpeed -> Bool)
#endif
#if defined(HTYPE_TCFLAG_T)
        prop "CTcflag instance"                        (prop_matchesShow :: Int -> CTcflag -> Bool)
#endif
#if defined(HTYPE_RLIM_T)
        prop "CRLim instance"                          (prop_matchesShow :: Int -> CRLim -> Bool)
#endif

    describe "Text.Show.Text.Text.Read.Lex" $ do
        prop "Lexeme instance"                         (prop_matchesShow :: Int -> Lexeme -> Bool)
#if MIN_VERSION_base(4,7,0)
        prop "Number instance"                         (prop_matchesShow :: Int -> Number -> Bool)
#endif
