{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
#endif
{-|
Module:      Properties.BaseAndFriends
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types located in @base@ and other
common libraries.
-}
module Properties.BaseAndFriends (baseAndFriendsTests) where

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
# else
import           GHC.Event (Event)
# endif
#endif
import           GHC.Fingerprint.Type (Fingerprint)
import qualified GHC.Generics as G (Fixity)
import           GHC.Generics (U1, Par1, Rec1, K1, M1, (:+:), (:*:), (:.:),
                               Associativity, Arity)
import           GHC.IO.Encoding.Failure (CodingFailureMode)
import           GHC.IO.Encoding.Types (CodingProgress)
#if MIN_VERSION_base(4,8,0)
import           GHC.RTS.Flags
import           GHC.StaticPtr (StaticPtrInfo)
#endif
import           GHC.Stats (GCStats)
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

import           Properties.Utils

import           System.Exit (ExitCode)
-- import qualified System.IO as S (print)
import           System.IO (BufferMode, IOMode, HandlePosn, Newline,
                            NewlineMode, SeekMode, Handle, mkTextEncoding)
-- import           System.IO.Silently (capture_, hCapture_)
import           System.Posix.Types

import           Test.QuickCheck.Instances ()
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit ((@=?), testCase)
import           Test.Tasty.QuickCheck (Gen, Property, arbitrary, generate,
                                        oneof, suchThat, testProperty)

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

baseAndFriendsTests :: [TestTree]
baseAndFriendsTests =
    [ testGroup "Text.Show.Text"
        [ testProperty "FromStringShow Int instance"             (prop_matchesShow :: Int -> FromStringShow Int -> Bool)
        , testProperty "FromStringShow Int: read . show = id"    (prop_readShow :: Int -> FromStringShow Int -> Bool)
        , testProperty "FromStringShow Int = Int" $              prop_showEq (FromStringShow :: Int -> FromStringShow Int)
        , testProperty "FromStringShow String instance"          (prop_matchesShow :: Int -> FromStringShow String -> Bool)
        , testProperty "FromStringShow String: read . show = id" (prop_readShow :: Int -> FromStringShow String -> Bool)
        , testProperty "FromStringShow String = String" $        prop_showEq (FromStringShow :: String -> FromStringShow String)
        , testProperty "FromTextShow Int instance"               (prop_matchesShow :: Int -> FromTextShow Int -> Bool)
        , testProperty "FromTextShow Int: read . show = id"      (prop_readShow :: Int -> FromTextShow Int -> Bool)
        , testProperty "FromTextShow Int = Int" $                prop_showEq (FromTextShow :: Int -> FromTextShow Int)
        , testProperty "FromTextShow String instance"            (prop_matchesShow :: Int -> FromTextShow String -> Bool)
        , testProperty "FromTextShow String: read . show = id"   (prop_readShow :: Int -> FromTextShow String -> Bool)
        , testProperty "FromTextShow String = String" $          prop_showEq (FromTextShow :: String -> FromTextShow String)
        -- TODO: Figure out why these fail on NixOS
--         , testProperty "print behavior"                          prop_print
--         , testProperty "traceShow behavior"                      prop_traceShow
        ]
    , testGroup "Text.Show.Text.Control.Applicative"
        [ testProperty "Const Int Int instance"                  (prop_matchesShow :: Int -> Const Int Int -> Bool)
        , testProperty "ZipList Int instance"                    (prop_matchesShow :: Int -> ZipList Int -> Bool)
#if __GLASGOW_HASKELL__ >= 706
        , testProperty "ZipList Int generic show"                (prop_genericShow :: Int -> ZipList Int -> Bool)
#endif
        ]
    , testGroup "Text.Show.Text.Control.Concurrent"
        [ testProperty "BlockReason instance"                    (prop_matchesShow :: Int -> BlockReason -> Bool)
        , testProperty "ThreadId instance"                       prop_showThreadId
        , testProperty "ThreadStatus instance"                   (prop_matchesShow :: Int -> ThreadStatus -> Bool)
        ]
    , testGroup "Text.Show.Text.Control.Exception"
        [ testProperty "SomeException instance"                  (prop_matchesShow :: Int -> SomeException -> Bool)
        , testProperty "IOException instance"                    (prop_matchesShow :: Int -> IOException -> Bool)
        , testProperty "ArithException instance"                 (prop_matchesShow :: Int -> ArithException -> Bool)
        , testProperty "ArrayException instance"                 (prop_matchesShow :: Int -> ArrayException -> Bool)
        , testProperty "AssertionFailed instance"                (prop_matchesShow :: Int -> AssertionFailed -> Bool)
#if MIN_VERSION_base(4,7,0)
        , testProperty "SomeAsyncException instance"             (prop_matchesShow :: Int -> SomeAsyncException -> Bool)
#endif
        , testProperty "AsyncException instance"                 (prop_matchesShow :: Int -> AsyncException -> Bool)
        , testProperty "NonTermination instance"                 (prop_matchesShow :: Int -> NonTermination -> Bool)
        , testProperty "NestedAtomically instance"               (prop_matchesShow :: Int -> NestedAtomically -> Bool)
        , testProperty "BlockedIndefinitelyOnMVar instance"      (prop_matchesShow :: Int -> BlockedIndefinitelyOnMVar -> Bool)
        , testProperty "BlockedIndefinitelyOnSTM instance"       (prop_matchesShow :: Int -> BlockedIndefinitelyOnSTM -> Bool)
#if MIN_VERSION_base(4,8,0)
        , testProperty "AllocationLimitExceeded instance"        (prop_matchesShow :: Int -> AllocationLimitExceeded -> Bool)
#endif
        , testProperty "Deadlock instance"                       (prop_matchesShow :: Int -> Deadlock -> Bool)
        , testProperty "NoMethodError instance"                  (prop_matchesShow :: Int -> NoMethodError -> Bool)
        , testProperty "PatternMatchFail instance"               (prop_matchesShow :: Int -> PatternMatchFail -> Bool)
        , testProperty "RecConError instance"                    (prop_matchesShow :: Int -> RecConError -> Bool)
        , testProperty "RecSelError instance"                    (prop_matchesShow :: Int -> RecSelError -> Bool)
        , testProperty "RecUpdError instance"                    (prop_matchesShow :: Int -> RecUpdError -> Bool)
        , testProperty "ErrorCall instance"                      (prop_matchesShow :: Int -> ErrorCall -> Bool)
        , testProperty "MaskingState instance"                   (prop_matchesShow :: Int -> MaskingState -> Bool)
        ]
    , testGroup "Text.Show.Text.Control.Monad.ST"
        [ testProperty "ST instance"                             (prop_matchesShow :: Int -> ST Int Int -> Bool)
        ]
#if !defined(mingw32_HOST_OS) && MIN_VERSION_text(1,0,0)
-- TODO: Figure out why this test diverges on Windows
    , testGroup "Text.Show.Text.Data.Array"
        [ testProperty "Array Int Int instance"                  (prop_matchesShow :: Int -> Array Int Int -> Bool)
        , testProperty "UArray Int Int instance"                 (prop_matchesShow :: Int -> UArray Int Int -> Bool)
        ]
#endif
    , testGroup "Text.Show.Text.Data.Bool"
        [ testProperty "Bool instance"                           (prop_matchesShow :: Int -> Bool -> Bool)
        , testProperty "Bool generic show"                       (prop_genericShow :: Int -> Bool -> Bool)
        ]
    , testGroup "Text.Show.Text.Data.ByteString"
        [ testProperty "strict ByteString instance"              (prop_matchesShow :: Int -> BS.ByteString -> Bool)
        , testProperty "lazy ByteString instance"                (prop_matchesShow :: Int -> BL.ByteString -> Bool)
        , testProperty "ShortByteString instance"                (prop_matchesShow :: Int -> ShortByteString -> Bool)
        ]
    , testGroup "Text.Show.Text.Data.Char"
        [ testProperty "Char instance"                           (prop_matchesShow :: Int -> Char -> Bool)
        , testProperty "GeneralCategory instance"                (prop_matchesShow :: Int -> GeneralCategory -> Bool)
        , testCase "asciiTab = asciiTabB" $                      map fromString asciiTab @=? elems asciiTabB
        ]
    , testGroup "Text.Show.Text.Data.Complex"
        [ testProperty "Complex Double instance"                 (prop_matchesShow :: Int -> Complex Double -> Bool)
        ]
    , testGroup "Text.Show.Text.Data.Data"
        [ testProperty "Constr instance"                         (prop_matchesShow :: Int -> Constr -> Bool)
        , testProperty "ConstrRep instance"                      (prop_matchesShow :: Int -> ConstrRep -> Bool)
        , testProperty "DataRep instance"                        (prop_matchesShow :: Int -> DataRep -> Bool)
        , testProperty "DataType instance"                       (prop_matchesShow :: Int -> DataType -> Bool)
        , testProperty "Fixity instance"                         (prop_matchesShow :: Int -> D.Fixity -> Bool)
        ]
    , testGroup "Text.Show.Text.Data.Dynamic"
        [ testProperty "Dynamic instance"                        (prop_matchesShow :: Int -> Dynamic -> Bool)
        ]
    , testGroup "Text.Show.Text.Data.Either"
        [ testProperty "Either Int Int instance"                 (prop_matchesShow :: Int -> Either Int Int -> Bool)
        , testProperty "Either Int Int generic show"             (prop_genericShow :: Int -> Either Int Int -> Bool)
        ]
    , testGroup "Text.Show.Text.Data.Fixed"
        [ testProperty "Fixed E0 instance"                       (prop_matchesShow :: Int -> Fixed E0 -> Bool)
        , testProperty "Fixed E1 instance"                       (prop_matchesShow :: Int -> Fixed E1 -> Bool)
        , testProperty "Fixed E2 instance"                       (prop_matchesShow :: Int -> Fixed E2 -> Bool)
        , testProperty "Fixed E3 instance"                       (prop_matchesShow :: Int -> Fixed E3 -> Bool)
        , testProperty "Fixed E6 instance"                       (prop_matchesShow :: Int -> Fixed E6 -> Bool)
        , testProperty "Fixed E9 instance"                       (prop_matchesShow :: Int -> Fixed E9 -> Bool)
        , testProperty "Fixed E12 instance"                      (prop_matchesShow :: Int -> Fixed E12 -> Bool)
        , testProperty "showFixed output"                        prop_showFixed
        ]
    , testGroup "Text.Show.Text.Data.Floating"
        [ testProperty "Float instance"                          (prop_matchesShow :: Int -> Float -> Bool)
        , testProperty "Double instance"                         (prop_matchesShow :: Int -> Double -> Bool)
        , testProperty "showbEFloat output" $                    prop_showXFloat showEFloat showbEFloat
        , testProperty "showbFFloat output" $                    prop_showXFloat showFFloat showbFFloat
        , testProperty "showbGFloat output" $                    prop_showXFloat showGFloat showbGFloat
#if MIN_VERSION_base(4,7,0)
        , testProperty "showbFFloatAlt output" $                 prop_showXFloat showFFloatAlt showbFFloatAlt
        , testProperty "showbGFloatAlt output" $                 prop_showXFloat showGFloatAlt showbGFloatAlt
#endif
        , testProperty "FPFormat instance"                       (prop_matchesShow :: Int -> FPFormat -> Bool)
        ]
    , testGroup "Text.Show.Text.Data.Functions"
        [ testProperty "Int -> Int instance"                     (prop_matchesShow :: Int -> (Int -> Int) -> Bool)
        ]
    , testGroup "Text.Show.Text.Data.Functor.Identity"
        [ testProperty "Identity Int instance"                   (prop_matchesShow :: Int -> Identity Int -> Bool)
        ]
    , testGroup "Text.Show.Text.Data.Integral"
        [ testProperty "Int instance"                            (prop_matchesShow :: Int -> Int -> Bool)
        , testProperty "Int8 instance"                           (prop_matchesShow :: Int -> Int8 -> Bool)
        , testProperty "Int16 instance"                          (prop_matchesShow :: Int -> Int16 -> Bool)
        , testProperty "Int32 instance"                          (prop_matchesShow :: Int -> Int32 -> Bool)
        , testProperty "Int64 instance"                          (prop_matchesShow :: Int -> Int64 -> Bool)
        , testProperty "Integer instance"                        (prop_matchesShow :: Int -> Integer -> Bool)
        , testProperty "Word instance"                           (prop_matchesShow :: Int -> Word -> Bool)
        , testProperty "Word8 instance"                          (prop_matchesShow :: Int -> Word8 -> Bool)
        , testProperty "Word16 instance"                         (prop_matchesShow :: Int -> Word16 -> Bool)
        , testProperty "Word32 instance"                         (prop_matchesShow :: Int -> Word32 -> Bool)
        , testProperty "Word64 instance"                         (prop_matchesShow :: Int -> Word64 -> Bool)
#if !defined(mingw32_HOST_OS) && MIN_VERSION_text(1,0,0)
-- TODO: Figure out why this diverges on Windows
        , testProperty "showbIntAtBase output"                   prop_showIntAtBase
#endif
        ]
    , testGroup "Text.Show.Text.Data.List"
        [ testProperty "String instance"                         (prop_matchesShow :: Int -> String -> Bool)
        , testProperty "[String] instance"                       (prop_matchesShow :: Int -> [String] -> Bool)
        , testProperty "[Int] instance"                          (prop_matchesShow :: Int -> [Int] -> Bool)
        , testProperty "showbListWith output"                    prop_showListWith
        ]
    , testGroup "Text.Show.Text.Data.Maybe"
        [ testProperty "Maybe Int instance"                      (prop_matchesShow :: Int -> Maybe Int -> Bool)
        ]
    , testGroup "Text.Show.Text.Data.Monoid"
        [ testProperty "All instance"                            (prop_matchesShow :: Int -> All -> Bool)
        , testProperty "Any instance"                            (prop_matchesShow :: Int -> Any -> Bool)
        , testProperty "Dual Int instance"                       (prop_matchesShow :: Int -> Dual Int -> Bool)
        , testProperty "First (Maybe Int) instance"              (prop_matchesShow :: Int -> First (Maybe Int) -> Bool)
        , testProperty "Last (Maybe Int) instance"               (prop_matchesShow :: Int -> Last (Maybe Int) -> Bool)
        , testProperty "Product Int instance"                    (prop_matchesShow :: Int -> Product Int -> Bool)
        , testProperty "Sum Int instance"                        (prop_matchesShow :: Int -> Sum Int -> Bool)
#if MIN_VERSION_base(4,8,0)
        , testProperty "Alt Maybe Int instance"                  (prop_matchesShow :: Int -> Alt Maybe Int -> Bool)
#endif
#if __GLASGOW_HASKELL__ >= 706
        , testProperty "All generic show"                        (prop_genericShow :: Int -> All -> Bool)
        , testProperty "Any generic show"                        (prop_genericShow :: Int -> Any -> Bool)
        , testProperty "Dual Int generic show"                   (prop_genericShow :: Int -> Dual Int -> Bool)
        , testProperty "First (Maybe Int) generic show"          (prop_genericShow :: Int -> First (Maybe Int) -> Bool)
        , testProperty "Last (Maybe Int) generic show"           (prop_genericShow :: Int -> Last (Maybe Int) -> Bool)
        , testProperty "Product Int generic show"                (prop_genericShow :: Int -> Product Int -> Bool)
        , testProperty "Sum Int generic show"                    (prop_genericShow :: Int -> Sum Int -> Bool)
#endif
#if MIN_VERSION_base(4,8,0)
        , testProperty "Alt Maybe Int generic show"              (prop_genericShow :: Int -> Alt Maybe Int -> Bool)
#endif
        ]
#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
    , testGroup "Text.Show.Text.Data.OldTypeable"
        [ testProperty "TypeRep instance"                        (prop_matchesShow :: Int -> OldT.TypeRep -> Bool)
        , testProperty "TyCon instance"                          (prop_matchesShow :: Int -> OldT.TyCon -> Bool)
        ]
#endif
    , testGroup "Text.Show.Text.Data.Ord"
        [ testProperty "Ordering instance"                       (prop_matchesShow :: Int -> Ordering -> Bool)
        , testProperty "Ordering generic show"                   (prop_genericShow :: Int -> Ordering -> Bool)
#if MIN_VERSION_base(4,6,0)
        , testProperty "Down Int instance"                       (prop_matchesShow :: Int -> Down Int -> Bool)
#endif
        ]
    , testGroup "Text.Show.Text.Data.Proxy"
        [ testProperty "Proxy Int instance"                      (prop_matchesShow :: Int -> Proxy Int -> Bool)
--         , testProperty "Proxy Int generic show"                  (prop_genericShow :: Int -> Proxy Int -> Bool)
        ]
    , testGroup "Text.Show.Text.Data.Ratio"
        [ testProperty "Ratio Int instance"                      (prop_matchesShow :: Int -> Ratio Int -> Bool)
        ]
    , testGroup "Text.Show.Text.Data.Text"
        [ testProperty "Builder instance"                        (prop_matchesShow :: Int -> Builder -> Bool)
        , testProperty "strict Text instance"                    (prop_matchesShow :: Int -> TS.Text -> Bool)
        , testProperty "lazy Text instance"                      (prop_matchesShow :: Int -> TL.Text -> Bool)
        , testProperty "I16 instance"                            (prop_matchesShow :: Int -> I16 -> Bool)
        , testProperty "UnicodeException instance"               (prop_matchesShow :: Int -> UnicodeException -> Bool)
#if MIN_VERSION_text(1,0,0)
        , testProperty "Decoding instance"                       (prop_matchesShow :: Int -> Decoding -> Bool)
#endif
#if MIN_VERSION_text(1,1,0)
        , testProperty "Size instance"                           (prop_matchesShow :: Int -> Size -> Bool)
#endif
        ]
    , testGroup "Text.Show.Text.Data.Tuple"
        [ testProperty "() instance"                             (prop_matchesShow :: Int -> () -> Bool)
        , testProperty "(Int, Int) instance"                     (prop_matchesShow :: Int -> (Int, Int) -> Bool)
        , testProperty "(Int, Int, Int) instance"                (prop_matchesShow :: Int -> (Int, Int, Int) -> Bool)
        , testProperty "(Int, Int, Int, Int) instance"           (prop_matchesShow :: Int -> (Int, Int, Int, Int) -> Bool)
        , testProperty "(Int, Int, Int, Int, Int) instance"      (prop_matchesShow :: Int -> (Int, Int, Int, Int, Int) -> Bool)
        , testProperty "() generic show"                         (prop_genericShow :: Int -> () -> Bool)
        , testProperty "(Int, Int) generic show"                 (prop_genericShow :: Int -> (Int, Int) -> Bool)
        , testProperty "(Int, Int, Int) generic show"            (prop_genericShow :: Int -> (Int, Int, Int) -> Bool)
        , testProperty "(Int, Int, Int, Int) generic show"       (prop_genericShow :: Int -> (Int, Int, Int, Int) -> Bool)
        , testProperty "(Int, Int, Int, Int, Int) generic show"  (prop_genericShow :: Int -> (Int, Int, Int, Int, Int) -> Bool)
        ]
#if MIN_VERSION_base(4,7,0)
    , testGroup "Text.Show.Text.Data.Type.Coercion"
        [ testProperty "Coercion instance"                       (prop_matchesShow :: Int -> Coercion All Bool -> Bool)
        ]
    , testGroup "Text.Show.Text.Data.Type.Equality"
        [ testProperty "(:~:) instance"                          (prop_matchesShow :: Int -> Int :~: Int -> Bool)
        ]
#endif
    , testGroup "Text.Show.Text.Data.Typeable"
        [ testProperty "TypeRep instance"                        (prop_matchesShow :: Int -> NewT.TypeRep -> Bool)
        , testProperty "TyCon instance"                          (prop_matchesShow :: Int -> NewT.TyCon -> Bool)
        ]
    , testGroup "Text.Show.Text.Data.Version"
        [ testProperty "Version instance"                        (prop_matchesShow :: Int -> Version -> Bool)
        , testProperty "showbVersionConcrete output"             prop_showVersion
        ]
    , testGroup "Text.Show.Text.Foreign.C.Types"
        [ testProperty "CChar"                                   (prop_matchesShow :: Int -> CChar -> Bool)
        , testProperty "CSChar instance"                         (prop_matchesShow :: Int -> CSChar -> Bool)
        , testProperty "CUChar instance"                         (prop_matchesShow :: Int -> CUChar -> Bool)
        , testProperty "CShort instance"                         (prop_matchesShow :: Int -> CShort -> Bool)
        , testProperty "CUShort instance"                        (prop_matchesShow :: Int -> CUShort -> Bool)
        , testProperty "CInt instance"                           (prop_matchesShow :: Int -> CInt -> Bool)
        , testProperty "CUInt instance"                          (prop_matchesShow :: Int -> CUInt -> Bool)
        , testProperty "CLong instance"                          (prop_matchesShow :: Int -> CLong -> Bool)
        , testProperty "CULong instance"                         (prop_matchesShow :: Int -> CULong -> Bool)
        , testProperty "CPtrdiff instance"                       (prop_matchesShow :: Int -> CPtrdiff -> Bool)
        , testProperty "CSize instance"                          (prop_matchesShow :: Int -> CSize -> Bool)
        , testProperty "CWchar instance"                         (prop_matchesShow :: Int -> CWchar -> Bool)
        , testProperty "CSigAtomic instance"                     (prop_matchesShow :: Int -> CSigAtomic -> Bool)
        , testProperty "CLLong instance"                         (prop_matchesShow :: Int -> CLLong -> Bool)
        , testProperty "CULLong instance"                        (prop_matchesShow :: Int -> CULLong -> Bool)
        , testProperty "CIntPtr instance"                        (prop_matchesShow :: Int -> CIntPtr -> Bool)
        , testProperty "CUIntPtr instance"                       (prop_matchesShow :: Int -> CUIntPtr -> Bool)
        , testProperty "CIntMax instance"                        (prop_matchesShow :: Int -> CIntMax -> Bool)
        , testProperty "CUIntPtr instance"                       (prop_matchesShow :: Int -> CUIntPtr -> Bool)
        , testProperty "CIntMax instance"                        (prop_matchesShow :: Int -> CIntMax -> Bool)
        , testProperty "CUIntMax instance"                       (prop_matchesShow :: Int -> CUIntMax -> Bool)
        , testProperty "CClock instance"                         (prop_matchesShow :: Int -> CClock -> Bool)
        , testProperty "CTime instance"                          (prop_matchesShow :: Int -> CTime -> Bool)
        , testProperty "CUSeconds instance"                      (prop_matchesShow :: Int -> CUSeconds -> Bool)
        , testProperty "CSUSeconds instance"                     (prop_matchesShow :: Int -> CSUSeconds -> Bool)
        , testProperty "CFloat instance"                         (prop_matchesShow :: Int -> CFloat -> Bool)
        , testProperty "CDouble instance"                        (prop_matchesShow :: Int -> CUChar -> Bool)
        ]
    , testGroup "Text.Show.Text.Foreign.Ptr"
        [ testProperty "Ptr Int instance"                        (prop_matchesShow :: Int -> Ptr Int -> Bool)
        , testProperty "FunPtr Int instance"                     (prop_matchesShow :: Int -> FunPtr Int -> Bool)
        , testProperty "IntPtr instance"                         (prop_matchesShow :: Int -> IntPtr -> Bool)
        , testProperty "WordPtr instance"                        (prop_matchesShow :: Int -> WordPtr -> Bool)
        , testProperty "ForeignPtr instance"                     prop_showForeignPtr
        ]
#if !(defined(mingw32_HOST_OS) || defined(__GHCJS__))
    , testGroup "Text.Show.Text.GHC.Event"
        [ testProperty "Event instance"                          (prop_matchesShow :: Int -> Event -> Bool)
--         , testProperty "FdKey instance"                          (prop_matchesShow :: Int -> FdKey -> Bool)
        ]
#endif
    , testGroup "Text.Show.Text.Generic"
        [ testProperty "ConType instance"                        (prop_matchesShow :: Int -> ConType -> Bool)
        , testProperty "ConType generic show"                    (prop_genericShow :: Int -> ConType -> Bool)
        ]
#if defined(mingw32_HOST_OS) && !(defined(__GHCJS__))
    , testGroup "Text.Show.Text.GHC.Conc.Windows"
        [ testProperty "ConsoleEvent instance"                   (prop_matchesShow :: Int -> ConsoleEvent -> Bool)
        ]
#endif
    , testGroup "Text.Show.Text.GHC.Fingerprint"
        [ testProperty "Fingerprint instance"                    (prop_matchesShow :: Int -> Fingerprint -> Bool)
        ]
    , testGroup "Text.Show.Text.GHC.Generics"
        [ testProperty "Fixity instance"                         (prop_matchesShow :: Int -> G.Fixity -> Bool)
        , testProperty "Associativity instance"                  (prop_matchesShow :: Int -> Associativity -> Bool)
        , testProperty "Arity instance"                          (prop_matchesShow :: Int -> Arity -> Bool)
        , testProperty "U1 Int instance"                         (prop_matchesShow :: Int -> U1 Int -> Bool)
        , testProperty "Par1 Int instance"                       (prop_matchesShow :: Int -> Par1 Int -> Bool)
        , testProperty "Rec1 Maybe Int instance"                 (prop_matchesShow :: Int -> Rec1 Maybe Int -> Bool)
        , testProperty "K1 () Int () instance"                   (prop_matchesShow :: Int -> K1 () Int () -> Bool)
        , testProperty "M1 () () Maybe Int instance"             (prop_matchesShow :: Int -> M1 () () Maybe Int -> Bool)
        , testProperty "(Maybe :+: Maybe) Int instance"          (prop_matchesShow :: Int -> (Maybe :+: Maybe) Int -> Bool)
        , testProperty "(Maybe :*: Maybe) Int instance"          (prop_matchesShow :: Int -> (Maybe :*: Maybe) Int -> Bool)
        , testProperty "(Maybe :.: Maybe) Int instance"          (prop_matchesShow :: Int -> (Maybe :.: Maybe) Int -> Bool)
#if __GLASGOW_HASKELL__ >= 704
--         , testProperty "Fixity generic show"                     (prop_genericShow :: Int -> G.Fixity -> Bool)
--         , testProperty "Associativity generic show"              (prop_genericShow :: Int -> Associativity -> Bool)
--         , testProperty "Arity generic show"                      (prop_genericShow :: Int -> Arity -> Bool)
--         , testProperty "U1 Int generic show"                     (prop_genericShow :: Int -> U1 Int -> Bool)
#endif
#if __GLASGOW_HASKELL__ >= 706
        , testProperty "Par1 Int generic show"                   (prop_genericShow :: Int -> Par1 Int -> Bool)
        , testProperty "Rec1 Maybe Int generic show"             (prop_genericShow :: Int -> Rec1 Maybe Int -> Bool)
        , testProperty "K1 () Int () generic show"               (prop_genericShow :: Int -> K1 () Int () -> Bool)
        , testProperty "M1 () () Maybe Int generic show"         (prop_genericShow :: Int -> M1 () () Maybe Int -> Bool)
        , testProperty "(Maybe :+: Maybe) Int generic show"      (prop_genericShow :: Int -> (Maybe :+: Maybe) Int -> Bool)
        , testProperty "(Maybe :*: Maybe) Int generic show"      (prop_genericShow :: Int -> (Maybe :*: Maybe) Int -> Bool)
        , testProperty "(Maybe :.: Maybe) Int generic show"      (prop_genericShow :: Int -> (Maybe :.: Maybe) Int -> Bool)
#endif
        ]
#if MIN_VERSION_base(4,8,0)
    , testGroup "Text.Show.Text.GHC.RTS.Flags"
        [ testProperty "RTSFlags instance"                       prop_showRTSFlags
        , testProperty "GCFlags instance"                        prop_showGCFlags
        , testProperty "ConcFlags instance"                      (prop_matchesShow :: Int -> ConcFlags -> Bool)
        , testProperty "MiscFlags instance"                      (prop_matchesShow :: Int -> MiscFlags -> Bool)
        , testProperty "DebugFlags instance"                     (prop_matchesShow :: Int -> DebugFlags -> Bool)
        , testProperty "CCFlags instance"                        prop_showCCFlags
        , testProperty "ProfFlags instance"                      prop_showProfFlags
        , testProperty "TraceFlags instance"                     prop_showTraceFlags
        , testProperty "TickyFlags instance"                     (prop_matchesShow :: Int -> TickyFlags -> Bool)
        ]
    , testGroup "Text.Show.Text.GHC.StaticPtr"
        [ testProperty "StaticPtrInfo instance"                  (prop_matchesShow :: Int -> StaticPtrInfo -> Bool)
        ]
#endif
    , testGroup "Text.Show.Text.GHC.Stats"
        [ testProperty "GCStats instance"                        (prop_matchesShow :: Int -> GCStats -> Bool)
        ]
#if MIN_VERSION_base(4,6,0)
    , testGroup "Text.Show.Text.GHC.TypeLits"
        [
# if MIN_VERSION_base(4,7,0)
          testProperty "SomeNat instance"                        (prop_matchesShow :: Int -> SomeNat -> Bool)
        , testProperty "SomeSymbol instance"                     (prop_matchesShow :: Int -> SomeSymbol -> Bool)
-- # else
--           testProperty "IsEven instance"                         (prop_matchesShow :: Int -> IsEven -> Bool)
--         , testProperty "IsZero instance"                         (prop_matchesShow :: Int -> IsZero -> Bool)
# endif
        ]
#endif
    , testGroup "Text.Show.Text.Numeric.Natural"
        [ testProperty "Natural instance"                        (prop_matchesShow :: Int -> Natural -> Bool)
        ]
    , testGroup "Text.Show.Text.System.Exit"
        [ testProperty "ExitCode instance"                       (prop_matchesShow :: Int -> ExitCode -> Bool)
        ]
    , testGroup "Text.Show.Text.System.IO"
        [ testProperty "Handle instance"                         (prop_matchesShow :: Int -> Handle -> Bool)
        , testProperty "IOMode instance"                         (prop_matchesShow :: Int -> IOMode -> Bool)
        , testProperty "BufferMode instance"                     (prop_matchesShow :: Int -> BufferMode -> Bool)
        , testProperty "HandlePosn instance"                     (prop_matchesShow :: Int -> HandlePosn -> Bool)
        , testProperty "SeekMode instance"                       (prop_matchesShow :: Int -> SeekMode -> Bool)
        , testProperty "TextEncoding"                            prop_showTextEncoding
        , testProperty "CodingProgress instance"                 (prop_matchesShow :: Int -> CodingProgress -> Bool)
        , testProperty "CodingFailureMode instance"              (prop_matchesShow :: Int -> CodingFailureMode -> Bool)
        , testProperty "Newline instance"                        (prop_matchesShow :: Int -> Newline -> Bool)
        , testProperty "NewlineMode instance"                    (prop_matchesShow :: Int -> NewlineMode -> Bool)
        ]
    , testGroup "Text.Show.Text.System.Posix.Types"
        [ testProperty "Fd instance"                             (prop_matchesShow :: Int -> Fd -> Bool)
#if defined(HTYPE_DEV_T)
        , testProperty "CDev instance"                           (prop_matchesShow :: Int -> CDev -> Bool)
#endif
#if defined(HTYPE_INO_T)
        , testProperty "CIno instance"                           (prop_matchesShow :: Int -> CIno -> Bool)
#endif
#if defined(HTYPE_MODE_T)
        , testProperty "CMode instance"                          (prop_matchesShow :: Int -> CMode -> Bool)
#endif
#if defined(HTYPE_OFF_T)
        , testProperty "COff instance"                           (prop_matchesShow :: Int -> COff -> Bool)
#endif
#if defined(HTYPE_PID_T)
        , testProperty "CPid instance"                           (prop_matchesShow :: Int -> CPid -> Bool)
#endif
#if defined(HTYPE_SSIZE_T)
        , testProperty "CSsize instance"                         (prop_matchesShow :: Int -> CSsize -> Bool)
#endif
#if defined(HTYPE_GID_T)
        , testProperty "CGid instance"                           (prop_matchesShow :: Int -> CGid -> Bool)
#endif
#if defined(HTYPE_NLINK_T)
        , testProperty "CNlink instance"                         (prop_matchesShow :: Int -> CNlink -> Bool)
#endif
#if defined(HTYPE_UID_T)
        , testProperty "CUid instance"                           (prop_matchesShow :: Int -> CUid -> Bool)
#endif
#if defined(HTYPE_CC_T)
        , testProperty "CCc instance"                            (prop_matchesShow :: Int -> CCc -> Bool)
#endif
#if defined(HTYPE_SPEED_T)
        , testProperty "CSpeed instance"                         (prop_matchesShow :: Int -> CSpeed -> Bool)
#endif
#if defined(HTYPE_TCFLAG_T)
        , testProperty "CTcflag instance"                        (prop_matchesShow :: Int -> CTcflag -> Bool)
#endif
#if defined(HTYPE_RLIM_T)
        , testProperty "CRLim instance"                          (prop_matchesShow :: Int -> CRLim -> Bool)
#endif
        ]
    , testGroup "Text.Show.Text.Text.Read.Lex"
        [ testProperty "Lexeme instance"                         (prop_matchesShow :: Int -> Lexeme -> Bool)
#if MIN_VERSION_base(4,7,0)
        , testProperty "Number instance"                         (prop_matchesShow :: Int -> Number -> Bool)
#endif
        ]
    ]
