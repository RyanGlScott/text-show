{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

#if __GLASGOW_HASKELL__ < 706
-- Template Haskell's name generation didn't name-mangle very well prior to GHC
-- 7.6 and can cause name shadowing warnings, so suppress them.
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
#endif
{-|
Module:      Spec.MkShowSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@hspec@ test for 'mkShowbPrec' in "Text.Show.Text.TH".
-}
module Spec.MkShowSpec (main, spec) where

-- #if !(MIN_VERSION_base(4,8,0))
-- import           Control.Applicative ((*>), pure)
-- #endif

-- import           Debug.Trace (traceShow)

import           Derived (AllAtOnce)
#if MIN_VERSION_template_haskell(2,7,0)
import           Derived (NotAllShow(..), OneDataInstance)
#endif

import           Instances.Derived ()

-- import           Spec.Utils (ioProperty)

-- import           System.IO (hFlush, stdout, stderr)
-- import           System.IO.Silently (capture_, hCapture_)

import           Test.Hspec (Spec, describe, hspec, parallel)
import           Test.Hspec.QuickCheck (prop)

import qualified Text.Show as S (Show)
import           Text.Show.Text (Builder, FromStringShow(..), showbPrec)
-- import           Text.Show.Text.Debug.Trace.TH (mkTraceShow)
import           Text.Show.Text.TH (mkShowbPrec)

main :: IO ()
main = hspec spec

-- | Verifies 'mkShowbPrec' produces the same output as 'showsPrec'.
prop_mkShowbPrec :: S.Show a
                 => (Int -> a -> Builder) -- ^ TH-generated 'mkShowbPrec' function
                 -> Int -> a -> Bool
prop_mkShowbPrec sf p x = showbPrec p (FromStringShow x) == sf p x

-- -- | Verifies 'mkPrint' produces the same output as 'print'.
-- prop_mkPrint :: S.Show a
--              => (a -> IO ()) -- ^ TH-generated 'mkPrint' function
--              -> a -> Property
-- prop_mkPrint pf x = ioProperty $ do
--     sRes <- capture_ $ print x *> hFlush stdout
--     tRes <- capture_ $ pf    x *> hFlush stdout
--     pure $ sRes == tRes
-- 
-- -- | Verifies 'mkTraceShow' produces the same output as 'traceShow'.
-- prop_mkTraceShow :: S.Show a
--                  => (a -> IO () -> IO ()) -- ^ TH-generated 'mkTraceShow' function
--                  -> a -> Property
-- prop_mkTraceShow tsf x = ioProperty $ do
--     let handles = [stdout, stderr]
--     sRes <- hCapture_ handles $ traceShow x (pure ()) *> mapM_ hFlush handles
--     tRes <- hCapture_ handles $ tsf       x (pure ()) *> mapM_ hFlush handles
--     pure $ sRes == tRes

-- | Verifies 'mkShowbPrec' produces the same output as 'showsPrec' .
-- This uses a plain type constructor.
prop_mkShowbPrecTyCon :: Int -> AllAtOnce Int Int Int Int -> Bool
prop_mkShowbPrecTyCon = prop_mkShowbPrec $(mkShowbPrec ''AllAtOnce)

-- -- | Verifies 'mkPrint' produces the same output as 'print'.
-- -- This uses a plain type constructor.
-- prop_mkPrintTyCon :: AllAtOnce Int Int Int Int -> Property
-- prop_mkPrintTyCon = prop_mkPrint $(mkPrint ''AllAtOnce)
-- 
-- -- | Verifies 'mkTraceShow' produces the same output as 'traceShow'.
-- -- This uses a plain type constructor.
-- prop_mkTraceShowTyCon :: AllAtOnce Int Int Int Int -> Property
-- prop_mkTraceShowTyCon = prop_mkTraceShow $(mkTraceShow ''AllAtOnce)

#if MIN_VERSION_template_haskell(2,7,0)
-- | Verifies 'mkShowbPrec' produces the same output as 'showsPrec'.
-- This uses a data family name.
prop_mkShowbPrecDataFam :: Int -> OneDataInstance Int Int Int Int -> Bool
prop_mkShowbPrecDataFam = prop_mkShowbPrec $(mkShowbPrec ''OneDataInstance)

-- -- | Verifies 'mkPrint' produces the same output as 'print'.
-- -- This uses a data family name.
-- prop_mkPrintDataFam :: OneDataInstance Int Int Int Int -> Property
-- prop_mkPrintDataFam = prop_mkPrint $(mkPrint ''OneDataInstance)
-- 
-- -- | Verifies 'mkTraceShow' produces the same output as 'traceShow'.
-- -- This uses a data family name.
-- prop_mkTraceShowDataFam :: OneDataInstance Int Int Int Int -> Property
-- prop_mkTraceShowDataFam = prop_mkTraceShow $(mkTraceShow ''OneDataInstance)

-- | Verifies 'mkShowbPrec' produces the same output as 'showsPrec'.
-- This uses a data family instance constructor.
prop_mkShowbPrecDataFamInstCon :: Int -> NotAllShow Int Int Int Int -> Bool
prop_mkShowbPrecDataFamInstCon = prop_mkShowbPrec $(mkShowbPrec 'NASShow1)

-- -- | Verifies 'mkPrint' produces the same output as 'print'.
-- -- This uses a data family instance constructor.
-- prop_mkPrintDataFamInstCon :: NotAllShow Int Int Int Int -> Property
-- prop_mkPrintDataFamInstCon = prop_mkPrint $(mkPrint 'NASShow1)
-- 
-- -- | Verifies 'mkTraceShow' produces the same output as 'traceShow'.
-- -- This uses a data family instance constructor.
-- prop_mkTraceShowDataFamInstCon :: NotAllShow Int Int Int Int -> Property
-- prop_mkTraceShowDataFamInstCon = prop_mkTraceShow $(mkTraceShow 'NASShow1)
#endif

spec :: Spec
spec = parallel . describe "mkShow and related functions" $ do
    prop "$(mkShowbPrec ''AllAtOnce) (a plain type constructor)"            prop_mkShowbPrecTyCon
--     prop "$(mkPrint ''AllAtOnce) (a plain type constructor)"                prop_mkPrintTyCon
--     prop "$(mkTraceShow ''AllAtOnce) (a plain type constructor)"            prop_mkTraceShowTyCon
#if MIN_VERSION_template_haskell(2,7,0)
    prop "$(mkShowbPrec ''NotAllShow) (a data family instance constructor)" prop_mkShowbPrecDataFamInstCon
--     prop "$(mkPrint ''NotAllShow) (a data family instance constructor)"     prop_mkPrintDataFamInstCon
--     prop "$(mkTraceShow ''NotAllShow) (a data family instance constructor)" prop_mkTraceShowDataFamInstCon
    prop "$(mkShowbPrec ''OneDataInstance) (a data family name)"            prop_mkShowbPrecDataFam
--     prop "$(mkPrint ''OneDataInstance) (a data family name)"                prop_mkPrintDataFam
--     prop "$(mkTraceShow ''OneDataInstance) (a data family name)"            prop_mkTraceShowDataFam
#endif
