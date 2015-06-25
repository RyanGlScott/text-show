{-|
Module:      Spec.FromStringTextShowSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'FromStringShow' and 'FromTextShow'.
-}
module Spec.FromStringTextShowSpec (main, spec) where

-- import qualified Debug.Trace as S (traceShow)

import           Instances.FromStringTextShow ()

import           Spec.Utils (prop_matchesShow, prop_readShow, prop_showEq)

-- import qualified System.IO as S (print)
-- import           System.IO.Silently (capture_, hCapture_)

import           Test.Hspec (Spec, describe, hspec, parallel)
import           Test.Hspec.QuickCheck (prop)

import           Text.Show.Text (FromStringShow(..), FromTextShow(..))
-- import qualified Text.Show.Text as T (print)
-- import qualified Text.Show.Text.Debug.Trace as T (traceShow)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text" $ do
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

-- -- | Verifies the 'print' functions for 'String' and 'TS.Text' @Show@ display
-- -- the same output.
-- prop_print :: String -> Property
-- prop_print str = ioProperty $ do
--     sRes <- capture_ $ S.print str *> hFlush stdout
--     tRes <- capture_ $ T.print str *> hFlush stdout
--     pure $ sRes == tRes

-- -- | Verifies the 'traceShow' functions for 'String' and 'TS.Text' @Show@ display
-- -- the same output.
-- prop_traceShow :: String -> Property
-- prop_traceShow str = ioProperty $ do
--     let handles = [stdout, stderr]
--     sRes <- hCapture_ handles $ S.traceShow str (pure ()) *> mapM_ hFlush handles
--     tRes <- hCapture_ handles $ T.traceShow str (pure ()) *> mapM_ hFlush handles
--     pure $ sRes == tRes
