{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.TextSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @text@ library.
-}
module Spec.Data.TextSpec (main, spec) where

import           Instances.Data.Text ()

import           Spec.Utils (prop_matchesShow)

import           Test.Hspec (Spec, describe, hspec, parallel)
import           Test.Hspec.QuickCheck (prop)

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
import           Data.Text.Lazy.Builder (Builder)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Data.Text" $ do
    prop "Builder instance"          (prop_matchesShow :: Int -> Builder -> Bool)
    prop "strict Text instance"      (prop_matchesShow :: Int -> TS.Text -> Bool)
    prop "lazy Text instance"        (prop_matchesShow :: Int -> TL.Text -> Bool)
    prop "I16 instance"              (prop_matchesShow :: Int -> I16 -> Bool)
    prop "UnicodeException instance" (prop_matchesShow :: Int -> UnicodeException -> Bool)
#if MIN_VERSION_text(1,0,0)
    prop "Decoding instance"         (prop_matchesShow :: Int -> Decoding -> Bool)
#endif
#if MIN_VERSION_text(1,1,0)
    prop "Size instance"             (prop_matchesShow :: Int -> Size -> Bool)
#endif
