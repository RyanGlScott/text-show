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

import           Spec.Utils (prop_matchesTextShow)

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
spec = parallel $ do
    describe "Builder" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Builder -> Bool)
    describe "strict Text" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> TS.Text -> Bool)
    describe "lazy Text" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> TL.Text -> Bool)
    describe "I16" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> I16 -> Bool)
    describe "UnicodeException" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> UnicodeException -> Bool)
#if MIN_VERSION_text(1,0,0)
    describe "Decoding" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Decoding -> Bool)
#endif
#if MIN_VERSION_text(1,1,0)
    describe "Size" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Size -> Bool)
#endif
