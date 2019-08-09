{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.TextSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @text@ library.
-}
module Spec.Data.TextSpec (main, spec) where

import           Data.Proxy.Compat (Proxy(..))

import           Instances.Data.Text ()

import           Spec.Utils (matchesTextShowSpec)

import           Test.Hspec (Spec, describe, hspec, parallel)

import qualified Data.Text as TS
import qualified Data.Text as TL
#if MIN_VERSION_text(1,0,0)
import           Data.Text.Encoding (Decoding)
#endif
import           Data.Text.Encoding.Error (UnicodeException)
#if MIN_VERSION_text(1,1,0)
import           Data.Text.Internal.Fusion.Size (Size)
#endif
import           Data.Text.Lazy.Builder (Builder)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Builder" $
        matchesTextShowSpec (Proxy :: Proxy Builder)
    describe "strict Text" $
        matchesTextShowSpec (Proxy :: Proxy TS.Text)
    describe "lazy Text" $
        matchesTextShowSpec (Proxy :: Proxy TL.Text)
    describe "UnicodeException" $
        matchesTextShowSpec (Proxy :: Proxy UnicodeException)
#if MIN_VERSION_text(1,0,0)
    describe "Decoding" $
        matchesTextShowSpec (Proxy :: Proxy Decoding)
#endif
#if MIN_VERSION_text(1,1,0)
    describe "Size" $
        matchesTextShowSpec (Proxy :: Proxy Size)
#endif
