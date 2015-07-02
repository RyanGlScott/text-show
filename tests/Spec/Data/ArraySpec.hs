{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.ArraySpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for array data types.
-}
module Spec.Data.ArraySpec (main, spec) where

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)
import Test.QuickCheck.Instances ()

#if !defined(mingw32_HOST_OS) && MIN_VERSION_text(1,0,0)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)

import Spec.Utils (prop_matchesShow)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if !defined(mingw32_HOST_OS) && MIN_VERSION_text(1,0,0)
-- TODO: Figure out why these tests diverge on Windows
    describe "Array Int Int" $
        prop "Show instance" (prop_matchesShow :: Int -> Array Int Int -> Bool)
    describe "UArray Int Int" $
        prop "Show instance" (prop_matchesShow :: Int -> UArray Int Int -> Bool)
#else
    pure ()
#endif
