{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.ArraySpec
Copyright:   (C) 2014-2017 Ryan Scott
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

#if !defined(mingw32_HOST_OS)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Proxy (Proxy(..))

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (describe)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if !defined(mingw32_HOST_OS)
-- TODO: Figure out why these tests diverge on Windows
    describe "Array Int Int" $
        matchesTextShowSpec (Proxy :: Proxy (Array Int Int))
    describe "UArray Int Int" $
        matchesTextShowSpec (Proxy :: Proxy (UArray Int Int))
#else
    pure ()
#endif
