{-# LANGUAGE CPP #-}

{-|
Module:      Spec.System.IOSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "System.IO" module.
-}
module Spec.System.IOSpec (main, spec) where

#if MIN_VERSION_base(4,4,0)
import GHC.IO.Encoding.Failure (CodingFailureMode)
import GHC.IO.Encoding.Types (CodingProgress)
#endif

import Instances.System.IO ()

import Prelude ()
import Prelude.Compat

import Spec.Utils (prop_matchesTextShow)

import System.IO (BufferMode, IOMode, HandlePosn, Newline,
                  NewlineMode, SeekMode, Handle, mkTextEncoding)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, generate, ioProperty, oneof)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Handle" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Handle -> Bool)
    describe "IOMode" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> IOMode -> Bool)
    describe "BufferMode" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> BufferMode -> Bool)
    describe "HandlePosn" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> HandlePosn -> Bool)
    describe "SeekMode" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> SeekMode -> Bool)
    describe "TextEncoding" $
        prop "TextShow instance" prop_showTextEncoding
#if MIN_VERSION_base(4,4,0)
    describe "CodingProgress" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CodingProgress -> Bool)
    describe "CodingFailureMode" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CodingFailureMode -> Bool)
#endif
    describe "Newline" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Newline -> Bool)
    describe "NewlineMode" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> NewlineMode -> Bool)

-- | Verifies the 'TextShow' instance for 'TextEncoding' is accurate.
prop_showTextEncoding :: Int -> Property
prop_showTextEncoding p = ioProperty $ do
    -- Based on this description:
    -- http://hackage.haskell.org/package/base-4.7.0.2/docs/System-IO.html#v:mkTextEncoding
    utf <- generate . oneof $ map pure [ "UTF-8"
                                       , "UTF-16", "UTF-16BE", "UTF-16LE"
                                       , "UTF-32", "UTF-32BE", "UTF-32LE"
                                       ]
    tenc <- mkTextEncoding utf
    pure $ prop_matchesTextShow p tenc
