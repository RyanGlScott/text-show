{-# LANGUAGE CPP #-}

{-|
Module:      Spec.System.IOSpec
Copyright:   (C) 2014-2015 Ryan Scott
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

import Spec.Utils (ioProperty, prop_matchesShow)

import System.IO (BufferMode, IOMode, HandlePosn, Newline,
                  NewlineMode, SeekMode, Handle, mkTextEncoding)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, generate, oneof)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.System.IO" $ do
    prop "Handle instance"            (prop_matchesShow :: Int -> Handle -> Bool)
    prop "IOMode instance"            (prop_matchesShow :: Int -> IOMode -> Bool)
    prop "BufferMode instance"        (prop_matchesShow :: Int -> BufferMode -> Bool)
    prop "HandlePosn instance"        (prop_matchesShow :: Int -> HandlePosn -> Bool)
    prop "SeekMode instance"          (prop_matchesShow :: Int -> SeekMode -> Bool)
    prop "TextEncoding instance"      prop_showTextEncoding
#if MIN_VERSION_base(4,4,0)
    prop "CodingProgress instance"    (prop_matchesShow :: Int -> CodingProgress -> Bool)
    prop "CodingFailureMode instance" (prop_matchesShow :: Int -> CodingFailureMode -> Bool)
#endif
    prop "Newline instance"           (prop_matchesShow :: Int -> Newline -> Bool)
    prop "NewlineMode instance"       (prop_matchesShow :: Int -> NewlineMode -> Bool)

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
