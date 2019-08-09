{-|
Module:      Spec.System.IOSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "System.IO" module.
-}
module Spec.System.IOSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))

import GHC.IO.Encoding.Failure (CodingFailureMode)
import GHC.IO.Encoding.Types (CodingProgress)

import Instances.System.IO ()

import Prelude ()
import Prelude.Compat

import Spec.Utils (matchesTextShowSpec, prop_matchesTextShow)

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
        matchesTextShowSpec (Proxy :: Proxy Handle)
    describe "IOMode" $
        matchesTextShowSpec (Proxy :: Proxy IOMode)
    describe "BufferMode" $
        matchesTextShowSpec (Proxy :: Proxy BufferMode)
    describe "HandlePosn" $
        matchesTextShowSpec (Proxy :: Proxy HandlePosn)
    describe "SeekMode" $
        matchesTextShowSpec (Proxy :: Proxy SeekMode)
    describe "TextEncoding" $
        prop "TextShow instance" prop_showTextEncoding
    describe "CodingProgress" $
        matchesTextShowSpec (Proxy :: Proxy CodingProgress)
    describe "CodingFailureMode" $
        matchesTextShowSpec (Proxy :: Proxy CodingFailureMode)
    describe "Newline" $
        matchesTextShowSpec (Proxy :: Proxy Newline)
    describe "NewlineMode" $
        matchesTextShowSpec (Proxy :: Proxy NewlineMode)

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
