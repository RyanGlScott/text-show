{-|
Module:      Spec.Data.ByteStringSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @bytestring@ library.
-}
module Spec.Data.ByteStringSpec (main, spec) where

import qualified Data.ByteString      as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import           Data.ByteString.Short (ShortByteString)
import           Data.Proxy.Compat (Proxy(..))

import           Spec.Utils (matchesTextShowSpec)

import           Test.Hspec (Spec, describe, hspec, parallel)
import           Test.QuickCheck.Instances ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "strict ByteString" $
        matchesTextShowSpec (Proxy :: Proxy BS.ByteString)
    describe "lazy ByteString" $
        matchesTextShowSpec (Proxy :: Proxy BL.ByteString)
    describe "ShortByteString" $
        matchesTextShowSpec (Proxy :: Proxy ShortByteString)
