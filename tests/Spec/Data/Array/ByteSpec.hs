{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.Array.ByteSpec
Copyright:   (C) 2022 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'ByteArray' from the "Data.Array.Byte" module.
-}
module Spec.Data.Array.ByteSpec (main, spec) where

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)
import Test.QuickCheck.Instances ()

#if MIN_VERSION_base(4,17,0)
import Data.Array.Byte (ByteArray)
import Data.Proxy.Compat (Proxy(..))

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (describe)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if MIN_VERSION_base(4,17,0)
    describe "ByteArray" $
        matchesTextShowSpec (Proxy :: Proxy ByteArray)
#else
    pure ()
#endif
