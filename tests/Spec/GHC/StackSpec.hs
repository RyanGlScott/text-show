{-|
Module:      Spec.GHC.StackSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'CallStack' and 'SrcLoc'.
-}
module Spec.GHC.StackSpec (main, spec) where

import Data.Proxy (Proxy(..))

import GHC.Stack (CallStack, SrcLoc)

import Instances.GHC.Stack ()

import Prelude ()
import Prelude.Compat

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "CallStack" $
        matchesTextShowSpec (Proxy :: Proxy CallStack)
    describe "SrcLoc" $
        matchesTextShowSpec (Proxy :: Proxy SrcLoc)
