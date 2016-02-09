{-# LANGUAGE CPP #-}

{-|
Module:      Spec.GHC.StackSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'CallStack' and 'SrcLoc'.
-}
module Spec.GHC.StackSpec (main, spec) where

import Instances.GHC.Stack ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_base(4,9,0)
import GHC.Stack (CallStack, SrcLoc)

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
#if MIN_VERSION_base(4,9,0)
    describe "CallStack" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CallStack -> Bool)
    describe "SrcLoc" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> SrcLoc -> Bool)
#else
    pure ()
#endif
