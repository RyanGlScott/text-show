{-# LANGUAGE CPP #-}

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

import Instances.GHC.Stack ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_base(4,8,1)
import Data.Proxy.Compat (Proxy(..))
import GHC.Stack (CallStack)
# if MIN_VERSION_base(4,9,0)
import GHC.Stack (SrcLoc)
# else
import GHC.SrcLoc (SrcLoc)
# endif

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (describe)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if MIN_VERSION_base(4,8,1)
    describe "CallStack" $
        matchesTextShowSpec (Proxy :: Proxy CallStack)
    describe "SrcLoc" $
        matchesTextShowSpec (Proxy :: Proxy SrcLoc)
#else
    pure ()
#endif
