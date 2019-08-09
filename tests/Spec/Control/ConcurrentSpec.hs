{-|
Module:      Spec.Control.ConcurrentSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Control.Concurrent" module.
-}
module Spec.Control.ConcurrentSpec (main, spec) where

import Control.Concurrent (myThreadId)

import Data.Proxy.Compat (Proxy(..))

import GHC.Conc (BlockReason, ThreadStatus)

import Instances.Control.Concurrent ()

import Prelude ()
import Prelude.Compat

import Spec.Utils (matchesTextShowSpec, prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, ioProperty)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "BlockReason" $
        matchesTextShowSpec (Proxy :: Proxy BlockReason)
    describe "ThreadId" $
        prop "TextShow instance" prop_showThreadId
    describe "ThreadStatus" $
        matchesTextShowSpec (Proxy :: Proxy ThreadStatus)

-- | Verifies the 'Show' instance for 'ThreadId' is accurate.
prop_showThreadId :: Int -> Property
prop_showThreadId p = ioProperty $ do
    tid <- myThreadId
    pure $ prop_matchesTextShow p tid
