{-|
Module:      Spec.Control.ConcurrentSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Control.Concurrent" module.
-}
module Spec.Control.ConcurrentSpec (main, spec) where

import Control.Concurrent (myThreadId)

import GHC.Conc (BlockReason, ThreadStatus)

import Instances.Control.Concurrent ()

import Prelude ()
import Prelude.Compat

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, ioProperty)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "BlockReason" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> BlockReason -> Bool)
    describe "ThreadId" $
        prop "TextShow instance" prop_showThreadId
    describe "ThreadStatus" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> ThreadStatus -> Bool)

-- | Verifies the 'Show' instance for 'ThreadId' is accurate.
prop_showThreadId :: Int -> Property
prop_showThreadId p = ioProperty $ do
    tid <- myThreadId
    pure $ prop_matchesTextShow p tid
