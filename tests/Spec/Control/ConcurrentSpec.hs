module Spec.Control.ConcurrentSpec (main, spec) where

import Control.Concurrent (myThreadId)

import GHC.Conc (BlockReason, ThreadStatus)

import Instances.Control.Concurrent ()

import Prelude ()
import Prelude.Compat

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, ioProperty)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "BlockReason" $
        prop "Show instance" (prop_matchesShow :: Int -> BlockReason -> Bool)
    describe "ThreadId" $
        prop "Show instance" prop_showThreadId
    describe "ThreadStatus" $
        prop "Show instance" (prop_matchesShow :: Int -> ThreadStatus -> Bool)

-- | Verifies the 'Show' instance for 'ThreadId' is accurate.
prop_showThreadId :: Int -> Property
prop_showThreadId p = ioProperty $ do
    tid <- myThreadId
    pure $ prop_matchesShow p tid
