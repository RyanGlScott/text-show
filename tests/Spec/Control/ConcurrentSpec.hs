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
spec = parallel . describe "Text.Show.Text.Control.Concurrent" $ do
    prop "BlockReason instance"  (prop_matchesShow :: Int -> BlockReason -> Bool)
    prop "ThreadId instance"     prop_showThreadId
    prop "ThreadStatus instance" (prop_matchesShow :: Int -> ThreadStatus -> Bool)

-- | Verifies the 'Show' instance for 'ThreadId' is accurate.
prop_showThreadId :: Int -> Property
prop_showThreadId p = ioProperty $ do
    tid <- myThreadId
    pure $ prop_matchesShow p tid
