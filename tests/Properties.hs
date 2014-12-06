-----------------------------------------------------------------------------
-- |
-- Module      :  Properties
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- @QuickCheck@ tests for @text-show@.
----------------------------------------------------------------------------
module Main (main) where

import Properties.BaseAndFriends (baseAndFriendsTests)
import Properties.Derived        (derivedTests)
import Properties.MkShow         (mkShowTests)

import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain testTree

allTests :: [TestTree]
allTests = concat [ baseAndFriendsTests
                  , derivedTests
                  , mkShowTests
                  ]

testTree :: TestTree
testTree = testGroup "QuickCheck properties" allTests