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
import Properties.Derived (derivedTests)

import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree = testGroup "QuickCheck properties" $ baseAndFriendsTests ++ derivedTests