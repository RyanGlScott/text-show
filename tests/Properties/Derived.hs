{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Properties.BaseAndFriends
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- @QuickCheck@ properties for data types that have derived 'Show' instances
-- (using "Text.Show.Text.TH").
----------------------------------------------------------------------------
module Properties.Derived (derivedTests) where

import Data.Text.Lazy.Builder (fromString)

import Instances.Derived

import Properties.Utils (prop_matchesShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text (showb, showbPrec)

-- | Verifies that the two 'Show' instances of 'GADT' coincide.
prop_showGADT :: Int    -- The precedence to show with
              -> Double -- The argument to 'GADTCon2'
              -> Int    -- The argument to 'GADTCon3'
              -> Bool
prop_showGADT p d i
    = let gc1 :: GADT Char Int
          gc1 = GADTCon1
          
          gc2 :: GADT Double Double
          gc2 = GADTCon2 d
          
          gc3 :: GADT Int String
          gc3 = GADTCon3 i
      in fromString (show gc1)              == showb gc1
         && fromString (showsPrec p gc2 "") == showbPrec p gc2
         && fromString (showsPrec p gc3 "") == showbPrec p gc3

derivedTests :: [TestTree]
derivedTests =
    [ testGroup "Template Haskell-derived data types"
        [ testProperty "Nullary instance"                            (prop_matchesShow :: Int -> Nullary -> Bool)
        , testProperty "PhantomNullary Int instance"                 (prop_matchesShow :: Int -> PhantomNullary Int -> Bool)
        , testProperty "MonomorphicUnary instance"                   (prop_matchesShow :: Int -> MonomorphicUnary -> Bool)
        , testProperty "PolymorphicUnary Int Int instance"           (prop_matchesShow :: Int -> PolymorphicUnary Int Int -> Bool)
        , testProperty "MonomorphicNewtype instance"                 (prop_matchesShow :: Int -> MonomorphicNewtype -> Bool)
        , testProperty "PolymorphicNewtype Int Int instance"         (prop_matchesShow :: Int -> PolymorphicNewtype Int Int -> Bool)
        , testProperty "MonomorphicProduct instance"                 (prop_matchesShow :: Int -> MonomorphicProduct -> Bool)
        , testProperty "PolymorphicProduct Int Int Int Int instance" (prop_matchesShow :: Int -> PolymorphicProduct Int Int Int Int -> Bool)
        , testProperty "MonomorphicRecord instance"                  (prop_matchesShow :: Int -> MonomorphicRecord -> Bool)
        , testProperty "PolymorphicRecord Int Int Int Int instance"  (prop_matchesShow :: Int -> PolymorphicRecord Int Int Int Int -> Bool)
        , testProperty "MonomorphicInfix instance"                   (prop_matchesShow :: Int -> MonomorphicInfix -> Bool)
        , testProperty "PolymorphicInfix Int Int Int instance"       (prop_matchesShow :: Int -> PolymorphicInfix Int Int Int -> Bool)
        , testProperty "AllAtOnce Int Int Int Int instance"          (prop_matchesShow :: Int -> AllAtOnce Int Int Int Int -> Bool)
        , testProperty "GADT instance"                               prop_showGADT
        , testProperty "LeftAssocTree Int instance"                  (prop_matchesShow :: Int -> LeftAssocTree Int -> Bool)
        , testProperty "RightAssocTree Int instance"                 (prop_matchesShow :: Int -> RightAssocTree Int -> Bool)
        , testProperty "Int :?: Int instance"                        (prop_matchesShow :: Int -> Int :?: Int -> Bool)
        ]
    ]