{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-|
Module:      Bench
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Benchmarks for @text-show@.
-}
module Main (main) where

import Control.DeepSeq (NFData)

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, nf)

import Data.List (foldl')

import GHC.Generics (Generic)

import TextShow (TextShow(..))
import TextShow.Generic (genericShowbPrec)
import TextShow.TH (deriveTextShow)

main :: IO ()
main = defaultMain
    [ sampleGroup "String Show"          BTSLeaf    BTSBranch    BTSEmpty    show
    , sampleGroup "Text Show (TH)"       BTTSTHLeaf BTTSTHBranch BTTSTHEmpty showtl
    , sampleGroup "Text Show (generics)" BTTSGLeaf  BTTSGBranch  BTTSGEmpty  showtl
    ]

sampleGroup :: forall a b. NFData b
            => String -> (Int -> a) -> (a -> a -> a) -> a -> (a -> b) -> Benchmark
sampleGroup title leaf branch empty showFun =
    bgroup title
        [ bench "Small sample"  $ nf smallSample  pile
        , bench "Medium sample" $ nf mediumSample pile
        , bench "Large sample"  $ nf largeSample  pile
        ]
  where
    pile :: (Int -> a, a -> a -> a, a, a -> b)
    pile = (leaf, branch, empty, showFun)

type Sample = forall a b. NFData b =>
    ( Int -> a
    , a -> a -> a
    , a
    , a -> b
    ) -> b

smallSample :: Sample
smallSample (leaf, branch, _, showFun) =
    showFun $
        (leaf 12345 `branch` leaf 1234) `branch`
        leaf 123456 `branch`
        (leaf 1234567 `branch` leaf 123456)
{-# NOINLINE smallSample #-}

mediumSample :: Sample
mediumSample (leaf, branch, empty, showFun) =
    showFun . foldl' branch empty . replicate 1000 $
        (leaf 12345 `branch` leaf 1234) `branch`
        leaf 123456 `branch`
        (leaf 1234567 `branch` leaf 123456)
{-# NOINLINE mediumSample #-}

largeSample :: Sample
largeSample (leaf, branch, empty, showFun) =
    showFun . foldl' branch empty . replicate 100000 $
        (leaf 12345 `branch` leaf 1234) `branch`
        leaf 123456 `branch`
        (leaf 1234567 `branch` leaf 123456)
{-# NOINLINE largeSample #-}

data BinTreeShow a = BTSEmpty
                   | BTSLeaf a
                   | BTSBranch (BinTreeShow a) (BinTreeShow a)
  deriving Show

data BinTreeTextShowTH a = BTTSTHEmpty
                         | BTTSTHLeaf a
                         | BTTSTHBranch (BinTreeTextShowTH a)
                                        (BinTreeTextShowTH a)
data BinTreeTextShowGenerics a = BTTSGEmpty
                               | BTTSGLeaf a
                               | BTTSGBranch (BinTreeTextShowGenerics a)
                                             (BinTreeTextShowGenerics a)
  deriving Generic
instance TextShow a => TextShow (BinTreeTextShowGenerics a) where
    showbPrec = genericShowbPrec

$(deriveTextShow ''BinTreeTextShowTH)
