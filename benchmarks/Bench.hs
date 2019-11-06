{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-|
Module:      Bench
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Benchmarks for @text-show@.
-}
module Main (main) where

import           Control.DeepSeq (NFData)

import           Criterion.Main (Benchmark, bench, bgroup, defaultMain, nf)

import           Data.List (foldl')
import qualified Data.Text as T

import           GHC.Generics (Generic)

import           TextShow (TextShow(..))
import           TextShow.Generic (genericShowbPrec, genericShowtPrec, genericShowtlPrec)
import           TextShow.TH (deriveTextShow)

-------------------------------------------------------------------------------
-- Tree-like ADTs
-------------------------------------------------------------------------------

-- NB: constructors must be same length!
data BinTree1 a = BTEmpty1
                | BTLeaf1 a
                | BTBranch1 (BinTree1 a) (BinTree1 a)
  deriving Show

data BinTree2 a = BTEmpty2
                | BTLeaf2 a
                | BTBranch2 (BinTree2 a) (BinTree2 a)

data BinTree3 a = BTEmpty3
                | BTLeaf3 a
                | BTBranch3 (BinTree3 a) (BinTree3 a)
  deriving Generic

instance TextShow a => TextShow (BinTree3 a) where
    showbPrec = genericShowbPrec

-------------------------------------------------------------------------------
-- Simple enumeration types
-------------------------------------------------------------------------------

data Color = Red | Green | Blue | Orange | Violet
  deriving (Generic, Show)

newtype Color2 = Color2 Color

instance TextShow Color2 where
    showbPrec  p (Color2 c) = genericShowbPrec  p c
    showtPrec  p (Color2 c) = genericShowtPrec  p c
    showtlPrec p (Color2 c) = genericShowtlPrec p c

colorShowt :: Color -> T.Text
colorShowt c = case c of
  Red    -> T.pack "Red"
  Green  -> T.pack "Green"
  Blue   -> T.pack "Blue"
  Orange -> T.pack "Orange"
  Violet -> T.pack "Violet"

-------------------------------------------------------------------------------

$(deriveTextShow ''BinTree2)
$(deriveTextShow ''Color)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
    [ sampleGroup "String Show"                 BTLeaf1 BTBranch1 BTEmpty1 show
    , sampleGroup "String Show, then Text.pack" BTLeaf1 BTBranch1 BTEmpty1 (T.pack . show)
    , sampleGroup "TextShow (TH)"               BTLeaf2 BTBranch2 BTEmpty2 showt
    , sampleGroup "TextShow (generics)"         BTLeaf3 BTBranch3 BTEmpty3 showt
    , bgroup "Enumeration type"
      [ bench "String Show"                 $ nf show            Violet
      , bench "String Show, then Text.pack" $ nf (T.pack . show) Violet
      , bench "TextShow (TH)"               $ nf showt           Violet
      , bench "TextShow (generics)"         $ nf showt         $ Color2 Violet
      , bench "Manually written showt"      $ nf colorShowt      Violet
      ]
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

type Sample = forall a b.
    ( Int -> a
    , a -> a -> a
    , a
    , a -> b
    ) -> b

smallSample :: Sample
smallSample (leaf, branch, _, showFun) =
    showFun $ sampleTree leaf branch
{-# NOINLINE smallSample #-}

mediumSample :: Sample
mediumSample (leaf, branch, empty, showFun) =
    showFun . foldl' branch empty . replicate 1000 $ sampleTree leaf branch
{-# NOINLINE mediumSample #-}

largeSample :: Sample
largeSample (leaf, branch, empty, showFun) =
    showFun . foldl' branch empty . replicate 100000 $ sampleTree leaf branch
{-# NOINLINE largeSample #-}

sampleTree :: (Int -> a) -> (a -> a -> a) -> a
sampleTree leaf branch =
    (leaf 12345 `branch` leaf 1234) `branch`
    leaf 123456 `branch`
    (leaf 1234567 `branch` leaf 123456)
