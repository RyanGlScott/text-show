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

import           Control.DeepSeq (NFData)

import           Criterion.Main (Benchmark, bench, bgroup, defaultMain, nf)

import           Data.List (foldl')
import qualified Data.Text.Lazy as T (pack)
import qualified Data.Text      as S
import qualified Data.Text.Lazy.Builder as LBuilder

import           GHC.Generics (Generic)

import           TextShow (TextShow(..))
import           TextShow.Generic (genericShowbPrec)
import           TextShow.TH (deriveTextShow)

main :: IO ()
main = defaultMain
    [ sampleGroup "String Show"                 BTLeaf1 BTBranch1 BTEmpty1 show
    , sampleGroup "String Show, then Text.pack" BTLeaf1 BTBranch1 BTEmpty1 (T.pack . show)
    , sampleGroup "Text Show (TH)"              BTLeaf2 BTBranch2 BTEmpty2 showtl
    , sampleGroup "Text Show (generics)"        BTLeaf3 BTBranch3 BTEmpty3 showtl
    , bgroup "Enumeration Type (strict Text)"
      [ bench "String Show, then Text.pack"  $ nf (S.pack . show) Violet
      , bench "Text Show (TH)" $ nf showt Violet
      , bench "Text Show (showtPrec lambda)" $ nf showt (Color2 Violet)
      , bench "Text Show (showtPrec non-lambda)" $ nf showt (Color3 Violet)
      , bench "Manually written text show"  $ nf manualColorRendering Violet
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

------------------------------------------
-- Simple enumeration types
-------------------------------------------

data Color = Red | Green | Blue | Orange | Violet
  deriving (Show)

newtype Color2 = Color2 Color
newtype Color3 = Color3 Color

instance TextShow Color2 where
  showb = error "Color2: did not write this"
  showt = showtPrec 0
  showtPrec = \_ (Color2 c) -> case c of
    Red    -> S.pack "Red"
    Green  -> S.pack "Green"
    Blue   -> S.pack "Blue"
    Orange -> S.pack "Orange"
    Violet -> S.pack "Violet"

instance TextShow Color3 where
  showb = error "Color2: did not write this"
  showt = showtPrec 0
  showtPrec _ (Color3 c) = case c of
    Red    -> S.pack "Red"
    Green  -> S.pack "Green"
    Blue   -> S.pack "Blue"
    Orange -> S.pack "Orange"
    Violet -> S.pack "Violet"

manualColorRendering :: Color -> S.Text
manualColorRendering c = case c of
  Red    -> S.pack "Red"
  Green  -> S.pack "Green"
  Blue   -> S.pack "Blue"
  Orange -> S.pack "Orange"
  Violet -> S.pack "Violet"


$(deriveTextShow ''BinTree2)

$(deriveTextShow ''Color)

