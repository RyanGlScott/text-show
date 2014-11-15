{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}
module Text.Show.Text.Containers where

import qualified Data.Foldable as F
import           Data.Monoid ((<>))
import           Data.Text.Lazy.Builder (Builder)

import qualified Data.IntMap as IM
import           Data.IntMap (IntMap)
import qualified Data.IntSet as IS
import           Data.IntSet (IntSet)
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Sequence (Seq)
import qualified Data.Set as Set
import           Data.Set (Set)
-- import qualified Data.Tree as T
-- import           Data.Tree (Tree)

import           GHC.Show (appPrec)

import           Prelude hiding (Show)

import           Text.Show.Text.Class (Show(..), showbParen, showbListDefault)
import           Text.Show.Text.Int ()
import           Text.Show.Text.Tuple ()

showbIntMap :: Show v => Int -> IntMap v -> Builder
showbIntMap p im
    = showbParen (p > appPrec) $ "fromList " <> showbListDefault (IM.toList im)
{-# INLINE showbIntMap #-}

showbIntSet :: Int -> IntSet -> Builder
showbIntSet p is
    = showbParen (p > appPrec) $ "fromList " <> showbListDefault (IS.toList is)
{-# INLINE showbIntSet #-}

showbMap :: (Show k, Show v) => Int -> Map k v -> Builder
showbMap p m
    = showbParen (p > appPrec) $ "fromList " <> showbListDefault (M.toList   m)
{-# INLINE showbMap #-}

showbSequence :: Show a => Int -> Seq a -> Builder
showbSequence p s
    = showbParen (p > appPrec) $ "fromList " <> showbListDefault (F.toList   s)
{-# INLINE showbSequence #-}

showbSet :: Show a => Int -> Set a -> Builder
showbSet p s
    = showbParen (p > appPrec) $ "fromList " <> showbListDefault (Set.toList s)
{-# INLINE showbSet #-}

-- TODO: showbTree (once Text.Show.Text.TH is done)

instance Show v => Show (IntMap v) where
    showbPrec = showbIntMap
    {-# INLINE showbPrec #-}

instance Show IntSet where
    showbPrec = showbIntSet
    {-# INLINE showbPrec #-}

instance (Show k, Show v) => Show (Map k v) where
    showbPrec = showbMap
    {-# INLINE showbPrec #-}

instance Show a => Show (Seq a) where
    showbPrec = showbSequence
    {-# INLINE showbPrec #-}

instance Show a => Show (Set a) where
    showbPrec = showbSet
    {-# INLINE showbPrec #-}