{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Data.Containers
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' functions for data types in the @containers@ library.
-- These are included for convenience (and because @containers@ is a
-- dependency of this library).
----------------------------------------------------------------------------
module Text.Show.Text.Data.Containers (
      showbIntMapPrec
    , showbIntSetPrec
    , showbMapPrec
    , showbSequencePrec
    , showbSetPrec
    , showbTreePrec
    ) where

import qualified Data.Foldable as F
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
import           Data.Tree (Tree(..))

import           GHC.Show (appPrec)

import           Prelude hiding (Show)

import           Text.Show.Text.Class (Show(showb, showbPrec), showbParen)
import           Text.Show.Text.Data.Integral ()
import           Text.Show.Text.Data.List ()
import           Text.Show.Text.Data.Tuple ()
import           Text.Show.Text.Utils ((<>), s)

-- | Convert an 'IntMap' into a 'Builder' with the given precedence.
showbIntMapPrec :: Show v => Int -> IntMap v -> Builder
showbIntMapPrec p im
    = showbParen (p > appPrec) $ "fromList " <> showb (IM.toList  im)
{-# INLINE showbIntMapPrec #-}

-- | Convert an 'IntSet' into a 'Builder' with the given precedence.
showbIntSetPrec :: Int -> IntSet -> Builder
showbIntSetPrec p is
    = showbParen (p > appPrec) $ "fromList " <> showb (IS.toList  is)
{-# INLINE showbIntSetPrec #-}

-- | Convert a 'Map' into a 'Builder' with the given precedence.
showbMapPrec :: (Show k, Show v) => Int -> Map k v -> Builder
showbMapPrec p m
    = showbParen (p > appPrec) $ "fromList " <> showb (M.toList    m)
{-# INLINE showbMapPrec #-}

-- | Convert a 'Sequence' into a 'Builder' with the given precedence.
showbSequencePrec :: Show a => Int -> Seq a -> Builder
showbSequencePrec p s'
    = showbParen (p > appPrec) $ "fromList " <> showb (F.toList   s')
{-# INLINE showbSequencePrec #-}

-- | Convert a 'Set' into a 'Builder' with the given precedence.
showbSetPrec :: Show a => Int -> Set a -> Builder
showbSetPrec p s'
    = showbParen (p > appPrec) $ "fromList " <> showb (Set.toList s')
{-# INLINE showbSetPrec #-}

-- | Convert a 'Tree' into a 'Builder' with the given precedence.
showbTreePrec :: Show a => Int -> Tree a -> Builder
showbTreePrec p (Node rl sf) = showbParen (p > appPrec) $
        "Node {rootLabel = "
     <> showb rl
     <> ", subForest = "
     <> showb sf
     <> s '}'
{-# INLINE showbTreePrec #-}

instance Show v => Show (IntMap v) where
    showbPrec = showbIntMapPrec
    {-# INLINE showbPrec #-}

instance Show IntSet where
    showbPrec = showbIntSetPrec
    {-# INLINE showbPrec #-}

instance (Show k, Show v) => Show (Map k v) where
    showbPrec = showbMapPrec
    {-# INLINE showbPrec #-}

instance Show a => Show (Seq a) where
    showbPrec = showbSequencePrec
    {-# INLINE showbPrec #-}

instance Show a => Show (Set a) where
    showbPrec = showbSetPrec
    {-# INLINE showbPrec #-}

instance Show a => Show (Tree a) where
    showbPrec = showbTreePrec
    {-# INLINE showbPrec #-}