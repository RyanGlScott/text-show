{-|
Module:      TextShow.Data.Typeable.Utils
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Utility functions for showing data types in the @Typeable@ (or @OldTypeable@) module.
-}
module TextShow.Data.Typeable.Utils (showbArgs, showbTuple) where

import Data.Text.Lazy.Builder (Builder, singleton)

import Prelude ()
import Prelude.Compat

import TextShow.Classes (TextShow(..))

-- | Helper function for showing a list of arguments, each separated by the given
-- 'Builder'.
showbArgs :: TextShow a => Builder -> [a] -> Builder
showbArgs _   []     = mempty
showbArgs _   [a]    = showbPrec 10 a
showbArgs sep (a:as) = showbPrec 10 a <> sep <> showbArgs sep as

-- | Helper function for showing a list of 'Show' instances in a tuple.
showbTuple :: TextShow a => [a] -> Builder
showbTuple args = singleton '(' <> showbArgs (singleton ',') args <> singleton ')'
{-# INLINE showbTuple #-}
