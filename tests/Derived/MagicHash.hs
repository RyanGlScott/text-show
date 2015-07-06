{-# LANGUAGE CPP             #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

{-|
Module:      Derived.MagicHash
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines data types with fields that have unlifted types.
-}
module Derived.MagicHash (TyCon#(..), TyFamily#(..)) where

import GHC.Exts

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

import TextShow.TH (deriveTextShow)

-------------------------------------------------------------------------------

data TyCon# = TyCon# {
    tcInt#    :: Int#
  , tcFloat#  :: Float#
  , tcDouble# :: Double#
  , tcChar#   :: Char#
  , tcWord#   :: Word#
} deriving Show
$(deriveTextShow ''TyCon#)

-------------------------------------------------------------------------------

data family TyFamily#
data instance TyFamily# = TyFamily# {
    tfInt#    :: Int#
  , tfFloat#  :: Float#
  , tfDouble# :: Double#
  , tfChar#   :: Char#
  , tfWord#   :: Word#
} deriving Show
#if MIN_VERSION_template_haskell(2,7,0)
$(deriveTextShow 'TyFamily#)
#endif

-------------------------------------------------------------------------------

instance Arbitrary TyCon# where
    arbitrary = do
        I# i# <- arbitrary
        F# f# <- arbitrary
        D# d# <- arbitrary
        C# c# <- arbitrary
        W# w# <- arbitrary
        pure $ TyCon# i# f# d# c# w#

instance Arbitrary TyFamily# where
    arbitrary = do
        I# i# <- arbitrary
        F# f# <- arbitrary
        D# d# <- arbitrary
        C# c# <- arbitrary
        W# w# <- arbitrary
        pure $ TyFamily# i# f# d# c# w#
