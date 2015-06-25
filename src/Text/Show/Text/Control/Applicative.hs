{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Control.Applicative
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'ZipList'.

/Since: 0.3/
-}
module Text.Show.Text.Control.Applicative (showbConstPrecWith, showbZipListPrecWith) where

import Control.Applicative (Const(..), ZipList)

import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showbPrec), Show1(..), Show2(..), showbUnaryWith)
import Text.Show.Text.Data.List ()
import Text.Show.Text.TH.Internal (deriveShow, deriveShow1)

-- | Convert a 'Const' value to a 'Builder' with the given show function and precedence.
-- 
-- /Since: 1/
showbConstPrecWith :: (Int -> a -> Builder) -> Int -> Const a b -> Builder
showbConstPrecWith sp = showbPrecWith2 sp undefined

-- | Convert a 'ZipList' to a 'Builder' with the given show function precedence.
-- 
-- /Since: 1/
showbZipListPrecWith :: (Int -> a -> Builder) -> Int -> ZipList a -> Builder
showbZipListPrecWith = showbPrecWith

instance Show a => Show (Const a b) where
    showbPrec = showbPrecWith undefined

instance Show a => Show1 (Const a) where
    showbPrecWith = showbPrecWith2 showbPrec

instance Show2 Const where
    showbPrecWith2 sp1 _ p (Const x) = showbUnaryWith sp1 "Const" p x

$(deriveShow  ''ZipList)
$(deriveShow1 ''ZipList)
