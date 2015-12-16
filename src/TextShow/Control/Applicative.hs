{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Control.Applicative
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for 'Const' and 'ZipList'.

/Since: 2/
-}
module TextShow.Control.Applicative (showbConstPrecWith, showbZipListPrecWith) where

import Control.Applicative (Const(..), ZipList)

import Data.Text.Lazy.Builder (Builder)

import TextShow.Classes (TextShow(..), TextShow1(..),
                         TextShow2(..), showbUnaryWith)
import TextShow.Data.List ()
import TextShow.TH.Internal (deriveTextShow, deriveTextShow1)

-- | Convert a 'Const' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbConstPrecWith :: (Int -> a -> Builder) -> Int -> Const a b -> Builder
showbConstPrecWith sp = showbPrecWith2 sp undefined

-- | Convert a 'ZipList' to a 'Builder' with the given show function precedence.
--
-- /Since: 2/
showbZipListPrecWith :: (Int -> a -> Builder) -> Int -> ZipList a -> Builder
showbZipListPrecWith = showbPrecWith

instance TextShow a => TextShow (Const a b) where
    showbPrec = showbPrecWith undefined

instance TextShow a => TextShow1 (Const a) where
    showbPrecWith = showbPrecWith2 showbPrec

instance TextShow2 Const where
    showbPrecWith2 sp1 _ p (Const x) = showbUnaryWith sp1 "Const" p x

$(deriveTextShow  ''ZipList)
$(deriveTextShow1 ''ZipList)
