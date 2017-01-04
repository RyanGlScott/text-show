{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Control.Applicative
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for 'Const' and 'ZipList'.

/Since: 2/
-}
module TextShow.Control.Applicative (liftShowbConstPrec, liftShowbZipListPrec) where

import Control.Applicative (Const(..), ZipList)

import Data.Text.Lazy.Builder (Builder)

import TextShow.Classes (TextShow(..), TextShow1(..),
                         TextShow2(..), showbUnaryWith)
import TextShow.Data.List ()
import TextShow.TH.Internal (deriveTextShow, deriveTextShow1)

-- | Convert a 'Const' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbConstPrec :: (Int -> a -> Builder) -> Int -> Const a b -> Builder
liftShowbConstPrec sp = liftShowbPrec2 sp undefined undefined undefined

-- | Convert a 'ZipList' to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbZipListPrec :: ([a] -> Builder) -> Int -> ZipList a -> Builder
liftShowbZipListPrec sl = liftShowbPrec undefined sl

instance TextShow a => TextShow (Const a b) where
    showbPrec = liftShowbPrec undefined undefined

instance TextShow a => TextShow1 (Const a) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList

instance TextShow2 Const where
    liftShowbPrec2 sp1 _ _ _ p (Const x) = showbUnaryWith sp1 "Const" p x

$(deriveTextShow  ''ZipList)
$(deriveTextShow1 ''ZipList)
