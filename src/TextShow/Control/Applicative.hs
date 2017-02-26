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

'TextShow' instances for 'Const' and 'ZipList'.

/Since: 2/
-}
module TextShow.Control.Applicative () where

import Control.Applicative (Const(..), ZipList)

import TextShow.Classes (TextShow(..), TextShow1(..),
                         TextShow2(..), showbUnaryWith)
import TextShow.Data.List ()
import TextShow.TH.Internal (deriveTextShow, deriveTextShow1)

-- | /Since: 2/
instance TextShow a => TextShow (Const a b) where
    showbPrec = liftShowbPrec undefined undefined

-- | /Since: 2/
instance TextShow a => TextShow1 (Const a) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList

-- | /Since: 2/
instance TextShow2 Const where
    liftShowbPrec2 sp1 _ _ _ p (Const x) = showbUnaryWith sp1 "Const" p x

-- | /Since: 2/
$(deriveTextShow  ''ZipList)
-- | /Since: 2/
$(deriveTextShow1 ''ZipList)
