{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds         #-}
#endif

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

import Data.Text.Lazy.Builder (Builder)

import TextShow.Classes (TextShow(..), TextShow1(..),
                         TextShow2(..), showbUnaryWith)
import TextShow.Data.List ()
import TextShow.TH.Internal (deriveTextShow, deriveTextShow1)

-- | /Since: 2/
instance TextShow a => TextShow (Const a b) where
    showbPrec = liftShowbConstPrec showbPrec

-- | /Since: 2/
instance TextShow a => TextShow1 (Const a) where
    liftShowbPrec _ _ = liftShowbConstPrec showbPrec

-- | /Since: 2/
instance TextShow2 Const where
    liftShowbPrec2 sp _ _ _  = liftShowbConstPrec sp

liftShowbConstPrec :: (Int -> a -> Builder) -> Int -> Const a b -> Builder
liftShowbConstPrec sp p (Const x) = showbUnaryWith sp "Const" p x
{-# INLINE liftShowbConstPrec #-}

-- | /Since: 2/
$(deriveTextShow  ''ZipList)
-- | /Since: 2/
$(deriveTextShow1 ''ZipList)
