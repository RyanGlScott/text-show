{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.Data.Functor.Product
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'Product'.

/Since: 3/
-}
module TextShow.Data.Functor.Product () where

import Data.Functor.Product (Product(..))

import TextShow.Classes (TextShow(..), TextShow1(..), showbPrec1)
import TextShow.TH.Internal (deriveTextShow1)

-- | /Since: 3/
$(deriveTextShow1 ''Product)

-- | /Since: 3/
instance (TextShow1 f, TextShow1 g, TextShow a) => TextShow (Product f g a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}
