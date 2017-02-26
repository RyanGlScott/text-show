{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.Data.Functor.Compose
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'Compose'.

/Since: 3/
-}
module TextShow.Data.Functor.Compose () where

import Data.Functor.Compose (Compose(..))
import TextShow.Classes (TextShow(..), TextShow1(..), showbPrec1, showbUnaryWith)

-- | /Since: 3/
instance (TextShow1 f, TextShow1 g, TextShow a) => TextShow (Compose f g a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 3/
instance (TextShow1 f, TextShow1 g) => TextShow1 (Compose f g) where
    liftShowbPrec sp sl p (Compose x) =
        showbUnaryWith (liftShowbPrec (liftShowbPrec sp sl)
                                      (liftShowbList sp sl))
                       "Compose" p x
