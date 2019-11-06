{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.Data.Functor.Sum
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'Sum'.

/Since: 3/
-}
module TextShow.Data.Functor.Sum () where

import Data.Functor.Sum (Sum)

import TextShow.Classes (TextShow(..), TextShow1(..), showbPrec1)
import TextShow.TH.Internal (deriveTextShow1)

-- | /Since: 3/
$(deriveTextShow1 ''Sum)

-- | /Since: 3/
instance (TextShow1 f, TextShow1 g, TextShow a) => TextShow (Sum f g a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}
