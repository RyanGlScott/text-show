{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Bool
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'Bool'.

/Since: 2/
-}
module TextShow.Data.Bool () where

import TextShow.TH.Internal (deriveTextShow)

-- | /Since: 2/
$(deriveTextShow ''Bool)
