{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Either
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'Either'.

/Since: 2/
-}
module TextShow.Data.Either () where

import TextShow.TH.Internal (deriveTextShow, deriveTextShow1, deriveTextShow2)

-- | /Since: 2/
$(deriveTextShow  ''Either)
-- | /Since: 2/
$(deriveTextShow1 ''Either)
-- | /Since: 2/
$(deriveTextShow2 ''Either)
