{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Ord
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for 'Ordering' and 'Down'.

/Since: 2/
-}
module TextShow.Data.Ord () where

import GHC.Exts (Down)
import TextShow.TH.Internal (deriveTextShow, deriveTextShow1)

-- | /Since: 2/
$(deriveTextShow  ''Ordering)
-- | /Since: 2/
$(deriveTextShow  ''Down)
-- | /Since: 2/
$(deriveTextShow1 ''Down)
