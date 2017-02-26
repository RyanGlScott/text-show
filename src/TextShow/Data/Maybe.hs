{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Maybe
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'Maybe'.

/Since: 2/
-}
module TextShow.Data.Maybe () where

import TextShow.TH.Internal (deriveTextShow, deriveTextShow1)

-- | /Since: 2/
$(deriveTextShow  ''Maybe)
-- | /Since: 2/
$(deriveTextShow1 ''Maybe)
