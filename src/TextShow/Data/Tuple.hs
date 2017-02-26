{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Tuple
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for tuple types.

/Since: 2/
-}
module TextShow.Data.Tuple () where

import TextShow.TH.Internal (deriveTextShow, deriveTextShow1, deriveTextShow2)

-- The Great Pyramids of Template Haskell
-- | /Since: 2/
$(deriveTextShow ''())
-- | /Since: 2/
$(deriveTextShow ''(,))
-- | /Since: 2/
$(deriveTextShow ''(,,))
-- | /Since: 2/
$(deriveTextShow ''(,,,))
-- | /Since: 2/
$(deriveTextShow ''(,,,,))
-- | /Since: 2/
$(deriveTextShow ''(,,,,,))
-- | /Since: 2/
$(deriveTextShow ''(,,,,,,))
-- | /Since: 2/
$(deriveTextShow ''(,,,,,,,))
-- | /Since: 2/
$(deriveTextShow ''(,,,,,,,,))
-- | /Since: 2/
$(deriveTextShow ''(,,,,,,,,,))
-- | /Since: 2/
$(deriveTextShow ''(,,,,,,,,,,))
-- | /Since: 2/
$(deriveTextShow ''(,,,,,,,,,,,))
-- | /Since: 2/
$(deriveTextShow ''(,,,,,,,,,,,,))
-- | /Since: 2/
$(deriveTextShow ''(,,,,,,,,,,,,,))
-- | /Since: 2/
$(deriveTextShow ''(,,,,,,,,,,,,,,))

-- | /Since: 2/
$(deriveTextShow1 ''(,))
-- | /Since: 2/
$(deriveTextShow1 ''(,,))
-- | /Since: 2/
$(deriveTextShow1 ''(,,,))
-- | /Since: 2/
$(deriveTextShow1 ''(,,,,))
-- | /Since: 2/
$(deriveTextShow1 ''(,,,,,))
-- | /Since: 2/
$(deriveTextShow1 ''(,,,,,,))
-- | /Since: 2/
$(deriveTextShow1 ''(,,,,,,,))
-- | /Since: 2/
$(deriveTextShow1 ''(,,,,,,,,))
-- | /Since: 2/
$(deriveTextShow1 ''(,,,,,,,,,))
-- | /Since: 2/
$(deriveTextShow1 ''(,,,,,,,,,,))
-- | /Since: 2/
$(deriveTextShow1 ''(,,,,,,,,,,,))
-- | /Since: 2/
$(deriveTextShow1 ''(,,,,,,,,,,,,))
-- | /Since: 2/
$(deriveTextShow1 ''(,,,,,,,,,,,,,))
-- | /Since: 2/
$(deriveTextShow1 ''(,,,,,,,,,,,,,,))

-- | /Since: 2/
$(deriveTextShow2 ''(,))
-- | /Since: 2/
$(deriveTextShow2 ''(,,))
-- | /Since: 2/
$(deriveTextShow2 ''(,,,))
-- | /Since: 2/
$(deriveTextShow2 ''(,,,,))
-- | /Since: 2/
$(deriveTextShow2 ''(,,,,,))
-- | /Since: 2/
$(deriveTextShow2 ''(,,,,,,))
-- | /Since: 2/
$(deriveTextShow2 ''(,,,,,,,))
-- | /Since: 2/
$(deriveTextShow2 ''(,,,,,,,,))
-- | /Since: 2/
$(deriveTextShow2 ''(,,,,,,,,,))
-- | /Since: 2/
$(deriveTextShow2 ''(,,,,,,,,,,))
-- | /Since: 2/
$(deriveTextShow2 ''(,,,,,,,,,,,))
-- | /Since: 2/
$(deriveTextShow2 ''(,,,,,,,,,,,,))
-- | /Since: 2/
$(deriveTextShow2 ''(,,,,,,,,,,,,,))
-- | /Since: 2/
$(deriveTextShow2 ''(,,,,,,,,,,,,,,))
