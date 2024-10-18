{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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

#if MIN_VERSION_ghc_prim(0,7,0)
import GHC.Tuple (Solo(..))
import TextShow.Classes (TextShow(..), TextShow1(..),
                         showbPrec1, showbUnaryWith)
#endif

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

#if MIN_VERSION_ghc_prim(0,7,0)
-- | /Since: 3.9.3/
instance TextShow a => TextShow (Solo a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 3.9.3/
instance TextShow1 Solo where
# if MIN_VERSION_ghc_prim(0,10,0)
    liftShowbPrec sp _ p (MkSolo x) = showbUnaryWith sp "MkSolo" p x
# else
    liftShowbPrec sp _ p (Solo   x) = showbUnaryWith sp "Solo"   p x
# endif
    {-# INLINE liftShowbPrec #-}
#endif
