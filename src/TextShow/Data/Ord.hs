{-# LANGUAGE OverloadedStrings #-}
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

import GHC.Exts (Down(..))

import TextShow.Classes ( TextShow(..), TextShow1(..)
                        , showbPrec1, showbUnaryWith )
import TextShow.TH.Internal (deriveTextShow)

-- | This instance would be equivalent to a derived 'TextShow' instance
-- if the 'getDown' field were removed.
--
-- /Since: 2/
instance TextShow a => TextShow (Down a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | This instance would be equivalent to a derived 'TextShow1' instance
-- if the 'getDown' field were removed.
--
-- /Since: 2/
instance TextShow1 Down where
    liftShowbPrec sp _ p (Down x) = showbUnaryWith sp "Down" p x
    {-# INLINE liftShowbPrec #-}

-- | /Since: 2/
$(deriveTextShow ''Ordering)
