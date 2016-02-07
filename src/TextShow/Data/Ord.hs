{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Ord
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for 'Ordering' and 'Down'.

/Since: 2/
-}
module TextShow.Data.Ord (
      showbOrdering
    , liftShowbDownPrec
    ) where

import Data.Text.Lazy.Builder (Builder)

import GHC.Exts (Down)

import TextShow.Classes (showb, liftShowbPrec)
import TextShow.TH.Internal (deriveTextShow, deriveTextShow1)

-- | Convert a 'Ordering' to a 'Builder'.
--
-- /Since: 2/
showbOrdering :: Ordering -> Builder
showbOrdering = showb
{-# INLINE showbOrdering #-}

-- | Convert a 'Down' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbDownPrec :: (Int -> a -> Builder) -> Int -> Down a -> Builder
liftShowbDownPrec sp = liftShowbPrec sp undefined
{-# INLINE liftShowbDownPrec #-}

$(deriveTextShow  ''Ordering)
$(deriveTextShow  ''Down)
$(deriveTextShow1 ''Down)
