{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Control.Monad.ST
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for strict 'ST'.

/Since: 2/
-}
module TextShow.Control.Monad.ST () where

import Control.Monad.ST (ST)
import TextShow.Classes (TextShow(..), TextShow1(..), TextShow2(..))

-- | /Since: 2/
instance TextShow (ST s a) where
    showb = liftShowbPrec undefined undefined 0
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow1 (ST s) where
    liftShowbPrec = liftShowbPrec2 undefined undefined
    {-# INLINE liftShowbPrec #-}

-- | /Since: 2/
instance TextShow2 ST where
    liftShowbPrec2 _ _ _ _ _ _ = "<<ST action>>"
    {-# INLINE liftShowbPrec2 #-}
