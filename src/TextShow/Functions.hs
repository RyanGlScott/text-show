{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Functions
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Optional orphan 'TextShow', 'TextShow1', and 'TextShow2' instances for functions.

/Since: 2/
-}
module TextShow.Functions () where

import TextShow.Classes (TextShow(..), TextShow1(..), TextShow2(..))

-- | /Since: 2/
instance TextShow (a -> b) where
    showbPrec = liftShowbPrec undefined undefined
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow1 ((->) a) where
    liftShowbPrec = liftShowbPrec2 undefined undefined
    {-# INLINE liftShowbPrec #-}

-- | /Since: 2/
instance TextShow2 (->) where
    liftShowbPrec2 _ _ _ _ _ _ = "<function>"
    {-# INLINE liftShowbPrec2 #-}
