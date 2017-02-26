{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Functor.Identity
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'Identity' values.

/Since: 2/
-}
module TextShow.Data.Functor.Identity () where

import Data.Functor.Identity (Identity(..))
import TextShow.Classes (TextShow(..), TextShow1(..),
                         showbPrec1, showbUnaryWith)

-- | /Since: 3/
instance TextShow a => TextShow (Identity a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 3/
instance TextShow1 Identity where
    -- This would be equivalent to the derived instance of 'Identity' if the
    -- 'runIdentity' field were removed.
    liftShowbPrec sp _ p (Identity x) = showbUnaryWith sp "Identity" p x
    {-# INLINE liftShowbPrec #-}
