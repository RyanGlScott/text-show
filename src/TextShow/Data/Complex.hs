{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.Data.Ratio
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for 'Ratio'.

/Since: 2/
-}
module TextShow.Data.Complex () where

import Data.Complex (Complex)

import TextShow.Classes (TextShow(..))
import TextShow.Data.Floating ()
import TextShow.TH.Internal (deriveTextShow1, makeShowbPrec)

-- | /Since: 2/
instance TextShow a => TextShow (Complex a) where
    {-# SPECIALIZE instance TextShow (Complex Float)  #-}
    {-# SPECIALIZE instance TextShow (Complex Double) #-}
    showbPrec = $(makeShowbPrec ''Complex)
    {-# INLINE showbPrec #-}

-- | /Since: 2/
$(deriveTextShow1 ''Complex)
