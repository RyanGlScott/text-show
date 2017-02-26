{-# LANGUAGE CPP             #-}

#if MIN_VERSION_base(4,7,0)
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      TextShow.Data.Type.Equality
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for propositional equality.
Only provided if using @base-4.7.0.0@ or later.

/Since: 2/
-}
module TextShow.Data.Type.Equality () where

#if MIN_VERSION_base(4,7,0)
import Data.Type.Equality ((:~:))

import TextShow.Classes (TextShow1(..))
import TextShow.TH.Internal (deriveTextShow, deriveTextShow2, makeLiftShowbPrec)

-- | /Since: 2/
$(deriveTextShow ''(:~:))

-- | /Since: 2/
instance TextShow1 ((:~:) a) where
    liftShowbPrec = $(makeLiftShowbPrec ''(:~:))

-- | /Since: 2/
$(deriveTextShow2 ''(:~:))
#endif
