{-# LANGUAGE CPP             #-}

#if MIN_VERSION_base(4,8,1)
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      TextShow.GHC.Stack
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for 'CallStack' and 'SrcLoc' values.
Only provided if using @base-4.8.1.0@ or later.

/Since: 3.0.1/
-}
module TextShow.GHC.Stack () where

#if MIN_VERSION_base(4,8,1)
import GHC.Stack (CallStack)
# if MIN_VERSION_base(4,9,0)
import GHC.Stack (SrcLoc, getCallStack)
import TextShow.Classes (TextShow(..))
# else
import GHC.SrcLoc (SrcLoc)
# endif

import TextShow.Data.Char     ()
import TextShow.Data.Integral ()
import TextShow.Data.List     ()
import TextShow.Data.Tuple    ()
import TextShow.TH.Internal (deriveTextShow)

-- | /Since: 3.0.1/
$(deriveTextShow ''SrcLoc)

# if MIN_VERSION_base(4,9,0)
-- | /Since: 3.0.1/
instance TextShow CallStack where
    showb = showb . getCallStack
    {-# INLINE showb #-}
# else
-- | /Since: 3.0.1/
$(deriveTextShow ''CallStack)
# endif
#endif
