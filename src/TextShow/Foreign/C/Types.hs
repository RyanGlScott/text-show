{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.Foreign.C.Types
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for Haskell newtypes corresponding to C
types in the Foreign Function Interface (FFI).

/Since: 2/
-}
module TextShow.Foreign.C.Types () where

import Foreign.C.Types

import TextShow.Classes (TextShow(..))
import TextShow.Data.Floating ()
import TextShow.Data.Integral ()

-- | /Since: 2/
deriving instance TextShow CChar
-- | /Since: 2/
deriving instance TextShow CSChar
-- | /Since: 2/
deriving instance TextShow CUChar
-- | /Since: 2/
deriving instance TextShow CShort
-- | /Since: 2/
deriving instance TextShow CUShort
-- | /Since: 2/
deriving instance TextShow CInt
-- | /Since: 2/
deriving instance TextShow CUInt
-- | /Since: 2/
deriving instance TextShow CLong
-- | /Since: 2/
deriving instance TextShow CULong
-- | /Since: 2/
deriving instance TextShow CPtrdiff
-- | /Since: 2/
deriving instance TextShow CSize
-- | /Since: 2/
deriving instance TextShow CWchar
-- | /Since: 2/
deriving instance TextShow CSigAtomic
-- | /Since: 2/
deriving instance TextShow CLLong
-- | /Since: 2/
deriving instance TextShow CULLong
-- | /Since: 2/
deriving instance TextShow CIntPtr
-- | /Since: 2/
deriving instance TextShow CUIntPtr
-- | /Since: 2/
deriving instance TextShow CIntMax
-- | /Since: 2/
deriving instance TextShow CUIntMax
-- | /Since: 2/
deriving instance TextShow CClock
-- | /Since: 2/
deriving instance TextShow CTime
-- | /Since: 2/
deriving instance TextShow CUSeconds
-- | /Since: 2/
deriving instance TextShow CSUSeconds
-- | /Since: 2/
deriving instance TextShow CFloat
-- | /Since: 2/
deriving instance TextShow CDouble

#if MIN_VERSION_base(4,10,0)
-- | Only available with @base-4.10.0.0@ or later.
--
-- /Since: 3.6/
deriving instance TextShow CBool
#endif
