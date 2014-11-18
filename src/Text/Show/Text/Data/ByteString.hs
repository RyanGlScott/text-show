{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Foreign.Data.ByteString
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' functions for data types in the @bytestring@ library.
-- These are included for convenience (and because @bytestring@ is a
-- dependency of this library).
----------------------------------------------------------------------------
module Text.Show.Text.Data.ByteString (
      showbByteStringStrict
    , showbByteStringLazy
#if MIN_VERSION_bytestring(0,10,4)
    , showbShortByteString
#endif
    ) where

import qualified Data.ByteString      as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
#if MIN_VERSION_bytestring(0,10,4)
import           Data.ByteString.Short (ShortByteString)
#endif
import           Data.Text.Lazy.Builder (Builder, fromString)

import qualified Prelude as P
import           Prelude hiding (Show(show))

import           Text.Show.Text.Class (Show(showb))

-- | Convert a strict 'ByteString' to a 'Builder'.
showbByteStringStrict :: BS.ByteString -> Builder
showbByteStringStrict = fromString . P.show
{-# INLINE showbByteStringStrict #-}

-- | Convert a lazy 'ByteString' to a 'Builder'.
showbByteStringLazy :: BL.ByteString -> Builder
showbByteStringLazy = fromString . P.show
{-# INLINE showbByteStringLazy #-}

#if MIN_VERSION_bytestring(0,10,4)
-- | Convert a 'ShortByteString' to a 'Builder'.
showbShortByteString :: ShortByteString -> Builder
showbShortByteString = fromString . P.show
{-# INLINE showbShortByteString #-}
#endif

instance Show BS.ByteString where
    showb = showbByteStringStrict
    {-# INLINE showb #-}

instance Show BL.ByteString where
    showb = showbByteStringLazy
    {-# INLINE showb #-}

#if MIN_VERSION_bytestring(0,10,4)
instance Show ShortByteString where
    showb = showbShortByteString
    {-# INLINE showb #-}
#endif