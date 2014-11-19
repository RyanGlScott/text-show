{-# LANGUAGE CPP, NoImplicitPrelude #-}
#if !MIN_VERSION_bytestring(0,10,0)
{-# LANGUAGE OverloadedStrings #-}
#endif
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
    , showbByteStringLazyPrec
#if MIN_VERSION_bytestring(0,10,4)
    , showbShortByteString
#endif
    ) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
#if MIN_VERSION_bytestring(0,10,4)
import           Data.ByteString.Short (ShortByteString)
#endif
import           Data.Text.Lazy.Builder (Builder, fromString)

import qualified Prelude as P
import           Prelude hiding (Show(show))

import           Text.Show.Text.Class (Show(showb, showbPrec))

-- Imports needed for older versions of bytestring
#if !MIN_VERSION_bytestring(0,10,0)
import qualified Data.ByteString.Lazy.Internal as BL
import           Data.Monoid ((<>))

import           GHC.Show (appPrec, appPrec1)

import           Text.Show.Text.Class (showbParen)
import           Text.Show.Text.Utils (s)
#endif

-- | Convert a strict 'ByteString' to a 'Builder'.
showbByteStringStrict :: BS.ByteString -> Builder
showbByteStringStrict = fromString . P.show
{-# INLINE showbByteStringStrict #-}

-- | Convert a lazy 'ByteString' to a 'Builder'.
showbByteStringLazy :: BL.ByteString -> Builder
showbByteStringLazy = showbByteStringLazyPrec 0
{-# INLINE showbByteStringLazy #-}

-- | Convert a lazy 'ByteString' to a 'Builder' with the given precedence.
-- 
-- With @bytestring-0.10.0.0@ or later, this function ignores the precedence
-- argument, since lazy 'ByteString's are printed out identically to 'String's.
-- On earlier versions of @bytestring@, however, lazy 'ByteString's can be printed
-- with parentheses (e.g., @Chunk "example" Empty@ vs. @(Chunk "example" Empty)@)
-- depending on the precedence.
showbByteStringLazyPrec :: Int -> BL.ByteString -> Builder
#if MIN_VERSION_bytestring(0,10,0)
showbByteStringLazyPrec _ = fromString . P.show
#else
showbByteStringLazyPrec _ BL.Empty         = "Empty"
showbByteStringLazyPrec p (BL.Chunk bs bl) = showbParen (p > appPrec) $
        "Chunk "
     <> showbPrec appPrec1 bs
     <> s ' '
     <> showbPrec appPrec1 bl
#endif
{-# INLINE showbByteStringLazyPrec #-}

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
    showbPrec = showbByteStringLazyPrec
    {-# INLINE showbPrec #-}

#if MIN_VERSION_bytestring(0,10,4)
instance Show ShortByteString where
    showb = showbShortByteString
    {-# INLINE showb #-}
#endif