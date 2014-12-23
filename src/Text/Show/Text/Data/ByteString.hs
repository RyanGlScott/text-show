{-# LANGUAGE CPP #-}
#if !(MIN_VERSION_bytestring(0,10,0))
{-# LANGUAGE TemplateHaskell #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.ByteString
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @bytestring@ library.
-}
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

import           Prelude hiding (Show(show))

import qualified Text.Show as S (show)
import           Text.Show.Text.Classes (Show(showb, showbPrec))

#if !(MIN_VERSION_bytestring(0,10,0))
import           Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowbPrec)
#endif

-- | Convert a strict 'BS.ByteString' to a 'Builder'.
showbByteStringStrict :: BS.ByteString -> Builder
showbByteStringStrict = fromString . S.show
{-# INLINE showbByteStringStrict #-}

-- | Convert a lazy 'BL.ByteString' to a 'Builder'.
showbByteStringLazy :: BL.ByteString -> Builder
showbByteStringLazy = showbByteStringLazyPrec 0
{-# INLINE showbByteStringLazy #-}

-- | Convert a lazy 'BL.ByteString' to a 'Builder' with the given precedence.
-- 
-- With @bytestring-0.10.0.0@ or later, this function ignores the precedence
-- argument, since lazy 'BL.ByteString's are printed out identically to 'String's.
-- On earlier versions of @bytestring@, however, lazy 'BL.ByteString's can be printed
-- with parentheses (e.g., @Chunk "example" Empty@ vs. @(Chunk "example" Empty)@)
-- depending on the precedence.
showbByteStringLazyPrec :: Int -> BL.ByteString -> Builder
#if MIN_VERSION_bytestring(0,10,0)
showbByteStringLazyPrec _ = fromString . S.show
#else
showbByteStringLazyPrec = showbPrec
#endif
{-# INLINE showbByteStringLazyPrec #-}

#if MIN_VERSION_bytestring(0,10,4)
-- | Convert a 'ShortByteString' to a 'Builder'.
showbShortByteString :: ShortByteString -> Builder
showbShortByteString = fromString . S.show
{-# INLINE showbShortByteString #-}
#endif

instance Show BS.ByteString where
    showb = showbByteStringStrict
    {-# INLINE showb #-}

#if MIN_VERSION_bytestring(0,10,0)
instance Show BL.ByteString where
    showbPrec = showbByteStringLazyPrec
    {-# INLINE showbPrec #-}
#else
$(deriveShowPragmas defaultInlineShowbPrec ''BL.ByteString)
#endif

#if MIN_VERSION_bytestring(0,10,4)
instance Show ShortByteString where
    showb = showbShortByteString
    {-# INLINE showb #-}
#endif
