{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE MagicHash       #-}
#if !(MIN_VERSION_bytestring(0,10,0))
{-# LANGUAGE TemplateHaskell #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.Data.ByteString
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for data types in the @bytestring@ library.

/Since: 2/
-}
module TextShow.Data.ByteString (
      showbByteStringStrict
    , showbByteStringLazy
    , showbByteStringLazyPrec
    , showbShortByteString
    ) where

import qualified Data.ByteString.Internal      as BS
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Data.ByteString.Short         as SBS
import           Data.ByteString.Short.Internal (ShortByteString(..))
import           Data.Text.Lazy.Builder (Builder)

import           GHC.Exts (ByteArray#, Char(C#), Int(I#), indexCharArray#)

import           TextShow.Classes (TextShow(..))
import           TextShow.Data.Char ()
import           TextShow.Data.List ()

#if !(MIN_VERSION_bytestring(0,10,0))
import           Data.Word (Word8)

import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Ptr (plusPtr)
import           Foreign.Storable (peek, peekByteOff)

import           TextShow.TH.Internal (deriveTextShow)
#endif

#include "inline.h"

-- | Convert a strict 'BS.ByteString' to a 'Builder'.
--
-- /Since: 2/
{-# INLINE showbByteStringStrict #-}
showbByteStringStrict :: BS.ByteString -> Builder
#if MIN_VERSION_bytestring(0,10,0)
showbByteStringStrict = showb . BS.unpackChars
#else
showbByteStringStrict = showb . unpackWith BS.w2c

-- | /O(n)/ Converts a 'ByteString' to a '[a]', using a conversion function.
unpackWith :: (Word8 -> a) -> BS.ByteString -> [a]
unpackWith _ (BS.PS _  _ 0) = []
unpackWith k (BS.PS ps s l) = BS.inlinePerformIO $ withForeignPtr ps $ \p ->
        go (p `plusPtr` s) (l - 1) []
    where
        go !p !0 !acc = peek p          >>= \e -> return (k e : acc)
        go !p !n !acc = peekByteOff p n >>= \e -> go p (n-1) (k e : acc)
{-# INLINE unpackWith #-}
#endif

-- | Convert a lazy 'BL.ByteString' to a 'Builder'.
--
-- /Since: 2/
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
--
-- /Since: 2/
showbByteStringLazyPrec :: Int -> BL.ByteString -> Builder
#if MIN_VERSION_bytestring(0,10,0)
showbByteStringLazyPrec _ = showb . BL.unpackChars
#else
showbByteStringLazyPrec = showbPrec
#endif
{-# INLINE showbByteStringLazyPrec #-}

-- | Convert a 'ShortByteString' to a 'Builder'.
--
-- /Since: 2/
showbShortByteString :: ShortByteString -> Builder
showbShortByteString = showb . unpackChars
{-# INLINE showbShortByteString #-}

-- Unpacking bytestrings into lists effeciently is a tradeoff: on the one hand
-- we would like to write a tight loop that just blats the list into memory, on
-- the other hand we want it to be unpacked lazily so we don't end up with a
-- massive list data structure in memory.
--
-- Our strategy is to combine both: we will unpack lazily in reasonable sized
-- chunks, where each chunk is unpacked strictly.
--
-- unpackChars does the lazy loop, while unpackAppendBytes and
-- unpackAppendChars do the chunks strictly.

unpackChars :: ShortByteString -> [Char]
unpackChars bs = unpackAppendCharsLazy bs []

-- Why 100 bytes you ask? Because on a 64bit machine the list we allocate
-- takes just shy of 4k which seems like a reasonable amount.
-- (5 words per list element, 8 bytes per word, 100 elements = 4000 bytes)

unpackAppendCharsLazy :: ShortByteString -> [Char] -> [Char]
unpackAppendCharsLazy sbs cs0 =
    go 0 (SBS.length sbs) cs0
  where
    sz = 100

    go off len cs
      | len <= sz = unpackAppendCharsStrict sbs off len cs
      | otherwise = unpackAppendCharsStrict sbs off sz  remainder
                      where remainder = go (off+sz) (len-sz) cs

-- For these unpack functions, since we're unpacking the whole list strictly we
-- build up the result list in an accumulator. This means we have to build up
-- the list starting at the end. So our traversal starts at the end of the
-- buffer and loops down until we hit the sentinal:

unpackAppendCharsStrict :: ShortByteString -> Int -> Int -> [Char] -> [Char]
unpackAppendCharsStrict !sbs off len cs =
    go (off-1) (off-1 + len) cs
  where
    go !sentinal !i !acc
      | i == sentinal = acc
      | otherwise     = let !c = indexCharArray (asBA sbs) i
                        in go sentinal (i-1) (c:acc)

------------------------------------------------------------------------
-- Primop wrappers

data BA = BA# ByteArray#

indexCharArray :: BA -> Int -> Char
indexCharArray (BA# ba#) (I# i#) = C# (indexCharArray# ba# i#)

------------------------------------------------------------------------
-- Internal utils

asBA :: ShortByteString -> BA
asBA (SBS ba#) = BA# ba#

instance TextShow BS.ByteString where
    showb = showbByteStringStrict
    INLINE_INST_FUN(showb)

#if MIN_VERSION_bytestring(0,10,0)
instance TextShow BL.ByteString where
    showbPrec = showbByteStringLazyPrec
    INLINE_INST_FUN(showbPrec)
#else
$(deriveTextShow ''BL.ByteString)
#endif

instance TextShow ShortByteString where
    showb = showbShortByteString
    INLINE_INST_FUN(showb)
