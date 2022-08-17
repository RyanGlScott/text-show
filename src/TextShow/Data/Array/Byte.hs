{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Array.Byte
Copyright:   (C) 2022 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides a 'TextShow' instance for 'ByteArray' from the "Data.Array.Byte"
module. Only provided if using @base-4.17.0.0@ or later.

/Since: 3.10/
-}
module TextShow.Data.Array.Byte () where

#if MIN_VERSION_base(4,17,0)
import           Data.Array.Byte (ByteArray(..))
import           Data.Bits (Bits(..))
import           Data.Char (intToDigit)
import           Data.Text.Lazy.Builder (Builder, fromString, singleton)

import           GHC.Exts (Int(..), indexWord8Array#, sizeofByteArray#)
import           GHC.Word (Word8(..))

import           Prelude ()
import           Prelude.Compat

import           TextShow.Classes (TextShow(..))

-- | /Since: 3.10/
instance TextShow ByteArray where
  showbPrec _ ba =
      fromString "[" <> go 0
    where
      showW8 :: Word8 -> Builder
      showW8 !w =
           singleton '0'
        <> singleton 'x'
        <> singleton (intToDigit (fromIntegral (unsafeShiftR w 4)))
        <> singleton (intToDigit (fromIntegral (w .&. 0x0F)))
      go i
        | i < sizeofByteArray ba = comma <> showW8 (indexByteArray ba i :: Word8) <> go (i+1)
        | otherwise              = singleton ']'
        where
          comma | i == 0    = mempty
                | otherwise = fromString ", "

-- | Read byte at specific index.
indexByteArray :: ByteArray -> Int -> Word8
{-# INLINE indexByteArray #-}
indexByteArray (ByteArray arr#) (I# i#) = W8# (indexWord8Array# arr# i#)

-- | Size of the byte array in bytes.
sizeofByteArray :: ByteArray -> Int
{-# INLINE sizeofByteArray #-}
sizeofByteArray (ByteArray arr#) = I# (sizeofByteArray# arr#)
#endif
