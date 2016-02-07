{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

{-|
Module:      TextShow.Utils
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Miscellaneous utility functions.
-}
module TextShow.Utils (
      coerce
    , i2d
    , isInfixTypeCon
    , isTupleString
    , lengthB
    , toString
    , toText
    , unlinesB
    , unwordsB
    ) where

import           Data.Int (Int64)
import           Data.Text (Text)
import           Data.Monoid.Compat ((<>))
import           Data.Text.Lazy (length, toStrict, unpack)
import           Data.Text.Lazy.Builder (Builder, singleton, toLazyText)

import           GHC.Exts (Char(C#), Int(I#))
import           GHC.Prim ((+#), chr#, ord#)

import           Prelude ()
import           Prelude.Compat hiding (length)

#if __GLASGOW_HASKELL__ >= 708
import qualified Data.Coerce as C (Coercible, coerce)
#else
import           Unsafe.Coerce (unsafeCoerce)
#endif

-- On GHC 7.8+, this is 'Data.Coerce.coerce'. Otherwise, it's 'unsafeCoerce'.
#if __GLASGOW_HASKELL__ >= 708
coerce :: C.Coercible a b => a -> b
coerce = C.coerce
#else
coerce :: a -> b
coerce = unsafeCoerce
#endif

-- | Unsafe conversion for decimal digits.
i2d :: Int -> Char
i2d (I# i#) = C# (chr# (ord# '0'# +# i#))
{-# INLINE i2d #-}

-- | Checks if a 'String' names a valid Haskell infix type constructor (i.e., does
-- it begin with a colon?).
isInfixTypeCon :: String -> Bool
isInfixTypeCon (':':_) = True
isInfixTypeCon _       = False
{-# INLINE isInfixTypeCon #-}

-- | Checks if a 'String' represents a tuple (other than '()')
isTupleString :: String -> Bool
isTupleString ('(':',':_) = True
isTupleString _           = False
{-# INLINE isTupleString #-}

-- | Computes the length of a 'Builder'.
--
-- /Since: 2/
lengthB :: Builder -> Int64
lengthB = length . toLazyText
{-# INLINE lengthB #-}

-- | Convert a 'Builder' to a 'String' (without surrounding it with double quotes,
-- as 'show' would).
--
-- /Since: 2/
toString :: Builder -> String
toString = unpack . toLazyText
{-# INLINE toString #-}

-- | Convert a 'Builder' to a strict 'Text'.
--
-- /Since: 2/
toText :: Builder -> Text
toText = toStrict . toLazyText
{-# INLINE toText #-}

-- | Merges several 'Builder's, separating them by newlines.
--
-- /Since: 2/
unlinesB :: [Builder] -> Builder
unlinesB (b:bs) = b <> singleton '\n' <> unlinesB bs
unlinesB []     = mempty

-- | Merges several 'Builder's, separating them by spaces.
--
-- /Since: 2/
unwordsB :: [Builder] -> Builder
unwordsB (b:bs@(_:_)) = b <> singleton ' ' <> unwordsB bs
unwordsB [b]          = b
unwordsB []           = mempty
