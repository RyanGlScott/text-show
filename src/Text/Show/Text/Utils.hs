{-# LANGUAGE MagicHash #-}
{-|
Module:      Text.Show.Text.Utils
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Miscellaneous utility functions.
-}
module Text.Show.Text.Utils where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Monoid.Compat ((<>))
import Data.Text.Lazy (length, toStrict, unpack)
import Data.Text.Lazy.Builder (Builder, singleton, toLazyText)

import GHC.Exts (Char(C#), Int(I#))
import GHC.Prim ((+#), chr#, ord#)

import Prelude ()
import Prelude.Compat hiding (length)

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

-- | A shorter name for 'singleton' for convenience's sake (since it tends to be used
-- pretty often in @text-show@).
s :: Char -> Builder
s = singleton
{-# INLINE s #-}

-- | Computes the length of a 'Builder'.
-- 
-- /Since: 0.3/
lengthB :: Builder -> Int64
lengthB = length . toLazyText
{-# INLINE lengthB #-}

-- | Convert a 'Builder' to a 'String' (without surrounding it with double quotes,
-- as 'show' would).
-- 
-- /Since: 0.4.1/
toString :: Builder -> String
toString = unpack . toLazyText
{-# INLINE toString #-}

-- | Convert a 'Builder' to a strict 'Text'.
-- 
-- /Since: 0.4.1/
toText :: Builder -> Text
toText = toStrict . toLazyText
{-# INLINE toText #-}

-- | Merges several 'Builder's, separating them by newlines.
-- 
-- /Since: 0.1/
unlinesB :: [Builder] -> Builder
unlinesB (b:bs) = b <> s '\n' <> unlinesB bs
unlinesB []     = mempty

-- | Merges several 'Builder's, separating them by spaces.
-- 
-- /Since: 0.1/
unwordsB :: [Builder] -> Builder
unwordsB (b:bs@(_:_)) = b <> s ' ' <> unwordsB bs
unwordsB [b]          = b
unwordsB []           = mempty
