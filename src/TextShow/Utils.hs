{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

{-|
Module:      TextShow.Utils
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Miscellaneous utility functions.
-}
module TextShow.Utils (
      coerce
    , i2d
    , isInfixDataCon
    , isSymVar
    , isTupleString
    , lengthB
    , toString
    , toText
    , unlinesB
    , unwordsB
    ) where

import           Data.Int (Int64)
import           Data.Text (Text)
import           Data.Text.Lazy (length, toStrict, unpack)
import           Data.Text.Lazy.Builder (Builder, singleton, toLazyText)

import           GHC.Exts (Char(C#), Int(I#), (+#), chr#, ord#)

import           Prelude ()
import           Prelude.Compat hiding (length)

#if __GLASGOW_HASKELL__ >= 708
import qualified Data.Coerce as C (Coercible, coerce)
#else
import           Unsafe.Coerce (unsafeCoerce)
#endif

#if defined(MIN_VERSION_ghc_boot_th)
import           GHC.Lexeme (startsVarSym)
#else
import           Data.Char (isSymbol, ord)
#endif

-- | On GHC 7.8 and later, this is 'C.coerce' from "Data.Coerce". Otherwise, it's
-- 'unsafeCoerce'.
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

-- | Checks if a 'String' names a valid Haskell infix data constructor (i.e., does
-- it begin with a colon?).
isInfixDataCon :: String -> Bool
isInfixDataCon (':':_) = True
isInfixDataCon _       = False
{-# INLINE isInfixDataCon #-}

-- | Checks if a 'String' names a valid Haskell infix, non-constructor function.
isSymVar :: String -> Bool
isSymVar ""      = False
isSymVar (c : _) = startsVarSym c

#if !defined(MIN_VERSION_ghc_boot_th)
startsVarSym :: Char -> Bool
startsVarSym c = startsVarSymASCII c || (ord c > 0x7f && isSymbol c) -- Infix Ids

startsVarSymASCII :: Char -> Bool
startsVarSymASCII c = c `elem` "!#$%&*+./<=>?@\\^|~-"
#endif

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
