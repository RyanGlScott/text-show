{-# LANGUAGE CPP           #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

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
      fromLazyText
    , fromString
    , i2d
    , isInfixDataCon
    , isSymVar
    , isTupleString
    , lengthB
    , runBuilderLazy
    , runBuilderString
    , unlinesB
    , unwordsB
    ) where

import           Data.Text.Builder.Linear (Builder)
import qualified Data.Text.Builder.Linear as TB
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

import           GHC.Exts (Char(C#), Int(I#), (+#), chr#, ord#)

import           Prelude ()
import           Prelude.Compat hiding (length)

#if defined(MIN_VERSION_ghc_boot_th)
import           GHC.Lexeme (startsVarSym)
#else
import           Data.Char (isSymbol, ord)
#endif

-- | Create a 'Builder' containing a given lazy 'TL.Text' value.
fromLazyText :: TL.Text -> Builder
fromLazyText = foldMap TB.fromText . TL.toChunks
{-# INLINE fromLazyText #-}

-- | Create a 'Builder' containing a given 'String' value.
fromString :: String -> Builder
fromString = TB.fromText . TS.pack
{-# INLINE fromString #-}

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
lengthB :: Builder -> Int
lengthB = TS.length . TB.runBuilder
{-# INLINE lengthB #-}

-- | Convert a 'Builder' to a lazy 'Text'.
--
-- /Since: 2/
runBuilderLazy :: Builder -> TL.Text
runBuilderLazy = TL.fromStrict . TB.runBuilder
{-# INLINE runBuilderLazy #-}

-- | Convert a 'Builder' to a 'String' (without surrounding it with double
-- quotes, as 'show' would).
--
-- /Since: 2/
runBuilderString :: Builder -> String
runBuilderString = TS.unpack . TB.runBuilder
{-# INLINE runBuilderString #-}

-- | Merges several 'Builder's, separating them by newlines.
--
-- /Since: 2/
unlinesB :: [Builder] -> Builder
unlinesB (b:bs) = b <> TB.fromChar '\n' <> unlinesB bs
unlinesB []     = mempty

-- | Merges several 'Builder's, separating them by spaces.
--
-- /Since: 2/
unwordsB :: [Builder] -> Builder
unwordsB (b:bs@(_:_)) = b <> TB.fromChar ' ' <> unwordsB bs
unwordsB [b]          = b
unwordsB []           = mempty
