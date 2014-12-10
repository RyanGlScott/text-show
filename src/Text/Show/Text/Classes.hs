{-# LANGUAGE CPP, NoImplicitPrelude, OverloadedStrings #-}
{-|
Module:      Text.Show.Text.Classes
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

The 'Show' and 'Show1' typeclasses.
-}
module Text.Show.Text.Classes where

import           Data.Text         as TS (Text)
import qualified Data.Text.IO      as TS (putStrLn, hPutStrLn)
import qualified Data.Text.Lazy    as TL (Text)
import qualified Data.Text.Lazy.IO as TL (putStrLn, hPutStrLn)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (Builder, toLazyText)

import           Prelude hiding (Show(show, showList))

import           System.IO (Handle)

import           Text.Show.Text.Utils ((<>), s)

-- | Conversion of values to @Text@. Because there are both strict and lazy @Text@
-- variants, the 'Show' class deliberately avoids using @Text@ in its functions.
-- Instead, 'showbPrec', 'showb', and 'showbList' all return 'Builder', an
-- efficient intermediate form that can be converted to either kind of @Text@.
-- 
-- 'Builder' is a 'Monoid', so it is useful to use the 'mappend' (or '<>') function
-- to combine 'Builder's when creating 'Show' instances. As an example:
-- 
-- @
-- import Text.Show.Text
-- 
-- data Example = Example Int Int
-- instance Show Example where
--     showb (Example i1 i2) = showb i1 <> showbSpace <> showb i2
-- @
-- 
-- If you do not want to create 'Show' instances manually, you can alternatively
-- use the "Text.Show.Text.TH" module to automatically generate default 'Show'
-- instances using Template Haskell.
class Show a where
    -- | Convert a value to a 'Builder' with the given predence.
    showbPrec :: Int -- ^ The operator precedence of the enclosing context (a number
                     -- from @0@ to @11@). Function application has precedence @10@.
              -> a   -- ^ The value to be converted to a 'String'.
              -> Builder
    
    -- | A specialized variant of 'showbPrec' using precedence context zero.
    showb :: a -> Builder
    
    -- | Allows for specialized display of lists. This is used, for example, when
    -- showing lists of 'Char's.
    showbList :: [a] -> Builder
    
    showbPrec _ = showb
    
    showb = showbPrec 0
    
    showbList = showbListDefault showb
#if __GLASGOW_HASKELL__ >= 708
    {-# MINIMAL showbPrec | showb #-}
#endif

-- | Lifting of the 'Show' class to unary type constructors.
class Show1 f where
    -- | 'Builder' conversion for values of a type that has a unary type constructor.
    showbPrec1 :: Show a => Int -> f a -> Builder

-- | Constructs a strict 'TS.Text' from a single value.
show :: Show a => a -> TS.Text
show = toStrict . showLazy
{-# INLINE show #-}

-- | Constructs a lazy 'TL.Text' from a single value.
showLazy :: Show a => a -> TL.Text
showLazy = toLazyText . showb
{-# INLINE showLazy #-}

-- | Constructs a strict 'TS.Text' from a single value with the given precedence.
showPrec :: Show a => Int -> a -> TS.Text
showPrec p = toStrict . showPrecLazy p
{-# INLINE showPrec #-}

-- | Constructs a lazy 'TL.Text' from a single value with the given precedence.
showPrecLazy :: Show a => Int -> a -> TL.Text
showPrecLazy p = toLazyText . showbPrec p
{-# INLINE showPrecLazy #-}

-- | Construct a strict 'TS.Text' from a list of values.
showList :: Show a => [a] -> TS.Text
showList = toStrict . showListLazy
{-# INLINE showList #-}

-- | Construct a lazy 'TL.Text' from a list of values.
showListLazy :: Show a => [a] -> TL.Text
showListLazy = toLazyText . showbList
{-# INLINE showListLazy #-}

-- | Surrounds 'Builder' output with parentheses if the 'Bool' parameter is 'True'.
showbParen :: Bool -> Builder -> Builder
showbParen p builder | p         = s '(' <> builder <> s ')'
                     | otherwise = builder
{-# INLINE showbParen #-}

-- | Construct a 'Builder' containing a single space character.
showbSpace :: Builder
showbSpace = s ' '
{-# INLINE showbSpace #-}

-- |
-- Converts a list of values into a 'Builder' in which the values are surrounded
-- by square brackets and each value is separated by a comma. This is the default
-- implementation of 'showbList' save for a few special cases (e.g., 'String').
showbListDefault :: (a -> Builder) -> [a] -> Builder
showbListDefault _      []     = "[]"
showbListDefault showbx (x:xs) = s '[' <> showbx x <> go xs -- "[..
  where
    go (y:ys) = s ',' <> showbx y <> go ys                  -- ..,..
    go []     = s ']'                                       -- ..]"
{-# INLINE showbListDefault #-}

-- | Writes a value's strict 'TS.Text' representation to the standard output, followed
--   by a newline.
print :: Show a => a -> IO ()
print = TS.putStrLn . show
{-# INLINE print #-}

-- | Writes a value's lazy 'TL.Text' representation to the standard output, followed
--   by a newline.
printLazy :: Show a => a -> IO ()
printLazy = TL.putStrLn . showLazy
{-# INLINE printLazy #-}

-- | Writes a value's strict 'TS.Text' representation to a file handle, followed
--   by a newline.
hPrint :: Show a => Handle -> a -> IO ()
hPrint h = TS.hPutStrLn h . show
{-# INLINE hPrint #-}

-- | Writes a value's lazy 'TL.Text' representation to a file handle, followed
--   by a newline.
hPrintLazy :: Show a => Handle -> a -> IO ()
hPrintLazy h = TL.hPutStrLn h . showLazy
{-# INLINE hPrintLazy #-}