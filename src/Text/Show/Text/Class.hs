{-# LANGUAGE CPP, NoImplicitPrelude, OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Class
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- The 'Show' type class.
----------------------------------------------------------------------------
module Text.Show.Text.Class where

import           Data.Monoid ((<>))
import           Data.Text         as TS (Text)
import qualified Data.Text.IO      as TS (putStrLn)
import qualified Data.Text.Lazy    as TL (Text)
import qualified Data.Text.Lazy.IO as TL (putStrLn)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (Builder, toLazyText)

import           Prelude hiding (Show(show))

import           Text.Show.Text.Functions (s)

-- | Conversion of values to 'Text'.
class Show a where
    -- |
    -- Constructs a 'Text' via an efficient 'Builder'. The precedence is used to 
    -- determine where to put parentheses in a shown expression involving operators.
    -- 
    -- 'Builder's can be efficiently combined, so the @showb@ functions are available
    -- for showing multiple values before producing an output 'Text'.
    showbPrec :: Int -> a -> Builder
    
    -- |
    -- Constructs a 'Text' via an efficient 'Builder'. 'Builder's can be efficiently
    -- combined, so this is available building a 'Text' from multiple values.
    showb :: a -> Builder
    
    -- |
    -- Allows for specialized display of lists. This is used, for example, when
    -- showing lists of 'Char's.
    showbList :: [a] -> Builder
    
    showbPrec _ = showb
    
    showb = showbPrec 0
    
    showbList = showbListDefault
#if __GLASGOW_HASKELL__ >= 708
    {-# MINIMAL showbPrec | showb #-}
#endif

-- | Constructs a strict 'Text' from a single value.
show :: Show a => a -> TS.Text
show = toStrict . showLazy
{-# INLINE show #-}

-- | Constructs a lazy 'Text' from a single value.
showLazy :: Show a => a -> TL.Text
showLazy = toLazyText . showb
{-# INLINE showLazy #-}

-- |
-- Converts a list of 'Show' values into a 'Builder' in which the values are surrounded
-- by square brackets and each value is separated by a comma. This is the default
-- implementation of 'showbList' save for a few special cases (e.g., 'String').
showbListDefault :: Show a => [a] -> Builder
showbListDefault []     = "[]"
showbListDefault (x:xs) = s '[' <> showb x <> go xs -- "[..
  where
    go (y:ys) = s ',' <> showb y <> go ys           -- ..,..
    go []     = s ']'                               -- ..]"
{-# INLINE showbListDefault #-}

-- | Surrounds 'Builder' output with parentheses if the 'Bool' parameter is 'True'.
showbParen :: Bool -> Builder -> Builder
showbParen p builder | p         = s '(' <> builder <> s ')'
                     | otherwise = builder
{-# INLINE showbParen #-}

-- | Prints a value's strict 'Text' representation to the standard output.
print :: Show a => a -> IO ()
print = TS.putStrLn . show
{-# INLINE print #-}

-- | Prints a value's lazy 'Text' representation to the standard output.
printLazy :: Show a => a -> IO ()
printLazy = TL.putStrLn . showLazy
{-# INLINE printLazy #-}