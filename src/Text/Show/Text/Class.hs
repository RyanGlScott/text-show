{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Text.Show.Text.Class where

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.IO as T (putStrLn)
import qualified Data.Text.Lazy as TL (Text)
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
    {-# MINIMAL showbPrec | showb #-}

-- | Constructs a strict 'Text' from a single value.
show :: Show a => a -> Text
show = toStrict . showLazy
{-# INLINE show #-}

-- | Constructs a lazy 'Text' from a single value.
showLazy :: Show a => a -> TL.Text
showLazy = toLazyText . showb
{-# INLINE showLazy #-}

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

-- | Prints a value's 'Text' representation to the standard output.
print :: Show a => a -> IO ()
print = T.putStrLn . show
{-# INLINE print #-}

printLazy :: Show a => a -> IO ()
printLazy = TL.putStrLn . showLazy
{-# INLINE printLazy #-}