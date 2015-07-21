{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE StandaloneDeriving         #-}
#endif
{-|
Module:      TextShow.Classes
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

The 'TextShow', 'TextShow1', and 'TextShow2' typeclasses.
-}
module TextShow.Classes where

import           Data.Data (Typeable)
import           Data.Monoid.Compat ((<>))
import           Data.Text         as TS (Text)
import qualified Data.Text.IO      as TS (putStrLn, hPutStrLn)
import qualified Data.Text.Lazy    as TL (Text)
import qualified Data.Text.Lazy.IO as TL (putStrLn, hPutStrLn)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (Builder, fromString, singleton, toLazyText)

import           GHC.Show (appPrec, appPrec1)

import           System.IO (Handle)

import           TextShow.Utils (toString)

-------------------------------------------------------------------------------

-- | Conversion of values to @Text@. Because there are both strict and lazy @Text@
-- variants, the 'TextShow' class deliberately avoids using @Text@ in its functions.
-- Instead, 'showbPrec', 'showb', and 'showbList' all return 'Builder', an
-- efficient intermediate form that can be converted to either kind of @Text@.
--
-- 'Builder' is a 'Monoid', so it is useful to use the 'mappend' (or '<>') function
-- to combine 'Builder's when creating 'TextShow' instances. As an example:
--
-- @
-- import Data.Monoid
-- import TextShow
--
-- data Example = Example Int Int
-- instance TextShow Example where
--     showb (Example i1 i2) = showb i1 <> showbSpace <> showb i2
-- @
--
-- If you do not want to create 'TextShow' instances manually, you can alternatively
-- use the "TextShow.TH" module to automatically generate default 'TextShow'
-- instances using Template Haskell, or the "TextShow.Generic" module to
-- quickly define 'TextShow' instances using 'genericShowbPrec'.
--
-- /Since: 2/
class TextShow a where
    -- | Convert a value to a 'Builder' with the given predence.
    --
    -- /Since: 2/
    showbPrec :: Int -- ^ The operator precedence of the enclosing context (a number
                     -- from @0@ to @11@). Function application has precedence @10@.
              -> a   -- ^ The value to be converted to a 'String'.
              -> Builder
    showbPrec _ = showb

    -- | A specialized variant of 'showbPrec' using precedence context zero.
    --
    -- /Since: 2/
    showb :: a -> Builder
    showb = showbPrec 0

    -- | Allows for specialized display of lists. This is used, for example, when
    -- showing lists of 'Char's.
    --
    -- /Since: 2/
    showbList :: [a] -> Builder
    showbList = showbListWith showb

#if __GLASGOW_HASKELL__ >= 708
    {-# MINIMAL showbPrec | showb #-}

deriving instance Typeable TextShow
#endif

-- | Constructs a strict 'TS.Text' from a single value.
--
-- /Since: 2/
showt :: TextShow a => a -> TS.Text
showt = toStrict . showtl
{-# INLINE showt #-}

-- | Constructs a lazy 'TL.Text' from a single value.
--
-- /Since: 2/
showtl :: TextShow a => a -> TL.Text
showtl = toLazyText . showb
{-# INLINE showtl #-}

-- | Constructs a strict 'TS.Text' from a single value with the given precedence.
--
-- /Since: 2/
showtPrec :: TextShow a => Int -> a -> TS.Text
showtPrec p = toStrict . showtlPrec p
{-# INLINE showtPrec #-}

-- | Constructs a lazy 'TL.Text' from a single value with the given precedence.
--
-- /Since: 2/
showtlPrec :: TextShow a => Int -> a -> TL.Text
showtlPrec p = toLazyText . showbPrec p
{-# INLINE showtlPrec #-}

-- | Construct a strict 'TS.Text' from a list of values.
--
-- /Since: 2/
showtList :: TextShow a => [a] -> TS.Text
showtList = toStrict . showtlList
{-# INLINE showtList #-}

-- | Construct a lazy 'TL.Text' from a list of values.
--
-- /Since: 2/
showtlList :: TextShow a => [a] -> TL.Text
showtlList = toLazyText . showbList
{-# INLINE showtlList #-}

-- | Surrounds 'Builder' output with parentheses if the 'Bool' parameter is 'True'.
--
-- /Since: 2/
showbParen :: Bool -> Builder -> Builder
showbParen p builder | p         = singleton '(' <> builder <> singleton ')'
                     | otherwise = builder
{-# INLINE showbParen #-}

-- | Construct a 'Builder' containing a single space character.
--
-- /Since: 2/
showbSpace :: Builder
showbSpace = singleton ' '

-- | Converts a list of values into a 'Builder' in which the values are surrounded
-- by square brackets and each value is separated by a comma. The function argument
-- controls how each element is shown.

-- @'showbListWith' 'showb'@ is the default implementation of 'showbList' save for
-- a few special cases (e.g., 'String').
--
-- /Since: 2/
showbListWith :: (a -> Builder) -> [a] -> Builder
showbListWith _      []     = "[]"
showbListWith showbx (x:xs) = singleton '[' <> showbx x <> go xs -- "[..
  where
    go (y:ys) = singleton ',' <> showbx y <> go ys               -- ..,..
    go []     = singleton ']'                                    -- ..]"
{-# INLINE showbListWith #-}

-- | Writes a value's strict 'TS.Text' representation to the standard output, followed
--   by a newline.
--
-- /Since: 2/
printT :: TextShow a => a -> IO ()
printT = TS.putStrLn . showt
{-# INLINE printT #-}

-- | Writes a value's lazy 'TL.Text' representation to the standard output, followed
--   by a newline.
--
-- /Since: 2/
printTL :: TextShow a => a -> IO ()
printTL = TL.putStrLn . showtl
{-# INLINE printTL #-}

-- | Writes a value's strict 'TS.Text' representation to a file handle, followed
--   by a newline.
--
-- /Since: 2/
hPrintT :: TextShow a => Handle -> a -> IO ()
hPrintT h = TS.hPutStrLn h . showt
{-# INLINE hPrintT #-}

-- | Writes a value's lazy 'TL.Text' representation to a file handle, followed
--   by a newline.
--
-- /Since: 2/
hPrintTL :: TextShow a => Handle -> a -> IO ()
hPrintTL h = TL.hPutStrLn h . showtl
{-# INLINE hPrintTL #-}

-- | Convert a @ShowS@-based show function to a @Builder@-based one.
--
-- /Since: 2.1/
showsToShowb :: (Int -> a -> ShowS) -> Int -> a -> Builder
showsToShowb sp p x = fromString $ sp p x ""
{-# INLINE showsToShowb #-}

-- | Convert a @Builder@-based show function to a @ShowS@-based one.
--
-- /Since: 2.1/
showbToShows :: (Int -> a -> Builder) -> Int -> a -> ShowS
showbToShows sp p = showString . toString . sp p
{-# INLINE showbToShows #-}

-------------------------------------------------------------------------------

-- | Lifting of the 'TextShow' class to unary type constructors.
--
-- /Since: 2/
class TextShow1 f where
    -- | Lifts a 'showbPrec' function through the type constructor.
    --
    -- /Since: 2/
    showbPrecWith :: (Int -> a -> Builder) -> Int -> f a -> Builder

#if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable TextShow1
#endif

-- | Lift the standard 'showbPrec' function through the type constructor.
--
-- /Since: 2/
showbPrec1 :: (TextShow1 f, TextShow a) => Int -> f a -> Builder
showbPrec1 = showbPrecWith showbPrec
{-# INLINE showbPrec1 #-}

-- | @'showbUnaryWith' sp n p x@ produces the 'Builder' representation of a unary data
-- constructor with name @n@ and argument @x@, in precedence context @p@, using the
-- function @sp@ to show occurrences of the type argument.
--
-- /Since: 2/
showbUnaryWith :: (Int -> a -> Builder) -> Builder -> Int -> a -> Builder
showbUnaryWith sp nameB p x = showbParen (p > appPrec) $
    nameB <> showbSpace <> sp appPrec1 x
{-# INLINE showbUnaryWith #-}

-------------------------------------------------------------------------------

-- | Lifting of the 'TextShow' class to binary type constructors.
--
-- /Since: 2/
class TextShow2 f where
    -- | Lifts 'showbPrec' functions through the type constructor.
    --
    -- /Since: 2/
    showbPrecWith2 :: (Int -> a -> Builder) -> (Int -> b -> Builder) ->
        Int -> f a b -> Builder

#if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable TextShow2
#endif

-- | Lift two 'showbPrec' functions through the type constructor.
--
-- /Since: 2/
showbPrec2 :: (TextShow2 f, TextShow a, TextShow b) => Int -> f a b -> Builder
showbPrec2 = showbPrecWith2 showbPrec showbPrec
{-# INLINE showbPrec2 #-}

-- | @'showbBinaryWith' sp n p x y@ produces the 'Builder' representation of a binary
-- data constructor with name @n@ and arguments @x@ and @y@, in precedence context
-- @p@, using the functions @sp1@ and @sp2@ to show occurrences of the type arguments.
--
-- /Since: 2/
showbBinaryWith :: (Int -> a -> Builder) -> (Int -> b -> Builder) ->
    Builder -> Int -> a -> b -> Builder
showbBinaryWith sp1 sp2 nameB p x y = showbParen (p > appPrec) $ nameB
    <> showbSpace <> sp1 appPrec1 x
    <> showbSpace <> sp2 appPrec1 y
{-# INLINE showbBinaryWith #-}
