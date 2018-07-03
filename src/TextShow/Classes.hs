{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE StandaloneDeriving         #-}
#endif
{-|
Module:      TextShow.Classes
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

The 'TextShow', 'TextShow1', and 'TextShow2' typeclasses.
-}
module TextShow.Classes where

#if __GLASGOW_HASKELL__ >= 708
import           Data.Data (Typeable)
#endif
import qualified Data.Text         as TS (Text, singleton)
import qualified Data.Text.IO      as TS (putStrLn, hPutStrLn)
import qualified Data.Text.Lazy    as TL (Text, singleton)
import qualified Data.Text.Lazy.IO as TL (putStrLn, hPutStrLn)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (Builder, fromLazyText, fromString,
                                         fromText, singleton, toLazyText)

import           GHC.Show (appPrec, appPrec1)

import           Prelude ()
import           Prelude.Compat

import           System.IO (Handle)

import           TextShow.Utils (toString, toText)

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
-- import Data.Semigroup
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
-- quickly define 'TextShow' instances using "GHC.Generics".
--
-- /Since: 2/
class TextShow a where
    -- | Convert a value to a 'Builder' with the given predence.
    --
    -- /Since: 2/
    showbPrec :: Int -- ^ The operator precedence of the enclosing context (a number
                     -- from @0@ to @11@). Function application has precedence @10@.
              -> a   -- ^ The value to be converted to a 'Builder'.
              -> Builder
    showbPrec _ = showb

    -- | Converts a value to a strict 'TS.Text'. If you hand-define this, it should
    -- satisfy:
    --
    -- @
    -- 'showb' = 'showbPrec' 0
    -- @
    --
    -- /Since: 2/
    showb :: a -- ^ The value to be converted to a 'Builder'.
          -> Builder
    showb = showbPrec 0

    -- | Converts a list of values to a 'Builder'. By default, this is defined as
    -- @'showbList = 'showbListWith' 'showb'@, but it can be overridden to allow
    -- for specialized displaying of lists (e.g., lists of 'Char's).
    --
    -- /Since: 2/
    showbList :: [a] -- ^ The list of values to be converted to a 'Builder'.
              -> Builder
    showbList = showbListWith showb

    -- | Converts a value to a strict 'TS.Text' with the given precedence. This
    -- can be overridden for efficiency, but it should satisfy:
    --
    -- @
    -- 'showtPrec' p = 'toStrict' . 'showtlPrec' p
    -- @
    --
    -- /Since: 3/
    showtPrec :: Int -- ^ The operator precedence of the enclosing context (a number
                     -- from @0@ to @11@). Function application has precedence @10@.
              -> a   -- ^ The value to be converted to a strict 'TS.Text'.
              -> TS.Text
    showtPrec p = toStrict . showtlPrec p

    -- | Converts a value to a strict 'TS.Text'. This can be overridden for
    -- efficiency, but it should satisfy:
    --
    -- @
    -- 'showt' = 'showtPrec' 0
    -- 'showt' = 'toStrict' . 'showtl'
    -- @
    --
    -- The first equation is the default definition of 'showt'.
    --
    -- /Since: 3/
    showt :: a -- ^ The value to be converted to a strict 'TS.Text'.
          -> TS.Text
    showt = showtPrec 0

    -- | Converts a list of values to a strict 'TS.Text'. This can be overridden for
    -- efficiency, but it should satisfy:
    --
    -- @
    -- 'showtList' = 'toStrict' . 'showtlList'
    -- @
    --
    -- /Since: 3/
    showtList :: [a] -- ^ The list of values to be converted to a strict 'TS.Text'.
              -> TS.Text
    showtList = toStrict . showtlList

    -- | Converts a value to a lazy 'TL.Text' with the given precedence. This
    -- can be overridden for efficiency, but it should satisfy:
    --
    -- @
    -- 'showtlPrec' p = 'toLazyText' . 'showbPrec' p
    -- @
    --
    -- /Since: 3/
    showtlPrec :: Int -- ^ The operator precedence of the enclosing context (a number
                      -- from @0@ to @11@). Function application has precedence @10@.
               -> a   -- ^ The value to be converted to a lazy 'TL.Text'.
               -> TL.Text
    showtlPrec p = toLazyText . showbPrec p

    -- | Converts a value to a lazy 'TL.Text'. This can be overridden for
    -- efficiency, but it should satisfy:
    --
    -- @
    -- 'showtl' = 'showtlPrec' 0
    -- 'showtl' = 'toLazyText' . 'showb'
    -- @
    --
    -- The first equation is the default definition of 'showtl'.
    --
    -- /Since: 3/
    showtl :: a -- ^ The value to be converted to a lazy 'TL.Text'.
           -> TL.Text
    showtl = showtlPrec 0

    -- | Converts a list of values to a lazy 'TL.Text'. This can be overridden for
    -- efficiency, but it should satisfy:
    --
    -- @
    -- 'showtlList' = 'toLazyText' . 'showbList'
    -- @
    --
    -- /Since: 3/
    showtlList :: [a] -- ^ The list of values to be converted to a lazy 'TL.Text'.
               -> TL.Text
    showtlList = toLazyText . showbList

#if __GLASGOW_HASKELL__ >= 708
    {-# MINIMAL showbPrec | showb #-}

deriving instance Typeable TextShow
#endif

-- | Surrounds 'Builder' output with parentheses if the 'Bool' parameter is 'True'.
--
-- /Since: 2/
showbParen :: Bool -> Builder -> Builder
showbParen p builder | p         = singleton '(' <> builder <> singleton ')'
                     | otherwise = builder

-- | Construct a 'Builder' containing a comma followed by a space.
--
-- /Since: 3.6/
showbCommaSpace :: Builder
showbCommaSpace = ", "

-- | Construct a 'Builder' containing a single space character.
--
-- /Since: 2/
showbSpace :: Builder
showbSpace = singleton ' '

-- | Converts a list of values into a 'Builder' in which the values are surrounded
-- by square brackets and each value is separated by a comma. The function argument
-- controls how each element is shown.
--
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

-- | Surrounds strict 'TS.Text' output with parentheses if the 'Bool' parameter is 'True'.
--
-- /Since: 3.4/
showtParen :: Bool -> TS.Text -> TS.Text
showtParen p t | p         = TS.singleton '(' <> t <> TS.singleton ')'
               | otherwise = t

-- | Construct a strict 'TS.Text' containing a comma followed by a space.
--
-- /Since: 3.6/
showtCommaSpace :: TS.Text
showtCommaSpace = ", "

-- | Construct a strict 'TS.Text' containing a single space character.
--
-- /Since: 3.4/
showtSpace :: TS.Text
showtSpace = TS.singleton ' '

-- | Converts a list of values into a strict 'TS.Text' in which the values are surrounded
-- by square brackets and each value is separated by a comma. The function argument
-- controls how each element is shown.
--
-- /Since: 3.4/
showtListWith :: (a -> TS.Text) -> [a] -> TS.Text
showtListWith _      []     = "[]"
showtListWith showtx (x:xs) = TS.singleton '[' <> showtx x <> go xs -- "[..
  where
    go (y:ys) = TS.singleton ',' <> showtx y <> go ys               -- ..,..
    go []     = TS.singleton ']'                                    -- ..]"

-- | Surrounds lazy 'TL.Text' output with parentheses if the 'Bool' parameter is 'True'.
--
-- /Since: 3.4/
showtlParen :: Bool -> TL.Text -> TL.Text
showtlParen p t | p         = TL.singleton '(' <> t <> TL.singleton ')'
                | otherwise = t
{-# INLINE showtlParen #-}

-- | Construct a lazy 'TL.Text' containing a comma followed by a space.
--
-- /Since: 3.6/
showtlCommaSpace :: TL.Text
showtlCommaSpace = ", "

-- | Construct a lazy 'TL.Text' containing a single space character.
--
-- /Since: 3.4/
showtlSpace :: TL.Text
showtlSpace = TL.singleton ' '

-- | Converts a list of values into a lazy 'TL.Text' in which the values are surrounded
-- by square brackets and each value is separated by a comma. The function argument
-- controls how each element is shown.
--
-- /Since: 3.4/
showtlListWith :: (a -> TL.Text) -> [a] -> TL.Text
showtlListWith _       []     = "[]"
showtlListWith showtlx (x:xs) = TL.singleton '[' <> showtlx x <> go xs -- "[..
  where
    go (y:ys) = TL.singleton ',' <> showtlx y <> go ys                 -- ..,..
    go []     = TL.singleton ']'                                       -- ..]"

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

-- | Convert a precedence-aware 'ShowS'-based show function to a 'Builder'-based one.
--
-- /Since: 3/
showsPrecToShowbPrec :: (Int -> a -> ShowS) -> Int -> a -> Builder
showsPrecToShowbPrec sp p x = fromString $ sp p x ""
{-# INLINE showsPrecToShowbPrec #-}

-- | Convert a precedence-aware, strict 'TS.Text'-based show function to a 'Builder'-based one.
--
-- /Since: 3.4/
showtPrecToShowbPrec :: (Int -> a -> TS.Text) -> Int -> a -> Builder
showtPrecToShowbPrec sp p = fromText . sp p
{-# INLINE showtPrecToShowbPrec #-}

-- | Convert a precedence-aware, lazy 'TL.Text'-based show function to a 'Builder'-based one.
--
-- /Since: 3.4/
showtlPrecToShowbPrec :: (Int -> a -> TL.Text) -> Int -> a -> Builder
showtlPrecToShowbPrec sp p = fromLazyText . sp p
{-# INLINE showtlPrecToShowbPrec #-}

-- | Convert a 'ShowS'-based show function to a 'Builder'-based one.
--
-- /Since: 3/
showsToShowb :: (a -> ShowS) -> a -> Builder
showsToShowb sf x = fromString $ sf x ""
{-# INLINE showsToShowb #-}

-- | Convert a strict 'TS.Text'-based show function to a 'Builder'-based one.
--
-- /Since: 3.4/
showtToShowb :: (a -> TS.Text) -> a -> Builder
showtToShowb sf = fromText . sf
{-# INLINE showtToShowb #-}

-- | Convert a lazy 'TL.Text'-based show function to a 'Builder'-based one.
--
-- /Since: 3.4/
showtlToShowb :: (a -> TL.Text) -> a -> Builder
showtlToShowb sf = fromLazyText . sf
{-# INLINE showtlToShowb #-}

-- | Convert a precedence-aware 'Builder'-based show function to a 'ShowS'-based one.
--
-- /Since: 3/
showbPrecToShowsPrec :: (Int -> a -> Builder) -> Int -> a -> ShowS
showbPrecToShowsPrec sp p = showString . toString . sp p
{-# INLINE showbPrecToShowsPrec #-}

-- | Convert a precedence-aware 'Builder'-based show function to a strict 'TS.Text'-based one.
--
-- /Since: 3.4/
showbPrecToShowtPrec :: (Int -> a -> Builder) -> Int -> a -> TS.Text
showbPrecToShowtPrec sp p = toText . sp p
{-# INLINE showbPrecToShowtPrec #-}

-- | Convert a precedence-aware 'Builder'-based show function to a lazy 'TL.Text'-based one.
--
-- /Since: 3.4/
showbPrecToShowtlPrec :: (Int -> a -> Builder) -> Int -> a -> TL.Text
showbPrecToShowtlPrec sp p = toLazyText . sp p
{-# INLINE showbPrecToShowtlPrec #-}

-- | Convert a 'Builder'-based show function to a 'ShowS'-based one.
--
-- /Since: 3/
showbToShows :: (a -> Builder) -> a -> ShowS
showbToShows sf = showString . toString . sf
{-# INLINE showbToShows #-}

-- | Convert a 'Builder'-based show function to a strict 'TS.Text'-based one.
--
-- /Since: 3/
showbToShowt :: (a -> Builder) -> a -> TS.Text
showbToShowt sf = toText . sf
{-# INLINE showbToShowt #-}

-- | Convert a 'Builder'-based show function to a lazy 'TL.Text'-based one.
--
-- /Since: 3/
showbToShowtl :: (a -> Builder) -> a -> TL.Text
showbToShowtl sf = toLazyText . sf
{-# INLINE showbToShowtl #-}

-------------------------------------------------------------------------------

-- | Lifting of the 'TextShow' class to unary type constructors.
--
-- /Since: 2/
class TextShow1 f where
    -- | 'showbPrec' function for an application of the type constructor
    -- based on 'showbPrec' and 'showbList' functions for the argument type.
    --
    -- /Since: 3/
    liftShowbPrec :: (Int -> a -> Builder) -> ([a] -> Builder)
                  -> Int -> f a -> Builder

    -- | 'showbList' function for an application of the type constructor
    -- based on 'showbPrec' and 'showbList' functions for the argument type.
    -- The default implementation using standard list syntax is correct
    -- for most types.
    --
    -- /Since: 3/
    liftShowbList :: (Int -> a -> Builder) -> ([a] -> Builder)
                  -> [f a] -> Builder
    liftShowbList sp sl = showbListWith (liftShowbPrec sp sl 0)

#if __GLASGOW_HASKELL__ >= 708
    {-# MINIMAL liftShowbPrec #-}

deriving instance Typeable TextShow1
#endif

-- | Lift the standard 'showbPrec' and 'showbList' functions through the
-- type constructor.
--
-- /Since: 2/
showbPrec1 :: (TextShow1 f, TextShow a) => Int -> f a -> Builder
showbPrec1 = liftShowbPrec showbPrec showbList
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

-- | 'showtPrec' function for an application of the type constructor
-- based on 'showtPrec' and 'showtList' functions for the argument type.
--
-- The current implementation is based on `liftShowbPrec` internally.
--
-- /Since: 3.4/
liftShowtPrec :: TextShow1 f => (Int -> a -> TS.Text) -> ([a] -> TS.Text)
              -> Int -> f a -> TS.Text
liftShowtPrec sp sl = showbPrecToShowtPrec $ liftShowbPrec (showtPrecToShowbPrec sp) (showtToShowb sl)

-- | 'showtlPrec' function for an application of the type constructor
-- based on 'showtlPrec' and 'showtlList' functions for the argument type.
--
-- The current implementation is based on `liftShowbPrec` internally.
--
-- /Since: 3.4/
liftShowtlPrec :: TextShow1 f => (Int -> a -> TL.Text) -> ([a] -> TL.Text)
               -> Int -> f a -> TL.Text
liftShowtlPrec sp sl = showbPrecToShowtlPrec $ liftShowbPrec (showtlPrecToShowbPrec sp) (showtlToShowb sl)

-------------------------------------------------------------------------------

-- | Lifting of the 'TextShow' class to binary type constructors.
--
-- /Since: 2/
class TextShow2 f where
    -- | 'showbPrec' function for an application of the type constructor
    -- based on 'showbPrec' and 'showbList' functions for the argument types.
    --
    -- /Since: 3/
    liftShowbPrec2 :: (Int -> a -> Builder) -> ([a] -> Builder)
                   -> (Int -> b -> Builder) -> ([b] -> Builder)
                   -> Int -> f a b -> Builder

    -- | 'showbList' function for an application of the type constructor
    -- based on 'showbPrec' and 'showbList' functions for the argument types.
    -- The default implementation using standard list syntax is correct
    -- for most types.
    --
    -- /Since: 3/
    liftShowbList2 :: (Int -> a -> Builder) -> ([a] -> Builder)
                   -> (Int -> b -> Builder) -> ([b] -> Builder)
                   -> [f a b] -> Builder
    liftShowbList2 sp1 sl1 sp2 sl2 =
        showbListWith (liftShowbPrec2 sp1 sl1 sp2 sl2 0)

#if __GLASGOW_HASKELL__ >= 708
    {-# MINIMAL liftShowbPrec2 #-}

deriving instance Typeable TextShow2
#endif

-- | Lift two 'showbPrec' functions through the type constructor.
--
-- /Since: 2/
showbPrec2 :: (TextShow2 f, TextShow a, TextShow b) => Int -> f a b -> Builder
showbPrec2 = liftShowbPrec2 showbPrec showbList showbPrec showbList
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

-- | 'showtPrec' function for an application of the type constructor
-- based on 'showtPrec' and 'showtList' functions for the argument type.
--
-- The current implementation is based on `liftShowbPrec2` internally.
--
-- /Since: 3.4/
liftShowtPrec2 :: TextShow2 f
               => (Int -> a -> TS.Text) -> ([a] -> TS.Text)
               -> (Int -> b -> TS.Text) -> ([b] -> TS.Text)
               -> Int -> f a b -> TS.Text
liftShowtPrec2 sp1 sl1 sp2 sl2 = showbPrecToShowtPrec $
    liftShowbPrec2 (showtPrecToShowbPrec sp1) (showtToShowb sl1)
                   (showtPrecToShowbPrec sp2) (showtToShowb sl2)

-- | 'showtlPrec' function for an application of the type constructor
-- based on 'showtlPrec' and 'showtlList' functions for the argument type.
--
-- The current implementation is based on `liftShowbPrec2` internally.
--
-- /Since: 3.4/
liftShowtlPrec2 :: TextShow2 f
                => (Int -> a -> TL.Text) -> ([a] -> TL.Text)
                -> (Int -> b -> TL.Text) -> ([b] -> TL.Text)
                -> Int -> f a b -> TL.Text
liftShowtlPrec2 sp1 sl1 sp2 sl2 = showbPrecToShowtlPrec $
    liftShowbPrec2 (showtlPrecToShowbPrec sp1) (showtlToShowb sl1)
                   (showtlPrecToShowbPrec sp2) (showtlToShowb sl2)
