{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFoldable, DeriveFunctor,
             DeriveTraversable, GeneralizedNewtypeDeriving, OverloadedStrings #-}
#if MIN_VERSION_base(4,4,0)
{-# LANGUAGE DeriveGeneric #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE TypeFamilies #-}
#endif
{-|
Module:      Text.Show.Text.Classes
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

The 'Show' and 'Show1' typeclasses.
-}
module Text.Show.Text.Classes where

#if !(MIN_VERSION_base(4,8,0))
import           Control.Applicative (Applicative((<*>), pure))
import           Data.Foldable (Foldable)
import           Data.Monoid (Monoid)
import           Data.Traversable (Traversable)
#endif

import           Control.Monad.Fix (MonadFix(..))
#if MIN_VERSION_base(4,4,0)
import           Control.Monad.Zip (MonadZip(..))
#endif

import           Data.Bits (Bits)
#if MIN_VERSION_base(4,7,0)
import           Data.Bits (FiniteBits)
#endif
import           Data.Data (Data, Typeable)
import           Data.Functor ((<$>))
import           Data.Ix (Ix)
import           Data.Semigroup (Semigroup)
import           Data.String (IsString)
import           Data.Text         as TS (Text)
import qualified Data.Text.IO      as TS (putStrLn, hPutStrLn)
import qualified Data.Text.Lazy    as TL (Text)
import qualified Data.Text.Lazy.IO as TL (putStrLn, hPutStrLn)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (Builder, fromString, toLazyText)

import           Foreign.Storable (Storable)

#if __GLASGOW_HASKELL__ >= 708
import           GHC.Exts (IsList(Item, fromList, toList))
#endif
#if MIN_VERSION_base(4,4,0)
import           GHC.Generics (Generic)
# if __GLASGOW_HASKELL__ >= 706
import           GHC.Generics (Generic1)
# endif
#endif
import           GHC.Show (appPrec, appPrec1)

import           Prelude hiding (Show(..))

import           System.IO (Handle)

import           Text.Printf (PrintfArg, PrintfType)
import           Text.Read (Read(..), readListPrecDefault)
import qualified Text.Show as S (Show(..))
import           Text.Show.Text.Utils ((<>), s, toString)

#include "inline.h"

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
-- instances using Template Haskell, or the "Text.Show.Text.Generic" module to
-- quickly define 'Show' instances using 'genericShowbPrec'.
-- 
-- /Since: 0.1/
class Show a where
    -- | Convert a value to a 'Builder' with the given predence.
    -- 
    -- /Since: 0.1/
    showbPrec :: Int -- ^ The operator precedence of the enclosing context (a number
                     -- from @0@ to @11@). Function application has precedence @10@.
              -> a   -- ^ The value to be converted to a 'String'.
              -> Builder
    
    -- | A specialized variant of 'showbPrec' using precedence context zero.
    -- 
    -- /Since: 0.1/
    showb :: a -> Builder
    
    -- | Allows for specialized display of lists. This is used, for example, when
    -- showing lists of 'Char's.
    -- 
    -- /Since: 0.1/
    showbList :: [a] -> Builder
    
    showbPrec _ = showb
    
    showb = showbPrec 0
    
    showbList = showbListWith showb
#if __GLASGOW_HASKELL__ >= 708
    {-# MINIMAL showbPrec | showb #-}
#endif

-- | Lifting of the 'Show' class to unary type constructors.
-- 
-- /Since: 0.5/
class Show1 f where
    -- | 'Builder' conversion for values of a type that has a unary type constructor.
    -- 
    -- /Since: 0.5/
    showbPrec1 :: Show a => Int -> f a -> Builder

-- | Constructs a strict 'TS.Text' from a single value.
-- 
-- /Since: 0.1/
show :: Show a => a -> TS.Text
show = toStrict . showLazy
{-# INLINE show #-}

-- | Constructs a lazy 'TL.Text' from a single value.
-- 
-- /Since: 0.3/
showLazy :: Show a => a -> TL.Text
showLazy = toLazyText . showb
{-# INLINE showLazy #-}

-- | Constructs a strict 'TS.Text' from a single value with the given precedence.
-- 
-- /Since: 0.3/
showPrec :: Show a => Int -> a -> TS.Text
showPrec p = toStrict . showPrecLazy p
{-# INLINE showPrec #-}

-- | Constructs a lazy 'TL.Text' from a single value with the given precedence.
-- 
-- /Since: 0.3/
showPrecLazy :: Show a => Int -> a -> TL.Text
showPrecLazy p = toLazyText . showbPrec p
{-# INLINE showPrecLazy #-}

-- | Construct a strict 'TS.Text' from a list of values.
-- 
-- /Since: 0.3.1/
showList :: Show a => [a] -> TS.Text
showList = toStrict . showListLazy
{-# INLINE showList #-}

-- | Construct a lazy 'TL.Text' from a list of values.
-- 
-- /Since: 0.3.1/
showListLazy :: Show a => [a] -> TL.Text
showListLazy = toLazyText . showbList
{-# INLINE showListLazy #-}

-- | Surrounds 'Builder' output with parentheses if the 'Bool' parameter is 'True'.
-- 
-- /Since: 0.1/
showbParen :: Bool -> Builder -> Builder
showbParen p builder | p         = s '(' <> builder <> s ')'
                     | otherwise = builder
{-# INLINE showbParen #-}

-- | Construct a 'Builder' containing a single space character.
-- 
-- /Since: 0.5/
showbSpace :: Builder
showbSpace = s ' '
{-# INLINE showbSpace #-}

-- | Converts a list of values into a 'Builder' in which the values are surrounded
-- by square brackets and each value is separated by a comma. The function argument
-- controls how each element is shown.

-- @'showbListWith' 'showb'@ is the default implementation of 'showbList' save for
-- a few special cases (e.g., 'String').
-- 
-- /Since: 0.7/
showbListWith :: (a -> Builder) -> [a] -> Builder
showbListWith _      []     = "[]"
showbListWith showbx (x:xs) = s '[' <> showbx x <> go xs -- "[..
  where
    go (y:ys) = s ',' <> showbx y <> go ys               -- ..,..
    go []     = s ']'                                    -- ..]"
{-# INLINE showbListWith #-}

-- | @'showbUnary' n p x@ produces the 'Builder' representation of a unary data
-- constructor with name @n@ and argument @x@, in precedence context @p@.
-- 
-- /Since: 0.5/
showbUnary :: Show a => Builder -> Int -> a -> Builder
showbUnary nameB p x = showbParen (p > appPrec) $
    nameB <> showbSpace <> showbPrec appPrec1 x
{-# INLINE showbUnary #-}

-- | @'showbUnary1' n p x@ produces the 'Builder' representation of a unary data
-- constructor with name @n@ and argument @x@, in precedence context @p@.
-- 
-- /Since: 0.5/
showbUnary1 :: (Show1 f, Show a) => Builder -> Int -> f a -> Builder
showbUnary1 nameB p x = showbParen (p > appPrec) $
    nameB <> showbSpace <> showbPrec1 appPrec1 x
{-# INLINE showbUnary1 #-}

-- | @'showbBinary1' n p x y@ produces the 'Builder' representation of a binary
-- data constructor with name @n@ and arguments @x@ and @y@, in precedence
-- context @p@.
-- 
-- /Since: 0.5/
showbBinary1 :: (Show1 f, Show1 g, Show a) => Builder -> Int -> f a -> g a -> Builder
showbBinary1 nameB p x y = showbParen (p > appPrec) $ nameB
    <> showbSpace <> showbPrec1 appPrec1 x
    <> showbSpace <> showbPrec1 appPrec1 y
{-# INLINE showbBinary1 #-}

-- | Writes a value's strict 'TS.Text' representation to the standard output, followed
--   by a newline.
-- 
-- /Since: 0.1/
print :: Show a => a -> IO ()
print = TS.putStrLn . show
{-# INLINE print #-}

-- | Writes a value's lazy 'TL.Text' representation to the standard output, followed
--   by a newline.
-- 
-- /Since: 0.3/
printLazy :: Show a => a -> IO ()
printLazy = TL.putStrLn . showLazy
{-# INLINE printLazy #-}

-- | Writes a value's strict 'TS.Text' representation to a file handle, followed
--   by a newline.
-- 
-- /Since: 0.3/
hPrint :: Show a => Handle -> a -> IO ()
hPrint h = TS.hPutStrLn h . show
{-# INLINE hPrint #-}

-- | Writes a value's lazy 'TL.Text' representation to a file handle, followed
--   by a newline.
-- 
-- /Since: 0.3/
hPrintLazy :: Show a => Handle -> a -> IO ()
hPrintLazy h = TL.hPutStrLn h . showLazy
{-# INLINE hPrintLazy #-}

-- | The @Text@ 'T.Show' instance for 'FromStringShow' is based on its @String@
-- 'S.Show' instance. That is,
-- 
-- @
-- showbPrec p ('FromStringShow' x) = 'fromString' (showsPrec p x "")
-- @
-- 
-- /Since: 0.5/
newtype FromStringShow a = FromStringShow { fromStringShow :: a }
  deriving ( Bits
           , Bounded
           , Data
           , Enum
           , Eq
#if MIN_VERSION_base(4,7,0)
           , FiniteBits
#endif
           , Floating
           , Foldable
           , Fractional
           , Functor
#if MIN_VERSION_base(4,4,0)
           , Generic
# if __GLASGOW_HASKELL__ >= 706
           , Generic1
# endif
#endif
           , Integral
           , IsString
           , Ix
           , Monoid
           , Num
           , Ord
           , PrintfArg
           , PrintfType
           , Real
           , RealFloat
           , RealFrac
           , Semigroup
           , Storable
           , Traversable
           , Typeable
           )

instance Applicative FromStringShow where
    pure = FromStringShow
    INLINE_INST_FUN(pure)
    
    FromStringShow f <*> FromStringShow x = FromStringShow $ f x
    INLINE_INST_FUN((<*>))

#if __GLASGOW_HASKELL__ >= 708
instance IsList a => IsList (FromStringShow a) where
    type Item (FromStringShow a) = Item a
    fromList = FromStringShow . fromList
    {-# INLINE fromList #-}
    toList = toList . fromStringShow
    {-# INLINE toList #-}
#endif

instance Monad FromStringShow where
    return = FromStringShow
    INLINE_INST_FUN(return)
    
    FromStringShow a >>= f = f a
    INLINE_INST_FUN((>>=))

instance MonadFix FromStringShow where
    mfix f = FromStringShow $ let FromStringShow a = f a in a
    INLINE_INST_FUN(mfix)

#if MIN_VERSION_base(4,4,0)
instance MonadZip FromStringShow where
    mzip (FromStringShow a) (FromStringShow b) = FromStringShow (a, b)
    INLINE_INST_FUN(mzip)
    
    mzipWith f (FromStringShow a) (FromStringShow b) = FromStringShow $ f a b
    INLINE_INST_FUN(mzipWith)
    
    munzip (FromStringShow (a, b)) = (FromStringShow a, FromStringShow b)
    INLINE_INST_FUN(munzip)
#endif

instance Read a => Read (FromStringShow a) where
    readPrec = FromStringShow <$> readPrec
    INLINE_INST_FUN(readPrec)
    
    readListPrec = readListPrecDefault
    INLINE_INST_FUN(readListPrec)

instance S.Show a => Show (FromStringShow a) where
    showbPrec p (FromStringShow x) = fromString $ S.showsPrec p x ""
    INLINE_INST_FUN(showbPrec)

instance S.Show a => S.Show (FromStringShow a) where
    showsPrec p (FromStringShow x) = S.showsPrec p x
    INLINE_INST_FUN(showsPrec)

-- | The @String@ 'S.Show' instance for 'FromTextShow' is based on its @Text@
-- 'T.Show' instance. That is,
-- 
-- @
-- showsPrec p ('FromTextShow' x) str = 'toString' (showbPrec p x) ++ str
-- @
-- 
-- /Since: 0.6/
newtype FromTextShow a = FromTextShow { fromTextShow :: a }
  deriving ( Bits
           , Bounded
           , Data
           , Enum
           , Eq
#if MIN_VERSION_base(4,7,0)
           , FiniteBits
#endif
           , Floating
           , Foldable
           , Fractional
           , Functor
#if MIN_VERSION_base(4,4,0)
           , Generic
# if __GLASGOW_HASKELL__ >= 706
           , Generic1
# endif
#endif
           , Integral
           , IsString
           , Ix
           , Monoid
           , Num
           , Ord
           , PrintfArg
           , PrintfType
           , Real
           , RealFloat
           , RealFrac
           , Semigroup
           , Show
           , Storable
           , Traversable
           , Typeable
           )

instance Applicative FromTextShow where
    pure = FromTextShow
    INLINE_INST_FUN(pure)
    
    FromTextShow f <*> FromTextShow x = FromTextShow $ f x
    INLINE_INST_FUN((<*>))

#if __GLASGOW_HASKELL__ >= 708
instance IsList a => IsList (FromTextShow a) where
    type Item (FromTextShow a) = Item a
    fromList = FromTextShow . fromList
    {-# INLINE fromList #-}
    toList = toList . fromTextShow
    {-# INLINE toList #-}
#endif

instance Monad FromTextShow where
    return = FromTextShow
    INLINE_INST_FUN(return)
    
    FromTextShow a >>= f = f a
    INLINE_INST_FUN((>>=))

instance MonadFix FromTextShow where
    mfix f = FromTextShow $ let FromTextShow a = f a in a
    INLINE_INST_FUN(mfix)

#if MIN_VERSION_base(4,4,0)
instance MonadZip FromTextShow where
    mzip (FromTextShow a) (FromTextShow b) = FromTextShow (a, b)
    INLINE_INST_FUN(mzip)
    
    mzipWith f (FromTextShow a) (FromTextShow b) = FromTextShow $ f a b
    INLINE_INST_FUN(mzipWith)
    
    munzip (FromTextShow (a, b)) = (FromTextShow a, FromTextShow b)
    INLINE_INST_FUN(munzip)
#endif

instance Read a => Read (FromTextShow a) where
    readPrec = FromTextShow <$> readPrec
    INLINE_INST_FUN(readPrec)
    
    readListPrec = readListPrecDefault
    INLINE_INST_FUN(readListPrec)

instance Show a => S.Show (FromTextShow a) where
    showsPrec p (FromTextShow x) str = toString (showbPrec p x) ++ str
    INLINE_INST_FUN(showsPrec)

instance Show1 FromTextShow where
    showbPrec1 = showbPrec
    INLINE_INST_FUN(showbPrec1)