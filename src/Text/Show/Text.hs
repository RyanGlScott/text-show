{-# LANGUAGE OverloadedStrings #-}
module Text.Show.Text (
      Show(..)
    , show
    , showbParen
    , showbLitChar
    , showbLitString
    , unlinesB
    , unwordsB
    , print
    ) where

import           Data.Array
import           Data.Int
import           Data.Complex
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Monoid
import           Data.Ratio
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Text (Text)
import           Data.Text.IO
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (Builder, toLazyText)
import           Data.Text.Lazy.Builder.Int
import           Data.Text.Lazy.Builder.RealFloat
import           Data.Word

import           Prelude hiding (Show(..), print, putStrLn)

import           Text.Show.Text.Char
import           Text.Show.Text.Util

class Show a where
    showbPrec :: Int -> a -> Builder
    showb :: a -> Builder
    showbList :: [a] -> Builder
    
    showbPrec _ = showb
    
    showb = showbPrec 0
    
    showbList []     = "[]"                      -- "[]"
    showbList (x:xs) = s '[' <> showb x <> go xs -- "[..
      where
        go (y:ys) = s ',' <> showb y <> go ys    -- ..,..
        go []     = s ']'                        -- ..]"

show :: Show a => a -> Text
show = toStrict . toLazyText . showb

showbParen :: Bool -> Builder -> Builder
showbParen p builder | p         = s '(' <> builder <> s ')'
                     | otherwise = builder

print :: Show a => a -> IO ()
print = putStrLn . show

unlinesB :: [Builder] -> Builder
unlinesB (b:bs) = b <> s '\n' <> unlinesB bs
unlinesB []     = mempty

unwordsB :: [Builder] -> Builder
unwordsB (b:bs@(_:_)) = b <> s ' ' <> unwordsB bs
unwordsB [b]          = b
unwordsB []           = mempty

instance Show Builder where
    showb b = b

instance Show () where
    showb () = "()"

instance Show Char where
    showb '\'' = "'\\''"
    showb c    = s '\'' <> showbLitChar c <> s '\''
    showbList cs = s '"' <> showbLitString cs <> s '"'

instance Show Bool where
    showb True  = "True"
    showb False = "False"

instance Show a => Show [a] where
    showb = showbList

instance Show Int where
    showb = decimal
    showbPrec k i = showbParen (i < 0 && k > 0) $ decimal i

instance Show Int8 where
    showb = decimal
    showbPrec k i = showbParen (i < 0 && k > 0) $ decimal i

instance Show Int16 where
    showb = decimal
    showbPrec k i = showbParen (i < 0 && k > 0) $ decimal i

instance Show Int32 where
    showb = decimal
    showbPrec k i = showbParen (i < 0 && k > 0) $ decimal i

instance Show Int64 where
    showb = decimal
    showbPrec k i = showbParen (i < 0 && k > 0) $ decimal i

instance Show Word where
    showb = decimal

instance Show Word8 where
    showb = decimal

instance Show Word16 where
    showb = decimal

instance Show Word32 where
    showb = decimal

instance Show Word64 where
    showb = decimal

instance Show Integer where
    showb = decimal
    showbPrec k i = showbParen (i < 0 && k > 0) $ decimal i

instance Show Float where
    showb = realFloat
    showbPrec k f = showbParen (f < 0 && k > 0) $ realFloat f

instance Show Double where
    showb = realFloat
    showbPrec k f = showbParen (f < 0 && k > 0) $ realFloat f

instance (Show a, Integral a) => Show (Ratio a) where
    showbPrec k q = showbParen (k > 7) $ showbPrec 8 (numerator q) <>
                    s '%' <> showb (denominator q)

instance (Show a, RealFloat a) => Show (Complex a) where
    showbPrec k (a :+ b) = showbParen (k > 6) $ showbPrec 7 a <> " :+ " <> showbPrec 7 b

instance Show a => Show (Maybe a) where
    showbPrec _ Nothing  = "Nothing"
    showbPrec k (Just a) = showbParen (k > 10) $ "Just " <> showbPrec 11 a

instance (Show a, Show b) => Show (Either a b) where
    showbPrec k (Left a)  = showbParen (k > 10) $ "Left " <> showbPrec 11 a
    showbPrec k (Right b) = showbParen (k > 10) $ "Right " <> showbPrec 11 b

instance Show Ordering where
    showb LT = "LT"
    showb EQ = "EQ"
    showb GT = "GT"

instance (Show a, Show b) => Show (a, b) where
    showb (a, b) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ')'

instance (Show a, Show b, Show c) => Show (a, b, c) where
    showb (a, b, c) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ',' <> showb c <>
      s ')'

instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
    showb (a, b, c, d) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ',' <> showb c <>
      s ',' <> showb d <>
      s ')'

instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
    showb (a, b, c, d, e) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ',' <> showb c <>
      s ',' <> showb d <>
      s ',' <> showb e <>
      s ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (a, b, c, d, e, f) where
    showb (a, b, c, d, e, f) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ',' <> showb c <>
      s ',' <> showb d <>
      s ',' <> showb e <>
      s ',' <> showb f <>
      s ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => Show (a, b, c, d, e, f, g) where
    showb (a, b, c, d, e, f, g) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ',' <> showb c <>
      s ',' <> showb d <>
      s ',' <> showb e <>
      s ',' <> showb f <>
      s ',' <> showb g <>
      s ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) => Show (a, b, c, d, e, f, g, h) where
    showb (a, b, c, d, e, f, g, h) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ',' <> showb c <>
      s ',' <> showb d <>
      s ',' <> showb e <>
      s ',' <> showb f <>
      s ',' <> showb g <>
      s ',' <> showb h <>
      s ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i) => Show (a, b, c, d, e, f, g, h, i) where
    showb (a, b, c, d, e, f, g, h, i) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ',' <> showb c <>
      s ',' <> showb d <>
      s ',' <> showb e <>
      s ',' <> showb f <>
      s ',' <> showb g <>
      s ',' <> showb h <>
      s ',' <> showb i <>
      s ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j) => Show (a, b, c, d, e, f, g, h, i, j) where
    showb (a, b, c, d, e, f, g, h, i, j) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ',' <> showb c <>
      s ',' <> showb d <>
      s ',' <> showb e <>
      s ',' <> showb f <>
      s ',' <> showb g <>
      s ',' <> showb h <>
      s ',' <> showb i <>
      s ',' <> showb j <>
      s ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k) => Show (a, b, c, d, e, f, g, h, i, j, k) where
    showb (a, b, c, d, e, f, g, h, i, j, k) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ',' <> showb c <>
      s ',' <> showb d <>
      s ',' <> showb e <>
      s ',' <> showb f <>
      s ',' <> showb g <>
      s ',' <> showb h <>
      s ',' <> showb i <>
      s ',' <> showb j <>
      s ',' <> showb k <>
      s ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l) => Show (a, b, c, d, e, f, g, h, i, j, k, l) where
    showb (a, b, c, d, e, f, g, h, i, j, k, l) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ',' <> showb c <>
      s ',' <> showb d <>
      s ',' <> showb e <>
      s ',' <> showb f <>
      s ',' <> showb g <>
      s ',' <> showb h <>
      s ',' <> showb i <>
      s ',' <> showb j <>
      s ',' <> showb k <>
      s ',' <> showb l <>
      s ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m) => Show (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    showb (a, b, c, d, e, f, g, h, i, j, k, l, m) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ',' <> showb c <>
      s ',' <> showb d <>
      s ',' <> showb e <>
      s ',' <> showb f <>
      s ',' <> showb g <>
      s ',' <> showb h <>
      s ',' <> showb i <>
      s ',' <> showb j <>
      s ',' <> showb k <>
      s ',' <> showb l <>
      s ',' <> showb m <>
      s ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n) => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    showb (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ',' <> showb c <>
      s ',' <> showb d <>
      s ',' <> showb e <>
      s ',' <> showb f <>
      s ',' <> showb g <>
      s ',' <> showb h <>
      s ',' <> showb i <>
      s ',' <> showb j <>
      s ',' <> showb k <>
      s ',' <> showb l <>
      s ',' <> showb m <>
      s ',' <> showb n <>
      s ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o) => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    showb (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =
      s '(' <> showb a <>
      s ',' <> showb b <>
      s ',' <> showb c <>
      s ',' <> showb d <>
      s ',' <> showb e <>
      s ',' <> showb f <>
      s ',' <> showb g <>
      s ',' <> showb h <>
      s ',' <> showb i <>
      s ',' <> showb j <>
      s ',' <> showb k <>
      s ',' <> showb l <>
      s ',' <> showb m <>
      s ',' <> showb n <>
      s ',' <> showb o <>
      s ')'

instance (Show i, Show e, Ix i) => Show (Array i e) where
    showbPrec k a = showbParen (k > 10) $ "array " <>
                    showb (bounds a) <> s ' ' <> showb (assocs a)

instance (Show k, Show v) => Show (Map k v) where
    showbPrec k m = showbParen (k > 10) $ "fromList " <>
                    showb (M.toList m)

instance Show e => Show (Set e) where
    showbPrec k set = showbParen (k > 10) $ "fromList " <>
                      showb (S.toList set)