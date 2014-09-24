module Text.Show.Text (
      Show(..)
    , show
    , unlinesB
    , unwordsB
    , print
    ) where

-- import Data.Int
import Data.Complex
import Data.Monoid
import Data.Ratio
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, toLazyText)
-- import Data.Word

import Prelude hiding (Show(..))

import Text.Show.Text.Util

class Show a where
    showbPrec :: Int -> a -> Builder
    showb :: a -> Builder
    showbList :: [a] -> Builder
    
    showbPrec _ = showb
    
    showb = showbPrec 0
    
    showbList []     = c '[' <> c ']'            -- "[]"
    showbList (x:xs) = c '[' <> showb x <> go xs -- "[..
      where
        go (y:ys) = c ',' <> showb y <> go ys    -- ..,..
        go []     = c ']'                        -- ..]"

show :: Show a => a -> Text
show = toStrict . toLazyText . showb

showbParen :: Bool -> Builder -> Builder
showbParen p builder | p         = c '(' <> builder <> c ')'
                     | otherwise = builder

-- print :: Show a => a -> IO ()
-- print = putStrLn . show

unlinesB :: [Builder] -> Builder
unlinesB (b:bs) = b <> c '\n' <> unlinesB bs
unlinesB []     = mempty

unwordsB :: [Builder] -> Builder
unwordsB (b:bs@(_:_)) = b <> c ' ' <> unwordsB bs
unwordsB [b]          = b
unwordsB []           = mempty

instance Show Builder where
    showb b = b

instance Show () where
    showb () = c '(' <> c ')'

-- instance Show Char where
--     

instance Show Bool where
    showb True  = c 'T' <> c 'r' <> c 'u' <> c 'e'
    showb False = c 'F' <> c 'a' <> c 'l' <> c 's' <> c 'e'

instance Show a => Show [a] where
    showb = showbList

-- instance Show Int where
--     

-- instance Show Int8 where
--     

-- instance Show Int16 where
--     

-- instance Show Int32 where
--     

-- instance Show Int64 where
--     

-- instance Show Word where
--     

-- instance Show Word8 where
--      

-- instance Show Word16 where
--      

-- instance Show Word32 where
--      

-- instance Show Word64 where
--      

-- instance Show Integer where
--     

-- instance Show Float where
--      

-- instance Show Double where
--     

instance (Show a, Integral a) => Show (Ratio a) where
    showbPrec k q = showbParen (k > 7) $ showbPrec 8 (numerator q) <>
                    c '%' <> showb (denominator q)

instance (Show a, RealFloat a) => Show (Complex a) where
    showbPrec k (a :+ b) = showbParen (k > 6) $ showbPrec 7 a <> c ' ' <> 
                           c ':' <> c '+' <> c ' ' <> showbPrec 7 b

instance Show a => Show (Maybe a) where
    showbPrec _ Nothing  = c 'N' <> c 'o' <> c 't' <> c 'h' <> c 'i' <> c 'n' <> c 'g'
    showbPrec k (Just a) = showbParen (k > 10) $
                           c 'J' <> c 'u' <> c 's' <> c 't' <> c ' ' <>
                           showbPrec 11 a

instance (Show a, Show b) => Show (Either a b) where
    showbPrec k (Left a)  = showbParen (k > 10) $
                            c 'L' <> c 'e' <> c 'f' <> c 't' <> c ' ' <>
                            showbPrec 11 a
    showbPrec k (Right b) = showbParen (k > 10) $
                            c 'R' <> c 'i' <> c 'g' <> c 'h' <> c 't' <> c ' ' <>
                            showbPrec 11 b

instance Show Ordering where
    showb LT = c 'L' <> c 'T'
    showb EQ = c 'E' <> c 'Q'
    showb GT = c 'G' <> c 'T'

instance (Show a, Show b) => Show (a, b) where
    showb (a, b) =
      c '(' <> showb a <>
      c ',' <> showb b <>
      c ')'

instance (Show a, Show b, Show c) => Show (a, b, c) where
    showb (a, b, c') =
      c '(' <> showb a  <>
      c ',' <> showb b  <>
      c ',' <> showb c' <>
      c ')'

instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
    showb (a, b, c', d) =
      c '(' <> showb a  <>
      c ',' <> showb b  <>
      c ',' <> showb c' <>
      c ',' <> showb d  <>
      c ')'

instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
    showb (a, b, c', d, e) =
      c '(' <> showb a  <>
      c ',' <> showb b  <>
      c ',' <> showb c' <>
      c ',' <> showb d  <>
      c ',' <> showb e  <>
      c ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (a, b, c, d, e, f) where
    showb (a, b, c', d, e, f) =
      c '(' <> showb a  <>
      c ',' <> showb b  <>
      c ',' <> showb c' <>
      c ',' <> showb d  <>
      c ',' <> showb e  <>
      c ',' <> showb f  <>
      c ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => Show (a, b, c, d, e, f, g) where
    showb (a, b, c', d, e, f, g) =
      c '(' <> showb a  <>
      c ',' <> showb b  <>
      c ',' <> showb c' <>
      c ',' <> showb d  <>
      c ',' <> showb e  <>
      c ',' <> showb f  <>
      c ',' <> showb g  <>
      c ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) => Show (a, b, c, d, e, f, g, h) where
    showb (a, b, c', d, e, f, g, h) =
      c '(' <> showb a  <>
      c ',' <> showb b  <>
      c ',' <> showb c' <>
      c ',' <> showb d  <>
      c ',' <> showb e  <>
      c ',' <> showb f  <>
      c ',' <> showb g  <>
      c ',' <> showb h  <>
      c ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i) => Show (a, b, c, d, e, f, g, h, i) where
    showb (a, b, c', d, e, f, g, h, i) =
      c '(' <> showb a  <>
      c ',' <> showb b  <>
      c ',' <> showb c' <>
      c ',' <> showb d  <>
      c ',' <> showb e  <>
      c ',' <> showb f  <>
      c ',' <> showb g  <>
      c ',' <> showb h  <>
      c ',' <> showb i  <>
      c ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j) => Show (a, b, c, d, e, f, g, h, i, j) where
    showb (a, b, c', d, e, f, g, h, i, j) =
      c '(' <> showb a  <>
      c ',' <> showb b  <>
      c ',' <> showb c' <>
      c ',' <> showb d  <>
      c ',' <> showb e  <>
      c ',' <> showb f  <>
      c ',' <> showb g  <>
      c ',' <> showb h  <>
      c ',' <> showb i  <>
      c ',' <> showb j  <>
      c ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k) => Show (a, b, c, d, e, f, g, h, i, j, k) where
    showb (a, b, c', d, e, f, g, h, i, j, k) =
      c '(' <> showb a  <>
      c ',' <> showb b  <>
      c ',' <> showb c' <>
      c ',' <> showb d  <>
      c ',' <> showb e  <>
      c ',' <> showb f  <>
      c ',' <> showb g  <>
      c ',' <> showb h  <>
      c ',' <> showb i  <>
      c ',' <> showb j  <>
      c ',' <> showb k  <>
      c ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l) => Show (a, b, c, d, e, f, g, h, i, j, k, l) where
    showb (a, b, c', d, e, f, g, h, i, j, k, l) =
      c '(' <> showb a  <>
      c ',' <> showb b  <>
      c ',' <> showb c' <>
      c ',' <> showb d  <>
      c ',' <> showb e  <>
      c ',' <> showb f  <>
      c ',' <> showb g  <>
      c ',' <> showb h  <>
      c ',' <> showb i  <>
      c ',' <> showb j  <>
      c ',' <> showb k  <>
      c ',' <> showb l  <>
      c ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m) => Show (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    showb (a, b, c', d, e, f, g, h, i, j, k, l, m) =
      c '(' <> showb a  <>
      c ',' <> showb b  <>
      c ',' <> showb c' <>
      c ',' <> showb d  <>
      c ',' <> showb e  <>
      c ',' <> showb f  <>
      c ',' <> showb g  <>
      c ',' <> showb h  <>
      c ',' <> showb i  <>
      c ',' <> showb j  <>
      c ',' <> showb k  <>
      c ',' <> showb l  <>
      c ',' <> showb m  <>
      c ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n) => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    showb (a, b, c', d, e, f, g, h, i, j, k, l, m, n) =
      c '(' <> showb a  <>
      c ',' <> showb b  <>
      c ',' <> showb c' <>
      c ',' <> showb d  <>
      c ',' <> showb e  <>
      c ',' <> showb f  <>
      c ',' <> showb g  <>
      c ',' <> showb h  <>
      c ',' <> showb i  <>
      c ',' <> showb j  <>
      c ',' <> showb k  <>
      c ',' <> showb l  <>
      c ',' <> showb m  <>
      c ',' <> showb n  <>
      c ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o) => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    showb (a, b, c', d, e, f, g, h, i, j, k, l, m, n, o) =
      c '(' <> showb a  <>
      c ',' <> showb b  <>
      c ',' <> showb c' <>
      c ',' <> showb d  <>
      c ',' <> showb e  <>
      c ',' <> showb f  <>
      c ',' <> showb g  <>
      c ',' <> showb h  <>
      c ',' <> showb i  <>
      c ',' <> showb j  <>
      c ',' <> showb k  <>
      c ',' <> showb l  <>
      c ',' <> showb m  <>
      c ',' <> showb n  <>
      c ',' <> showb o  <>
      c ')'