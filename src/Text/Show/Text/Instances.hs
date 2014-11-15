{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Show.Text.Instances where

import           Data.Array (Array, assocs, bounds)
import           Data.Complex (Complex(..))
import           Data.Ix (Ix)
import           Data.Monoid ((<>))
import           Data.Ratio (Ratio, numerator, denominator)
import qualified Data.Text as T (Text, unpack)
import qualified Data.Text.Lazy as TL (Text, unpack)
import           Data.Text.Buildable (build)
import           Data.Text.Lazy.Builder (Builder, toLazyText)

import           GHC.Real (ratioPrec, ratioPrec1)
import           GHC.Show (appPrec, appPrec1)

import           Prelude hiding (Show(show), print, putStrLn)

import           Text.Show.Text.Char ()
import           Text.Show.Text.Class (Show(..), showbParen)
import           Text.Show.Text.Float ()
import           Text.Show.Text.Functions (s)
import           Text.Show.Text.Int ()
import           Text.Show.Text.Tuple ()

instance Show a => Show [a] where
    {-# SPECIALIZE instance Show [String] #-}
    {-# SPECIALIZE instance Show [Char]   #-}
    {-# SPECIALIZE instance Show [Int]    #-}
    showb = showbList
    {-# INLINE showb #-}

instance Show () where
    showb () = "()"
    {-# INLINE showb #-}

instance Show Bool where
    showb = build
    {-# INLINE showb #-}

instance (Show a, Integral a) => Show (Ratio a) where
    {-# SPECIALIZE instance Show Rational #-}
    showbPrec p q = showbParen (p > ratioPrec) $
                       showbPrec ratioPrec1 (numerator q)
                    <> " % "
                    <> showbPrec ratioPrec1 (denominator q)
    {-# INLINE showbPrec #-}

instance (Show a, RealFloat a) => Show (Complex a) where
    {-# SPECIALIZE instance Show (Complex Float) #-}
    {-# SPECIALIZE instance Show (Complex Double) #-}
    showbPrec p (a :+ b) = showbParen (p > complexPrec) $
                              showbPrec (complexPrec+1) a
                           <> " :+ "
                           <> showbPrec (complexPrec+1) b
      where complexPrec = 6
    {-# INLINE showbPrec #-}

instance Show Builder where
    showbPrec p = showbPrec p . toLazyText
    {-# INLINE showbPrec #-}

-- Strict variant
instance Show T.Text where
    showbPrec p = showbPrec p . T.unpack
    {-# INLINE showbPrec #-}

-- Lazy variant
instance Show TL.Text where
    showbPrec p = showbPrec p . TL.unpack
    {-# INLINE showbPrec #-}

instance Show a => Show (Maybe a) where
    showbPrec _ Nothing  = "Nothing"
    showbPrec p (Just a) = showbParen (p > appPrec) $ "Just " <> showbPrec appPrec1 a
    {-# INLINE showbPrec #-}

instance (Show a, Show b) => Show (Either a b) where
    showbPrec p (Left a)  = showbParen (p > appPrec) $ "Left "  <> showbPrec appPrec1 a
    showbPrec p (Right b) = showbParen (p > appPrec) $ "Right " <> showbPrec appPrec1 b
    {-# INLINE showbPrec #-}

instance Show Ordering where
    showb LT = "LT"
    showb EQ = "EQ"
    showb GT = "GT"
    {-# INLINE showb #-}

instance (Show i, Show e, Ix i) => Show (Array i e) where
    showbPrec p a = showbParen (p > appPrec) $
                       "array "
                    <> showbPrec appPrec1 (bounds a)
                    <> s ' '
                    <> showbPrec appPrec1 (assocs a)
    {-# INLINE showbPrec #-}