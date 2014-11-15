{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Show.Text.Float where

import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder.RealFloat (realFloat)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showbPrec), showbParen)
import Text.Show.Text.Functions (s)

showbSignedFloatPrec :: RealFloat a
  => (a -> Builder) -- ^ a function that can show unsigned values
  -> Int            -- ^ the precedence of the enclosing context
  -> a              -- ^ the value to show
  -> Builder
showbSignedFloatPrec showbPos p x
    | x < 0 || isNegativeZero x = showbParen (p > 6) $ s '-' <> showbPos (-x)
    | otherwise                 = showbPos x
{-# INLINE showbSignedFloatPrec #-}

instance Show Float where
    showbPrec p = showbSignedFloatPrec realFloat p
    {-# INLINE showbPrec #-}

instance Show Double where
    showbPrec p = showbSignedFloatPrec realFloat p
    {-# INLINE showbPrec #-}