{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Ratio
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Ratio' values.
-}
module Text.Show.Text.Data.Complex (showbComplexPrec) where

import Data.Complex (Complex)
import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showbPrec), Show1(showbPrec1))
import Text.Show.Text.Data.Floating ()
import Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowbPrec,
                                   specializeTypes)

-- | Convert a 'Complex' value to a 'Builder' with the given precedence.
showbComplexPrec :: Show a => Int -> Complex a -> Builder
showbComplexPrec = showbPrec
{-# INLINE showbComplexPrec #-}

$(deriveShowPragmas defaultInlineShowbPrec {
                        specializeTypes = [ [t| Complex Float  |]
                                          , [t| Complex Double |]
                                          ]
                    }
                    ''Complex)

instance Show1 Complex where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}