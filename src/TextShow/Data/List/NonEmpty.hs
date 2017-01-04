{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.Data.List.NonEmpty
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'NonEmpty' lists.

/Since: 3/
-}
module TextShow.Data.List.NonEmpty (liftShowbNonEmptyPrec) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text.Lazy.Builder (Builder)

import TextShow.Classes (TextShow1(..))
import TextShow.Data.List ()
import TextShow.TH.Internal (deriveTextShow, deriveTextShow1)

-- | Convert a 'NonEmpty' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 3/
liftShowbNonEmptyPrec :: (Int -> a -> Builder) -> ([a] -> Builder)
                      -> Int -> NonEmpty a -> Builder
liftShowbNonEmptyPrec = liftShowbPrec

$(deriveTextShow  ''NonEmpty)
$(deriveTextShow1 ''NonEmpty)
