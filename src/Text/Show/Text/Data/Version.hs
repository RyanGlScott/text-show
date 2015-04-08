{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Version
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for 'Version'.

/Since: 0.3/
-}
module Text.Show.Text.Data.Version (
      showbVersionPrec
    , showbVersionConcrete
    ) where

import Data.List (intersperse)
import Data.Monoid.Compat ((<>))
import Data.Text.Lazy.Builder (Builder, fromString)
import Data.Version (Version(..))

import Prelude ()
import Prelude.Compat

import Text.Show.Text.Classes (showb, showbPrec)
import Text.Show.Text.Data.Char ()
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.Data.List ()
import Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowbPrec)
import Text.Show.Text.Utils (s)

-- | Convert a 'Version' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbVersionPrec :: Int -> Version -> Builder
showbVersionPrec = showbPrec
{-# INLINE showbVersionPrec #-}

-- | Provides one possible concrete representation for 'Version'.  For
-- a version with 'versionBranch' @= [1,2,3]@ and 'versionTags' 
-- @= [\"tag1\",\"tag2\"]@, the output will be @1.2.3-tag1-tag2@.
-- 
-- /Since: 0.3/
showbVersionConcrete :: Version -> Builder
showbVersionConcrete (Version branch tags)
    = mconcat (intersperse (s '.') $ map showb branch) <>
        mconcat (map ((s '-' <>) . fromString) tags)
{-# INLINE showbVersionConcrete #-}

$(deriveShowPragmas defaultInlineShowbPrec ''Version)
