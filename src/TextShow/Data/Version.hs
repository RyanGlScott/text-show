{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Version
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for 'Version'.

/Since: 2/
-}
module TextShow.Data.Version (
      showbVersionPrec
    , showbVersionConcrete
    ) where

import Data.List (intersperse)
import Data.Monoid.Compat ((<>))
import Data.Text.Lazy.Builder (Builder, fromString, singleton)
import Data.Version (Version(..))

import Prelude ()
import Prelude.Compat

import TextShow.Classes (showb, showbPrec)
import TextShow.Data.Char ()
import TextShow.Data.Integral ()
import TextShow.Data.List ()
import TextShow.TH.Internal (deriveTextShow)

-- | Convert a 'Version' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbVersionPrec :: Int -> Version -> Builder
showbVersionPrec = showbPrec
{-# INLINE showbVersionPrec #-}

-- | Provides one possible concrete representation for 'Version'.  For
-- a version with 'versionBranch' @= [1,2,3]@ and 'versionTags'
-- @= [\"tag1\",\"tag2\"]@, the output will be @1.2.3-tag1-tag2@.
--
-- /Since: 2/
showbVersionConcrete :: Version -> Builder
showbVersionConcrete (Version branch tags)
    = mconcat (intersperse (singleton '.') $ map showb branch) <>
        mconcat (map ((singleton '-' <>) . fromString) tags)
{-# INLINE showbVersionConcrete #-}

$(deriveTextShow ''Version)
