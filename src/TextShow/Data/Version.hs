{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.Data.Version
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides a 'TextShow' instance for 'Version' and the 'showbVersion' function.

/Since: 2/
-}
module TextShow.Data.Version (showbVersion) where

import Data.List (intersperse)
import Data.Text.Builder.Linear (Builder, fromChar)
import Data.Version (Version(..))

import Prelude ()
import Prelude.Compat

import TextShow.Classes (TextShow(..))
import TextShow.Data.Char ()
import TextShow.Data.Integral ()
import TextShow.Data.List ()
import TextShow.TH.Internal (deriveTextShow)
import TextShow.Utils (fromString)

-- | Provides one possible concrete representation for 'Version'.  For
-- a version with 'versionBranch' @= [1,2,3]@ and 'versionTags'
-- @= [\"tag1\",\"tag2\"]@, the output will be @1.2.3-tag1-tag2@.
--
-- /Since: 3.6/
showbVersion :: Version -> Builder
showbVersion (Version branch tags)
    = mconcat (intersperse (fromChar '.') $ map showb branch) <>
        mconcat (map ((fromChar '-' <>) . fromString) tags)
{-# INLINE showbVersion #-}

-- | /Since: 2/
$(deriveTextShow ''Version)
