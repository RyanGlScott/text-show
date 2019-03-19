{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
#if MIN_VERSION_text(0,9,0)
{-# LANGUAGE TemplateHaskell   #-}
#endif
{-# OPTIONS_GHC -fno-warn-deprecations #-} -- TODO: Remove this later
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Text
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines 'TextShow' instances for 'Text' types, as well as other miscellaneous
data types from the @text@ package.

Note that this module deliberately does not define a 'TextShow' instance for
the @I16@ data type from @Data.Text.Foreign@, as that module is not available
on certain widely used variants of GHC (e.g., @reflex-platform@). See #40
for more details. If this is a problem for you, please file an issue.

/Since: 2/
-}
module TextShow.Data.Text () where

import qualified Data.Text as TS
import           Data.Text.Encoding.Error (UnicodeException(..))
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (Builder, fromString, toLazyText)

import           Prelude ()
import           Prelude.Compat

import           TextShow.Classes (TextShow(..))
import           TextShow.Data.Char (showbString)
import           TextShow.Data.Integral (showbHex)
import           TextShow.TH.Internal (deriveTextShow)

#if MIN_VERSION_text(1,0,0)
import           Data.Text.Encoding (Decoding(..))
import           Data.Text.Lazy.Builder (singleton)
import           GHC.Show (appPrec)
import           TextShow.Classes (showbParen)
import           TextShow.Data.ByteString ()
#endif

#if MIN_VERSION_text(1,1,0)
import           Data.Text.Internal.Fusion.Size (Size)
#endif

-- | /Since: 2/
instance TextShow TS.Text where
    showb = showbString . TS.unpack
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow TL.Text where
    showb = showbString . TL.unpack
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow Builder where
    showb = showb . toLazyText
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow UnicodeException where
    showb (DecodeError desc (Just w))
        = "Cannot decode byte '\\x" <> showbHex w <> "': " <> fromString desc
    showb (DecodeError desc Nothing)
        = "Cannot decode input: " <> fromString desc
    showb (EncodeError desc (Just c))
        = "Cannot encode character '\\x" <> showbHex (fromEnum c) <> "': " <> fromString desc
    showb (EncodeError desc Nothing)
        = "Cannot encode input: " <> fromString desc

#if MIN_VERSION_text(1,0,0)
-- | Only available with @text-1.0.0.0@ or later.
--
-- /Since: 2/
instance TextShow Decoding where
    showbPrec p (Some t bs _) = showbParen (p > appPrec) $
        "Some " <> showb t <>
        singleton ' ' <> showb bs <>
        " _"
    {-# INLINE showbPrec #-}
#endif

#if MIN_VERSION_text(1,1,0)
-- | Only available with @text-1.1.0.0@ or later.
--
-- /Since: 2/
$(deriveTextShow ''Size)
#endif
