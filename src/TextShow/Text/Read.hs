{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Text.Read
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'Lexeme' (and 'Number', if using a
recent-enough version of @base@).

/Since: 2/
-}
module TextShow.Text.Read () where

import Text.Read.Lex (Lexeme)

import TextShow.Data.Char     ()
import TextShow.Data.Integral ()
import TextShow.Data.List     ()
import TextShow.Data.Maybe    ()
import TextShow.Data.Ratio    ()
import TextShow.TH.Internal (deriveTextShow)
#if MIN_VERSION_base(4,6,0)
import TextShow.TH.Names (numberTypeName)
#endif

#if MIN_VERSION_base(4,6,0)
-- | Only available with @base-4.6.0.0@ or later.
--
-- /Since: 2/
$(deriveTextShow numberTypeName)
#endif

-- | /Since: 2/
$(deriveTextShow ''Lexeme)
