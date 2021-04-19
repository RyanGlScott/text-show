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

import Text.Read.Lex (Lexeme, Number)

import TextShow.Data.Char     ()
import TextShow.Data.Integral ()
import TextShow.Data.List     ()
import TextShow.Data.Maybe    ()
import TextShow.Data.Ratio    ()
import TextShow.TH.Internal (deriveTextShow)

-- | /Since: 2/
$(deriveTextShow ''Number)

-- | /Since: 2/
$(deriveTextShow ''Lexeme)
