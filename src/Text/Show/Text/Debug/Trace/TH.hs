{-# LANGUAGE TemplateHaskell #-}
{-|
Module:      Text.Show.Text.Debug.Trace.TH
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Functions that splice traces into source code which take an arbitrary data type or
data family instance as an argument (even if it is not an instance of @Show@). You
need to enable the @TemplateHaskell@ language extension in order to use this module.

/Since: 0.5/
-}
module Text.Show.Text.Debug.Trace.TH (
      mkTraceShow
    , mkTraceShowId
    , mkTraceShowM
    ) where

import Language.Haskell.TH.Syntax (Name, Q, Exp)

import Text.Show.Text.Debug.Trace
import Text.Show.Text.TH.Internal (mkShow)

-- | Generates a lambda expression which behaves like 'traceShow' (without requiring a
-- @Show@ instance).
-- 
-- /Since: 0.5/
mkTraceShow :: Name -> Q Exp
mkTraceShow name = [| trace . $(mkShow name) |]

-- | Generates a lambda expression which behaves like 'traceShowId' (without requiring a
-- @Show@ instance).
-- 
-- /Since: 0.5/
mkTraceShowId :: Name -> Q Exp
mkTraceShowId name = [| \a -> trace ($(mkShow name) a) a |]

-- | Generates a lambda expression which behaves like 'traceShowM' (without requiring a
-- @Show@ instance).
-- 
-- /Since: 0.5/
mkTraceShowM :: Name -> Q Exp
mkTraceShowM name = [| traceM . $(mkShow name) |]
