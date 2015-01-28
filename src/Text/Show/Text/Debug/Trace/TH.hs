{-# LANGUAGE TemplateHaskell #-}
{-|
Module:      Text.Show.Text.Debug.Trace.TH
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Functions that splice traces into source code which take arbitrary data types or
families as arguments (even if they are not instances of @Show@). You need to
enable the @TemplateHaskell@ language extension in order to use this module.

/Since: 0.5/
-}
module Text.Show.Text.Debug.Trace.TH (
      mkTraceShow
    , mkTraceShowId
    , mkTraceShowM
    ) where

import Language.Haskell.TH.Syntax (Name, Q, Exp)

import Text.Show.Text.Debug.Trace (traceLazy, traceMLazy)
import Text.Show.Text.TH.Internal (mkShowLazy)

-- | Generates a lambda expression which outputs the shown trace message of its first
-- argument before returning the second argument.
-- 
-- /Since: 0.5/
mkTraceShow :: Name -> Q Exp
mkTraceShow name = [| traceLazy . $(mkShowLazy name) |]

-- | Generates a lambda expression which outputs the shown trace message of its
-- argument before returning that argument.
-- 
-- /Since: 0.5/
mkTraceShowId :: Name -> Q Exp
mkTraceShowId name = [| \a -> traceLazy ($(mkShowLazy name) a) a |]

-- | Generates a lambda expression which outputs the shown trace message of its
-- argument in an arbitrary monad.
-- 
-- /Since: 0.5/
mkTraceShowM :: Name -> Q Exp
mkTraceShowM name = [| traceMLazy . $(mkShowLazy name) |]