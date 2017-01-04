{-# LANGUAGE TemplateHaskell #-}
{-|
Module:      TextShow.Debug.Trace.TH
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Functions that splice traces into source code which take an arbitrary data type or
data family instance as an argument (even if it is not an instance of @TextShow@). You
need to enable the @TemplateHaskell@ language extension in order to use this module.

/Since: 2/
-}
module TextShow.Debug.Trace.TH (
      makeTraceTextShow
    , makeTraceTextShowId
    , makeTraceTextShowM
    ) where

import Language.Haskell.TH.Syntax (Name, Q, Exp)

import TextShow.Debug.Trace
import TextShow.TH.Internal (makeShowt)

-- | Generates a lambda expression which behaves like 'traceTextShow' (without
-- requiring a @TextShow@ instance).
--
-- /Since: 2/
makeTraceTextShow :: Name -> Q Exp
makeTraceTextShow name = [| tracet . $(makeShowt name) |]

-- | Generates a lambda expression which behaves like 'traceTextShowId' (without
-- requiring a @TextShow@ instance).
--
-- /Since: 2/
makeTraceTextShowId :: Name -> Q Exp
makeTraceTextShowId name = [| \a -> tracet ($(makeShowt name) a) a |]

-- | Generates a lambda expression which behaves like 'traceTextShowM' (without
-- requiring a @TextShow@ instance).
--
-- /Since: 2/
makeTraceTextShowM :: Name -> Q Exp
makeTraceTextShowM name = [| tracetM . $(makeShowt name) |]
