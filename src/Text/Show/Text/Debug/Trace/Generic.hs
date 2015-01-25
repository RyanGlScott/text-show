{-# LANGUAGE FlexibleContexts #-}
{-|
Module:      Text.Show.Text.Debug.Trace.Generic
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Functions that trace the values of 'Generic' instances (even if they are not
instances of @Show@).

/Since: 0.6/
-}
module Text.Show.Text.Debug.Trace.Generic (
      genericTraceShow
    , genericTraceShowId
    , genericTraceShowM
    ) where

import GHC.Generics (Generic, Rep)

import Text.Show.Text.Debug.Trace (traceLazy, traceMLazy)
import Text.Show.Text.Generic (GShow, genericShowLazy)

-- | Outputs the shown trace message of its first argument (a 'Generic' instance)
-- before returning the second argument.
-- 
-- /Since: 0.6/
genericTraceShow :: (Generic a, GShow (Rep a)) => a -> b -> b
genericTraceShow = traceLazy . genericShowLazy

-- | Outputs the shown trace message of its argument (a 'Generic' instance) before
-- returning that argument.
-- 
-- /Since: 0.6/
genericTraceShowId :: (Generic a, GShow (Rep a)) => a -> a
genericTraceShowId a = traceLazy (genericShowLazy a) a

-- | Outputs the shown trace message of its argument (a 'Generic' instance) in an
-- arbitrary monad.
-- 
-- /Since: 0.6/
genericTraceShowM :: (Generic a, GShow (Rep a), Monad m) => a -> m ()
genericTraceShowM = traceMLazy . genericShowLazy