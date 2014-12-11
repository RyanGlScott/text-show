{-# LANGUAGE CPP #-}
{-|
Module:      Text.Show.Text.GHC
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Imports 'Show' instances for @GHC@ modules.
-}
module Text.Show.Text.GHC () where 

#if MIN_VERSION_base(4,4,0)
import Text.Show.Text.GHC.Event       ()
import Text.Show.Text.GHC.Fingerprint ()
import Text.Show.Text.GHC.Generics    ()
#endif
#if MIN_VERSION_base(4,5,0)
import Text.Show.Text.GHC.Stats       ()
#endif