{-# LANGUAGE CPP #-}
{-|
Module:      Text.Show.Text.GHC
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Imports 'Show' instances for @GHC@ modules.
-}
module Text.Show.Text.GHC () where 

#if defined(mingw32_HOST_OS)
import Text.Show.Text.GHC.Conc.Windows ()
#endif
#if MIN_VERSION_base(4,4,0)
# if !defined(mingw32_HOST_OS)
import Text.Show.Text.GHC.Event        ()
# endif
import Text.Show.Text.GHC.Fingerprint  ()
import Text.Show.Text.GHC.Generics     ()
#endif
#if MIN_VERSION_base(4,8,0)
import Text.Show.Text.GHC.RTS.Flags    ()
import Text.Show.Text.GHC.StaticPtr    ()
#endif
#if MIN_VERSION_base(4,5,0)
import Text.Show.Text.GHC.Stats        ()
#endif
#if MIN_VERSION_base(4,6,0)
import Text.Show.Text.GHC.TypeLits     ()
#endif
