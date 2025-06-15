{-# LANGUAGE CPP             #-}

#if !defined(__GHCJS__) && !defined(ghcjs_HOST_OS) && defined(mingw32_HOST_OS)
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#endif
{-|
Module:      TextShow.GHC.Conc.Windows
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'ConsoleEvent'.
Only provided if using Windows, and not using GHCJS.

/Since: 2/
-}
module TextShow.GHC.Conc.Windows () where

#if !defined(__GHCJS__) && !defined(ghcjs_HOST_OS) && defined(mingw32_HOST_OS)
import GHC.Conc.Windows (ConsoleEvent)
import TextShow.TH.Internal (deriveTextShow)

-- | /Since: 2/
$(deriveTextShow ''ConsoleEvent)
#endif
