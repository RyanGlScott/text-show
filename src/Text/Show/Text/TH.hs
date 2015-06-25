{-|
Module:      Text.Show.Text.TH
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Functions to mechanically derive 'T.Show', 'Show1', or 'Show2' instances, or to
splice @show@-related expressions into Haskell source code. You need to enable
the @TemplateHaskell@ language extension in order to use this module.

/Since: 0.3/
-}
module Text.Show.Text.TH (module Text.Show.Text.TH.Internal) where

import Text.Show.Text.Instances ()
import Text.Show.Text.TH.Internal
