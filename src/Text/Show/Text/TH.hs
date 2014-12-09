{-|
Module:      Text.Show.Text.TH
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Functions to mechanically derive 'Show' instances or splice
@show@-related expressions into Haskell source code. You need to enable
the @TemplateHaskell@ language extension in order to use this module.
-}
module Text.Show.Text.TH (module Text.Show.Text.TH.Internal) where

import Text.Show.Text.Instances ()
import Text.Show.Text.TH.Internal