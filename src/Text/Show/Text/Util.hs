module Text.Show.Text.Util (s) where

import Data.Text.Lazy.Builder (Builder, singleton)

-- | A shorter name for 'singleton' for convenience's sake (since it tends to be used
--   pretty often in this package).
s :: Char -> Builder
s = singleton