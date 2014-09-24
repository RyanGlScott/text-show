module Text.Show.Text.Util (
      c
    ) where

import Data.Text.Lazy.Builder

c :: Char -> Builder
c = singleton