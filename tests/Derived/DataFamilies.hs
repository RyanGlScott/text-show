{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric              #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds                  #-}
#endif

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
-- Starting with GHC 7.10, NullaryTypeClasses was deprecated in favor of
-- MultiParamTypeClasses, which is already enabled
{-# LANGUAGE NullaryTypeClasses         #-}
#endif

{-|
Module:      Derived.DataFamilies
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines corner case-provoking data families.
-}
module Derived.DataFamilies (
      NotAllShow(..)
#if __GLASGOW_HASKELL__ >= 706
    , KindDistinguished(..)
#endif
#if __GLASGOW_HASKELL__ >= 708
    , NullaryClass(..)
    , NullaryData(..)
#endif
    ) where

#include "generic.h"

#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
import qualified Generics.Deriving.TH as Generics
#endif

#if __GLASGOW_HASKELL__ >= 706
import           GHC.Generics (Generic)
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
import           GHC.Generics (Generic1)
# endif
#endif
import           GHC.Show (appPrec, appPrec1, showSpace)

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..), oneof)

#if MIN_VERSION_template_haskell(2,7,0)
import           TextShow.TH (deriveTextShow, deriveTextShow1, deriveTextShow2)
#endif

import TransformersCompat (Show1(..), Show2(..))

-------------------------------------------------------------------------------

data family NotAllShow (w :: *) (x :: *) (y :: *) (z :: *) :: *

data instance NotAllShow ()  ()  () d = NASNoShow
data instance NotAllShow Int b   c  d = NASShow1 c b
                                      | NASShow2 d
  deriving ( Show
#if __GLASGOW_HASKELL__ >= 706
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
#endif
           )

instance (Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (NotAllShow Int b c d) where
    arbitrary = oneof [ NASShow1 <$> arbitrary <*> arbitrary
                      , NASShow2 <$> arbitrary
                      ]

instance (Show b, Show c) => Show1 (NotAllShow Int b c) where
    showsPrecWith = showsPrecWith2 showsPrec
instance Show b => Show2 (NotAllShow Int b) where
    showsPrecWith2 sp1 _ p (NASShow1 c b) = showParen (p > appPrec) $
          showString "NASShow1 "
        . sp1 appPrec1 c . showSpace
        . showsPrec appPrec1 b
    showsPrecWith2 _ sp2 p (NASShow2 d) = showParen (p > appPrec) $
          showString "NASShow2 "
        . sp2 appPrec1 d

#if MIN_VERSION_template_haskell(2,7,0)
$(deriveTextShow  'NASShow1)
$(deriveTextShow1 'NASShow2)
$(deriveTextShow2 'NASShow1)

# if !defined(__LANGUAGE_DERIVE_GENERIC1__)
$(Generics.deriveMeta           'NASShow1)
$(Generics.deriveRepresentable1 'NASShow2)
# endif

# if __GLASGOW_HASKELL__ < 706
$(Generics.deriveRepresentable0 'NASShow1)
# endif
#endif

-------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 706
data family KindDistinguished (x :: k) (y :: *) (z :: *) :: *

newtype instance KindDistinguished (a :: *) b c = KindDistinguished1 b
  deriving ( Arbitrary
           , Show
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
           )

newtype instance KindDistinguished (a :: * -> *) b c = KindDistinguished2 b
  deriving ( Arbitrary
           , Show
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
           )

newtype instance KindDistinguished Either b c = KindDistinguished3 b
  deriving ( Arbitrary
           , Show
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
           )

instance Show b => Show1 (KindDistinguished (a :: *) b) where
    showsPrecWith = showsPrecWith2 showsPrec
instance Show2 (KindDistinguished (a :: *)) where
    showsPrecWith2 sp1 _ p (KindDistinguished1 b) = showParen (p > appPrec) $
          showString "KindDistinguished1 "
        . sp1 appPrec1 b

instance Show b => Show1 (KindDistinguished (a :: * -> *) b) where
    showsPrecWith = showsPrecWith2 showsPrec
instance Show2 (KindDistinguished (a :: * -> *)) where
    showsPrecWith2 sp1 _ p (KindDistinguished2 b) = showParen (p > appPrec) $
          showString "KindDistinguished2 "
        . sp1 appPrec1 b

instance Show b => Show1 (KindDistinguished (Either :: * -> * -> *) b) where
    showsPrecWith = showsPrecWith2 showsPrec
instance Show2 (KindDistinguished (Either :: * -> * -> *)) where
    showsPrecWith2 sp1 _ p (KindDistinguished3 b) = showParen (p > appPrec) $
          showString "KindDistinguished3 "
        . sp1 appPrec1 b

$(deriveTextShow  'KindDistinguished1)
$(deriveTextShow1 'KindDistinguished1)
$(deriveTextShow2 'KindDistinguished1)

$(deriveTextShow  'KindDistinguished2)
$(deriveTextShow1 'KindDistinguished2)
$(deriveTextShow2 'KindDistinguished2)

$(deriveTextShow  'KindDistinguished3)
$(deriveTextShow1 'KindDistinguished3)
$(deriveTextShow2 'KindDistinguished3)

# if !defined(__LANGUAGE_DERIVE_GENERIC1__)
$(Generics.deriveAll1 'KindDistinguished1)
$(Generics.deriveAll1 'KindDistinguished2)
$(Generics.deriveAll1 'KindDistinguished3)
# endif
#endif

-------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 708
class NullaryClass where
    data NullaryData :: *

instance NullaryClass where
    newtype NullaryData = NullaryCon Int
      deriving (Arbitrary, Show, Generic)

$(deriveTextShow 'NullaryCon)

# if __GLASGOW_HASKELL__ < 706
$(Generics.deriveAll 'NullaryCon)
# endif
#endif
