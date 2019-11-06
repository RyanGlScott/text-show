{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.GHC.Generics
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for generics-related data types.

/Since: 2/
-}
module TextShow.GHC.Generics () where

import Generics.Deriving.Base

import TextShow.Classes (TextShow(..), TextShow1(..), TextShow2(..))
import TextShow.Data.Char     ()
import TextShow.Data.Floating ()
import TextShow.Data.Integral ()
import TextShow.TH.Internal (deriveTextShow, deriveTextShow1, makeShowbPrec,
                             makeLiftShowbPrec, makeLiftShowbPrec2)

-- | /Since: 2/
$(deriveTextShow1 ''U1)
-- | /Since: 2/
instance TextShow (U1 p) where
    showbPrec = liftShowbPrec undefined undefined

-- | /Since: 2/
$(deriveTextShow  ''Par1)
-- | /Since: 2/
$(deriveTextShow1 ''Par1)

-- | /Since: 2/
instance TextShow (f p) => TextShow (Rec1 f p) where
    showbPrec = $(makeShowbPrec ''Rec1)
-- | /Since: 2/
$(deriveTextShow1 ''Rec1)

-- | /Since: 2/
instance TextShow c => TextShow (K1 i c p) where
    showbPrec = liftShowbPrec undefined undefined
-- | /Since: 2/
instance TextShow c => TextShow1 (K1 i c) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList
-- | /Since: 2/
instance TextShow2 (K1 i) where
    liftShowbPrec2 = $(makeLiftShowbPrec2 ''K1)

-- | /Since: 2/
instance TextShow (f p) => TextShow (M1 i c f p) where
    showbPrec = $(makeShowbPrec ''M1)
-- | /Since: 2/
instance TextShow1 f => TextShow1 (M1 i c f) where
    liftShowbPrec = $(makeLiftShowbPrec ''M1)

-- | /Since: 2/
instance (TextShow (f p), TextShow (g p)) => TextShow ((f :+: g) p) where
    showbPrec = $(makeShowbPrec ''(:+:))
-- | /Since: 2/
$(deriveTextShow1 ''(:+:))

-- | /Since: 2/
instance (TextShow (f p), TextShow (g p)) => TextShow ((f :*: g) p) where
    showbPrec = $(makeShowbPrec ''(:*:))
-- | /Since: 2/
$(deriveTextShow1 ''(:*:))

-- | /Since: 2/
instance TextShow (f (g p)) => TextShow ((f :.: g) p) where
    showbPrec = $(makeShowbPrec ''(:.:))
-- | /Since: 2/
$(deriveTextShow1 ''(:.:))

-- | /Since: 2.1.2/
instance TextShow (UChar p) where
    showbPrec = $(makeShowbPrec 'UChar)
-- | /Since: 2.1.2/
$(deriveTextShow1 'UChar)

-- | /Since: 2.1.2/
instance TextShow (UDouble p) where
    showbPrec = $(makeShowbPrec 'UDouble)
-- | /Since: 2.1.2/
$(deriveTextShow1 'UDouble)

-- | /Since: 2.1.2/
instance TextShow (UFloat p) where
    showbPrec = $(makeShowbPrec 'UFloat)
-- | /Since: 2.1.2/
$(deriveTextShow1 'UFloat)

-- | /Since: 2.1.2/
instance TextShow (UInt p) where
    showbPrec = $(makeShowbPrec 'UInt)
-- | /Since: 2.1.2/
$(deriveTextShow1 'UInt)

-- | /Since: 2.1.2/
instance TextShow (UWord p) where
    showbPrec = $(makeShowbPrec 'UWord)
-- | /Since: 2.1.2/
$(deriveTextShow1 'UWord)

-- | /Since: 2/
$(deriveTextShow ''Associativity)
-- | /Since: 2/
$(deriveTextShow ''Fixity)
#if MIN_VERSION_base(4,9,0)
-- | Only available with @base-4.9.0.0@ or later.
--
-- /Since: 3/
$(deriveTextShow ''SourceUnpackedness)
-- | Only available with @base-4.9.0.0@ or later.
--
-- /Since: 3/
$(deriveTextShow ''SourceStrictness)
-- | Only available with @base-4.9.0.0@ or later.
--
-- /Since: 3/
$(deriveTextShow ''DecidedStrictness)
#else
-- | Only available with @base-4.8@ or earlier.
--
-- /Since: 2/
$(deriveTextShow ''Arity)
#endif
