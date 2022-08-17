{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE DeriveLift           #-}
#endif

{-|
Module:      TextShow.Generic
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Generic versions of 'TextShow' and 'TextShow1' class functions, as an alternative to
"TextShow.TH", which uses Template Haskell. Because there is no 'Generic2'
class, 'TextShow2' cannot be implemented generically.

This implementation is loosely based off of the @Generics.Deriving.Show@ module
from the @generic-deriving@ library.

/Since: 2/
-}
module TextShow.Generic (
      -- * Generic adapter newtypes
      FromGeneric(..)
    , FromGeneric1(..)

      -- * Generic @show@ functions
      -- $generics

      -- ** Understanding a compiler error
    , genericShowt
    , genericShowtl
    , genericShowtPrec
    , genericShowtlPrec
    , genericShowtList
    , genericShowtlList
    , genericShowb
    , genericShowbPrec
    , genericShowbList
    , genericPrintT
    , genericPrintTL
    , genericHPrintT
    , genericHPrintTL
    , genericLiftShowbPrec
    , genericShowbPrec1
      -- * Internals
    , GTextShow(..)
    , GTextShowCon(..)
    , GTextShow1(..)
    , GTextShowCon1(..)
    , IsNullary(..)
    , ConType(..)
    , TextLike(..)
    ) where

import           Data.Data (Data, Typeable)
import           Data.String (IsString(..))
import qualified Data.Text    as TS (Text, singleton)
import qualified Data.Text.IO as TS (putStrLn, hPutStrLn)
import qualified Data.Text.Lazy    as TL (Text, singleton)
import qualified Data.Text.Lazy.IO as TL (putStrLn, hPutStrLn)
import qualified Data.Text.Lazy.Builder as TB (singleton)
import           Data.Text.Lazy.Builder (Builder)

import           Generics.Deriving.Base
#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
import qualified Generics.Deriving.TH as Generics
#endif

import           GHC.Exts (Char(C#), Double(D#), Float(F#), Int(I#), Word(W#))
import           GHC.Show (appPrec, appPrec1)

import           Language.Haskell.TH.Lift

import           Prelude ()
import           Prelude.Compat

import           System.IO (Handle)

import           TextShow.Classes (TextShow(..), TextShow1(..),
                                   showbListWith,  showbParen,  showbSpace,
                                   showtListWith,  showtParen,  showtSpace,
                                   showtlListWith, showtlParen, showtlSpace,
                                   showbPrec1, liftShowtPrec, liftShowtlPrec)
import           TextShow.Instances ()
import           TextShow.TH.Internal (deriveTextShow)
import           TextShow.Utils (isInfixDataCon, isSymVar, isTupleString)

{- $generics

'TextShow' instances can be easily defined for data types that are 'Generic' instances.
If you are using GHC 8.6 or later, the easiest way to do this is to use the
@DerivingVia@ extension.

@
&#123;-&#35; LANGUAGE DeriveGeneric, DerivingVia &#35;-&#125;
import GHC.Generics
import TextShow
import TextShow.Generic

data D a = D a
  deriving ('Generic', 'Generic1')
  deriving 'TextShow'  via 'FromGeneric'  (D a)
  deriving 'TextShow1' via 'FromGeneric1' D
@

Or, if you are using a version of GHC older than 8.6, one can alternatively
define these instances like so:

@
instance 'TextShow' a => 'TextShow' (D a) where
    'showbPrec' = 'genericShowbPrec'

instance 'TextShow1' D where
    'liftShowbPrec' = 'genericLiftShowbPrec'
@
-}

-- | An adapter newtype, suitable for @DerivingVia@.
-- The 'TextShow' instance for 'FromGeneric' leverages a 'Generic'-based
-- default. That is,
--
-- @
-- 'showbPrec' p ('FromGeneric' x) = 'genericShowbPrec' p x
-- @
--
-- /Since: 3.7.4/
newtype FromGeneric a = FromGeneric { fromGeneric :: a }
  deriving ( Data
           , Eq
           , Foldable
           , Functor
           , Generic
           , Generic1
           , Ord
           , Read
           , Show
           , Traversable
           , Typeable
#if __GLASGOW_HASKELL__ >= 800
           , Lift
#endif
           )

-- | /Since: 3.7.4/
instance (Generic a, GTextShow Builder (Rep a ())) => TextShow (FromGeneric a) where
  showbPrec p = genericShowbPrec p . fromGeneric

-- | An adapter newtype, suitable for @DerivingVia@.
-- The 'TextShow1' instance for 'FromGeneric1' leverages a 'Generic1'-based
-- default. That is,
--
-- @
-- 'liftShowbPrec' sp sl p ('FromGeneric1' x) = 'genericLiftShowbPrec' sp sl p x
-- @
--
-- /Since: 3.7.4/
newtype FromGeneric1 f a = FromGeneric1 { fromGeneric1 :: f a }
  deriving ( Eq
           , Ord
           , Read
           , Show
           , Generic
#if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
#endif
#if __GLASGOW_HASKELL__ >= 800
           , Lift
#endif
           )

deriving instance Foldable    f => Foldable    (FromGeneric1 f)
deriving instance Functor     f => Functor     (FromGeneric1 f)
deriving instance Traversable f => Traversable (FromGeneric1 f)
deriving instance Typeable FromGeneric1
deriving instance ( Data (f a), Typeable f, Typeable a
                  ) => Data (FromGeneric1 f (a :: *))

-- | /Since: 3.7.4/
instance (Generic1 f, GTextShow1 Builder (Rep1 f)) => TextShow1 (FromGeneric1 f) where
  liftShowbPrec sp sl p = genericLiftShowbPrec sp sl p . fromGeneric1

-- | A 'Generic' implementation of 'showt'.
--
-- /Since: 2/
genericShowt :: (Generic a, GTextShow TS.Text (Rep a ())) => a -> TS.Text
genericShowt = genericShowtPrec 0

-- | A 'Generic' implementation of 'showtl'.
--
-- /Since: 2/
genericShowtl :: (Generic a, GTextShow TL.Text (Rep a ())) => a -> TL.Text
genericShowtl = genericShowtlPrec 0

-- | A 'Generic' implementation of 'showPrect'.
--
-- /Since: 2/
genericShowtPrec :: (Generic a, GTextShow TS.Text (Rep a ())) => Int -> a -> TS.Text
genericShowtPrec p = gShowPrec p . fromRepUnit

-- | A 'Generic' implementation of 'showtlPrec'.
--
-- /Since: 2/
genericShowtlPrec :: (Generic a, GTextShow TL.Text (Rep a ())) => Int -> a -> TL.Text
genericShowtlPrec p = gShowPrec p . fromRepUnit

-- | A 'Generic' implementation of 'showtList'.
--
-- /Since: 2/
genericShowtList :: (Generic a, GTextShow TS.Text (Rep a ())) => [a] -> TS.Text
genericShowtList = showtListWith genericShowt

-- | A 'Generic' implementation of 'showtlList'.
--
-- /Since: 2/
genericShowtlList :: (Generic a, GTextShow TL.Text (Rep a ())) => [a] -> TL.Text
genericShowtlList = showtlListWith genericShowtl

-- | A 'Generic' implementation of 'showb'.
--
-- /Since: 2/
genericShowb :: (Generic a, GTextShow Builder (Rep a ())) => a -> Builder
genericShowb = genericShowbPrec 0

-- | A 'Generic' implementation of 'showbPrec'.
--
-- /Since: 2/
genericShowbPrec :: (Generic a, GTextShow Builder (Rep a ())) => Int -> a -> Builder
genericShowbPrec p = gShowPrec p . fromRepUnit

-- | A 'Generic' implementation of 'showbList'.
--
-- /Since: 2/
genericShowbList :: (Generic a, GTextShow Builder (Rep a ())) => [a] -> Builder
genericShowbList = showbListWith genericShowb

-- | A 'Generic' implementation of 'printT'.
--
-- /Since: 2/
genericPrintT :: (Generic a, GTextShow TS.Text (Rep a ())) => a -> IO ()
genericPrintT = TS.putStrLn . genericShowt

-- | A 'Generic' implementation of 'printTL'.
--
-- /Since: 2/
genericPrintTL :: (Generic a, GTextShow TL.Text (Rep a ())) => a -> IO ()
genericPrintTL = TL.putStrLn . genericShowtl

-- | A 'Generic' implementation of 'hPrintT'.
--
-- /Since: 2/
genericHPrintT :: (Generic a, GTextShow TS.Text (Rep a ())) => Handle -> a -> IO ()
genericHPrintT h = TS.hPutStrLn h . genericShowt

-- | A 'Generic' implementation of 'hPrintTL'.
--
-- /Since: 2/
genericHPrintTL :: (Generic a, GTextShow TL.Text (Rep a ())) => Handle -> a -> IO ()
genericHPrintTL h = TL.hPutStrLn h . genericShowtl

-- | A 'Generic1' implementation of 'genericLiftShowbPrec'.
--
-- /Since: 2/
genericLiftShowbPrec :: (Generic1 f, GTextShow1 Builder (Rep1 f))
                     => (Int -> a -> Builder) -> ([a] -> Builder)
                     -> Int -> f a -> Builder
genericLiftShowbPrec sp sl p = gLiftShowPrec sp sl p . from1

-- | A 'Generic'/'Generic1' implementation of 'showbPrec1'.
--
-- /Since: 2/
genericShowbPrec1 :: ( Generic a, Generic1 f
                     , GTextShow Builder (Rep a ())
                     , GTextShow1 Builder (Rep1 f)
                     )
                  => Int -> f a -> Builder
genericShowbPrec1 = genericLiftShowbPrec genericShowbPrec genericShowbList

-- | A type-specialized version of 'from' used to assist type inference.
fromRepUnit :: Generic a => a -> Rep a ()
fromRepUnit = from

-------------------------------------------------------------------------------

-- | Whether a constructor is a record ('Rec'), a tuple ('Tup'), is prefix ('Pref'),
-- or infix ('Inf').
--
-- /Since: 2/
data ConType = Rec | Tup | Pref | Inf String
  deriving ( Data
           , Eq
           , Generic
           , Ord
           , Read
           , Show
           , Typeable
#if __GLASGOW_HASKELL__ >= 800
           , Lift
#endif
           )

-- | TODO RGS: Docs
class (IsString text, Monoid text) => TextLike text where
  -- | TODO RGS: Docs
  fromChar :: Char -> text

  -- | TODO RGS: Docs
  space :: text
  space = fromChar ' '

  -- | Like 'showParen', but generalized to work over any 'TextLike' instance.
  withParens :: Bool -> text -> text
  withParens b p = if b then fromChar '(' <> p <> fromChar ')' else p
deriving instance Typeable TextLike

instance TextLike Builder where
  fromChar   = TB.singleton
  space      = showbSpace
  withParens = showbParen

instance TextLike TS.Text where
  fromChar   = TS.singleton
  space      = showtSpace
  withParens = showtParen

instance TextLike TL.Text where
  fromChar   = TL.singleton
  space      = showtlSpace
  withParens = showtlParen

{- | Class of generic representation types that can be converted to a @text@
type.

/Since: 3.10/
-}
class TextLike text => GTextShow text a where
  gShowPrec :: Int -> a -> text
deriving instance Typeable GTextShow

instance GTextShow text (f p) => GTextShow text (D1 d f p) where
  gShowPrec p (M1 x) = gShowPrec p x

instance TextLike text => GTextShow text (V1 p) where
  gShowPrec _ x = case x of {}

instance (GTextShow text (f p), GTextShow text (g p)) => GTextShow text ((f :+: g) p) where
  gShowPrec p (L1 x) = gShowPrec p x
  gShowPrec p (R1 x) = gShowPrec p x

instance (Constructor c, GTextShowCon text (f p), IsNullary f)
    => GTextShow text (C1 c f p) where
  gShowPrec = c1ShowPrec gShowPrecCon

{- | Class of generic representation types for which the 'ConType' has been
determined.

/Since: 3.10/
-}
class TextLike text => GTextShowCon text a where
  gShowPrecCon :: ConType -> Int -> a -> text
deriving instance Typeable GTextShowCon

instance TextLike text => GTextShowCon text (U1 p) where
  gShowPrecCon _ _ U1 = mempty

instance TextShow p => GTextShowCon Builder (Par1 p) where
  gShowPrecCon _ p (Par1 x) = showbPrec p x
instance TextShow p => GTextShowCon TS.Text (Par1 p) where
  gShowPrecCon _ p (Par1 x) = showtPrec p x
instance TextShow p => GTextShowCon TL.Text (Par1 p) where
  gShowPrecCon _ p (Par1 x) = showtlPrec p x

instance TextShow c => GTextShowCon Builder (K1 i c p) where
  gShowPrecCon _ p (K1 x) = showbPrec p x
instance TextShow c => GTextShowCon TS.Text (K1 i c p) where
  gShowPrecCon _ p (K1 x) = showtPrec p x
instance TextShow c => GTextShowCon TL.Text (K1 i c p) where
  gShowPrecCon _ p (K1 x) = showtlPrec p x

instance (TextShow1 f, TextShow p) => GTextShowCon Builder (Rec1 f p) where
  gShowPrecCon _ p (Rec1 x) = showbPrec1 p x
instance (TextShow1 f, TextShow p) => GTextShowCon TS.Text (Rec1 f p) where
  gShowPrecCon _ p (Rec1 x) = liftShowtPrec showtPrec showtList p x
instance (TextShow1 f, TextShow p) => GTextShowCon TL.Text (Rec1 f p) where
  gShowPrecCon _ p (Rec1 x) = liftShowtlPrec showtlPrec showtlList p x

instance (Selector s, GTextShowCon text (f p)) => GTextShowCon text (S1 s f p) where
  gShowPrecCon t = s1ShowPrec $ gShowPrecCon t

instance (GTextShowCon text (f p), GTextShowCon text (g p))
      => GTextShowCon text ((f :*: g) p) where
  gShowPrecCon t = productShowPrec (gShowPrecCon t) (gShowPrecCon t) t

instance (TextShow1 f, GTextShowCon Builder (g p)) => GTextShowCon Builder ((f :.: g) p) where
  gShowPrecCon t p (Comp1 x) =
    let gspc = gShowPrecCon t
    in liftShowbPrec gspc (showbListWith (gspc 0)) p x
instance (TextShow1 f, GTextShowCon TS.Text (g p)) => GTextShowCon TS.Text ((f :.: g) p) where
  gShowPrecCon t p (Comp1 x) =
    let gspc = gShowPrecCon t
    in liftShowtPrec gspc (showtListWith (gspc 0)) p x
instance (TextShow1 f, GTextShowCon TL.Text (g p)) => GTextShowCon TL.Text ((f :.: g) p) where
  gShowPrecCon t p (Comp1 x) =
    let gspc = gShowPrecCon t
    in liftShowtlPrec gspc (showtlListWith (gspc 0)) p x

instance GTextShowCon Builder (UChar p) where
  gShowPrecCon _ = uCharShowPrec showbPrec
instance GTextShowCon TS.Text (UChar p) where
  gShowPrecCon _ = uCharShowPrec showtPrec
instance GTextShowCon TL.Text (UChar p) where
  gShowPrecCon _ = uCharShowPrec showtlPrec

instance GTextShowCon Builder (UDouble p) where
  gShowPrecCon _ = uDoubleShowPrec showbPrec
instance GTextShowCon TS.Text (UDouble p) where
  gShowPrecCon _ = uDoubleShowPrec showtPrec
instance GTextShowCon TL.Text (UDouble p) where
  gShowPrecCon _ = uDoubleShowPrec showtlPrec

instance GTextShowCon Builder (UFloat p) where
  gShowPrecCon _ = uFloatShowPrec showbPrec
instance GTextShowCon TS.Text (UFloat p) where
  gShowPrecCon _ = uFloatShowPrec showtPrec
instance GTextShowCon TL.Text (UFloat p) where
  gShowPrecCon _ = uFloatShowPrec showtlPrec

instance GTextShowCon Builder (UInt p) where
  gShowPrecCon _ = uIntShowPrec showbPrec
instance GTextShowCon TS.Text (UInt p) where
  gShowPrecCon _ = uIntShowPrec showtPrec
instance GTextShowCon TL.Text (UInt p) where
  gShowPrecCon _ = uIntShowPrec showtlPrec

instance GTextShowCon Builder (UWord p) where
  gShowPrecCon _ = uWordShowPrec showbPrec
instance GTextShowCon TS.Text (UWord p) where
  gShowPrecCon _ = uWordShowPrec showtPrec
instance GTextShowCon TL.Text (UWord p) where
  gShowPrecCon _ = uWordShowPrec showtlPrec

{- | Class of generic representation types for unary type constructors that can
be converted to a @text@ type.

/Since: 3.10/
-}
class TextLike text => GTextShow1 text f where
  gLiftShowPrec :: (Int -> a -> text) -> ([a] -> text)
                -> Int -> f a -> text
deriving instance Typeable GTextShow1

instance GTextShow1 text f => GTextShow1 text (D1 d f) where
  gLiftShowPrec sp sl p (M1 x) = gLiftShowPrec sp sl p x

instance TextLike text => GTextShow1 text V1 where
  gLiftShowPrec _ _ _ x = case x of {}

instance (GTextShow1 text f, GTextShow1 text g) => GTextShow1 text (f :+: g) where
  gLiftShowPrec sp sl p (L1 x) = gLiftShowPrec sp sl p x
  gLiftShowPrec sp sl p (R1 x) = gLiftShowPrec sp sl p x

instance (Constructor c, GTextShowCon1 text f, IsNullary f)
    => GTextShow1 text (C1 c f) where
  gLiftShowPrec sp sl = c1ShowPrec $ gLiftShowPrecCon sp sl

{- | Class of generic representation types for unary type constructors for which
the 'ConType' has been determined.

/Since: 3.10/
-}
class TextLike text => GTextShowCon1 text f where
  gLiftShowPrecCon :: (Int -> a -> text) -> ([a] -> text)
                   -> ConType -> Int -> f a -> text
deriving instance Typeable GTextShowCon1

instance TextLike text => GTextShowCon1 text U1 where
  gLiftShowPrecCon _ _ _ _ U1 = mempty

instance TextLike text => GTextShowCon1 text Par1 where
  gLiftShowPrecCon sp _ _ p (Par1 x) = sp p x

instance TextShow c => GTextShowCon1 Builder (K1 i c) where
  gLiftShowPrecCon _ _ _ p (K1 x) = showbPrec p x
instance TextShow c => GTextShowCon1 TS.Text (K1 i c) where
  gLiftShowPrecCon _ _ _ p (K1 x) = showtPrec p x
instance TextShow c => GTextShowCon1 TL.Text (K1 i c) where
  gLiftShowPrecCon _ _ _ p (K1 x) = showtlPrec p x

instance TextShow1 f => GTextShowCon1 Builder (Rec1 f) where
  gLiftShowPrecCon sp sl _ p (Rec1 x) = liftShowbPrec sp sl p x
instance TextShow1 f => GTextShowCon1 TS.Text (Rec1 f) where
  gLiftShowPrecCon sp sl _ p (Rec1 x) = liftShowtPrec sp sl p x
instance TextShow1 f => GTextShowCon1 TL.Text (Rec1 f) where
  gLiftShowPrecCon sp sl _ p (Rec1 x) = liftShowtlPrec sp sl p x

instance (Selector s, GTextShowCon1 text f) => GTextShowCon1 text (S1 s f) where
  gLiftShowPrecCon sp sl t = s1ShowPrec $ gLiftShowPrecCon sp sl t

instance (GTextShowCon1 text f, GTextShowCon1 text g)
      => GTextShowCon1 text (f :*: g) where
  gLiftShowPrecCon sp sl t =
    productShowPrec (gLiftShowPrecCon sp sl t) (gLiftShowPrecCon sp sl t) t

instance (TextShow1 f, GTextShowCon1 Builder g) => GTextShowCon1 Builder (f :.: g) where
  gLiftShowPrecCon sp sl t p (Comp1 x) =
    let gspc = gLiftShowPrecCon sp sl t
    in liftShowbPrec gspc (showbListWith (gspc 0)) p x
instance (TextShow1 f, GTextShowCon1 TS.Text g) => GTextShowCon1 TS.Text (f :.: g) where
  gLiftShowPrecCon sp sl t p (Comp1 x) =
    let gspc = gLiftShowPrecCon sp sl t
    in liftShowtPrec gspc (showtListWith (gspc 0)) p x
instance (TextShow1 f, GTextShowCon1 TL.Text g) => GTextShowCon1 TL.Text (f :.: g) where
  gLiftShowPrecCon sp sl t p (Comp1 x) =
    let gspc = gLiftShowPrecCon sp sl t
    in liftShowtlPrec gspc (showtlListWith (gspc 0)) p x

instance GTextShowCon1 Builder UChar where
  gLiftShowPrecCon _ _ _ = uCharShowPrec showbPrec
instance GTextShowCon1 TS.Text UChar where
  gLiftShowPrecCon _ _ _ = uCharShowPrec showtPrec
instance GTextShowCon1 TL.Text UChar where
  gLiftShowPrecCon _ _ _ = uCharShowPrec showtlPrec

instance GTextShowCon1 Builder UDouble where
  gLiftShowPrecCon _ _ _ = uDoubleShowPrec showbPrec
instance GTextShowCon1 TS.Text UDouble where
  gLiftShowPrecCon _ _ _ = uDoubleShowPrec showtPrec
instance GTextShowCon1 TL.Text UDouble where
  gLiftShowPrecCon _ _ _ = uDoubleShowPrec showtlPrec

instance GTextShowCon1 Builder UFloat where
  gLiftShowPrecCon _ _ _ = uFloatShowPrec showbPrec
instance GTextShowCon1 TS.Text UFloat where
  gLiftShowPrecCon _ _ _ = uFloatShowPrec showtPrec
instance GTextShowCon1 TL.Text UFloat where
  gLiftShowPrecCon _ _ _ = uFloatShowPrec showtlPrec

instance GTextShowCon1 Builder UInt where
  gLiftShowPrecCon _ _ _ = uIntShowPrec showbPrec
instance GTextShowCon1 TS.Text UInt where
  gLiftShowPrecCon _ _ _ = uIntShowPrec showtPrec
instance GTextShowCon1 TL.Text UInt where
  gLiftShowPrecCon _ _ _ = uIntShowPrec showtlPrec

instance GTextShowCon1 Builder UWord where
  gLiftShowPrecCon _ _ _ = uWordShowPrec showbPrec
instance GTextShowCon1 TS.Text UWord where
  gLiftShowPrecCon _ _ _ = uWordShowPrec showtPrec
instance GTextShowCon1 TL.Text UWord where
  gLiftShowPrecCon _ _ _ = uWordShowPrec showtlPrec

c1ShowPrec :: forall text c f p.
              (TextLike text, Constructor c, IsNullary f)
           => (ConType -> Int -> f p -> text)
           -> Int -> C1 c f p -> text
c1ShowPrec sp p c@(M1 x) = case fixity of
  Prefix -> withParens ( p > appPrec
                         && not (isNullary x || conIsTuple c)
                       ) $
         (if conIsTuple c
             then mempty
             else let cn = conName c
                  in withParens (isInfixDataCon cn) $ fromString cn)
      <> (if isNullary x || conIsTuple c
             then mempty
             else fromChar ' ')
      <> showBraces t (sp t appPrec1 x)
  Infix _ m -> withParens (p > m) $ sp t (m+1) x
  where
    fixity :: Fixity
    fixity = conFixity c

    t :: ConType
    t = if conIsRecord c
        then Rec
        else case conIsTuple c of
          True  -> Tup
          False -> case fixity of
            Prefix    -> Pref
            Infix _ _ -> Inf $ conName c

    showBraces :: ConType -> text -> text
    showBraces Rec     b = fromChar '{' <> b <> fromChar '}'
    showBraces Tup     b = fromChar '(' <> b <> fromChar ')'
    showBraces Pref    b = b
    showBraces (Inf _) b = b

    conIsTuple :: C1 c f p -> Bool
    conIsTuple = isTupleString . conName

s1ShowPrec :: forall text s f p.
              (TextLike text, Selector s)
           => (Int -> f p -> text)
           -> Int -> S1 s f p -> text
s1ShowPrec sp p sel@(M1 x)
  | selName sel == "" = sp p x
  | otherwise         = infixRec
                        <> " = "
                        <> sp 0 x
  where
    infixRec :: text
    infixRec | isSymVar selectorName
             = fromChar '(' <> fromString selectorName <> fromChar ')'
             | otherwise
             = fromString selectorName

    selectorName :: String
    selectorName = selName sel

productShowPrec :: forall text f g p.
                   TextLike text
                => (Int -> f p -> text) -> (Int -> g p -> text)
                -> ConType -> Int -> (f :*: g) p -> text
productShowPrec spf spg t p (a :*: b) =
  case t of
    Rec ->
         spf 0 a
      <> ", "
      <> spg 0 b
    Inf o ->
         spf p a
      <> space
      <> infixOp o
      <> space
      <> spg p b
    Tup ->
         spf 0 a
      <> fromChar ','
      <> spg 0 b
    Pref ->
         spf p a
      <> space
      <> spg p b
  where
    infixOp :: String -> text
    infixOp o = if isInfixDataCon o
                   then fromString o
                   else fromChar '`' <> fromString o <> fromChar '`'

uCharShowPrec :: TextLike text => (Int -> Char -> text) -> Int -> UChar p -> text
uCharShowPrec sp p (UChar c) = sp (hashPrec p) (C# c) <> oneHash

uDoubleShowPrec :: TextLike text => (Int -> Double -> text) -> Int -> UDouble p -> text
uDoubleShowPrec sp p (UDouble d) = sp (hashPrec p) (D# d) <> twoHash

uFloatShowPrec :: TextLike text => (Int -> Float -> text) -> Int -> UFloat p -> text
uFloatShowPrec sp p (UFloat f) = sp (hashPrec p) (F# f) <> oneHash

uIntShowPrec :: TextLike text => (Int -> Int -> text) -> Int -> UInt p -> text
uIntShowPrec sp p (UInt i) = sp (hashPrec p) (I# i) <> oneHash

uWordShowPrec :: TextLike text => (Int -> Word -> text) -> Int -> UWord p -> text
uWordShowPrec sp p (UWord w) = sp (hashPrec p) (W# w) <> twoHash

oneHash, twoHash :: TextLike text => text
hashPrec :: Int -> Int
#if __GLASGOW_HASKELL__ >= 711
oneHash  = fromChar '#'
twoHash  = fromString "##"
hashPrec = const 0
#else
oneHash  = mempty
twoHash  = mempty
hashPrec = id
#endif

-- | Class of generic representation types that represent a constructor with
-- zero or more fields.
class IsNullary f where
    -- Returns 'True' if the constructor has no fields.
    isNullary :: f a -> Bool

instance IsNullary U1 where
    isNullary _ = True

instance IsNullary Par1 where
    isNullary _ = False

instance IsNullary (K1 i c) where
    isNullary _ = False

instance IsNullary f => IsNullary (S1 s f) where
    isNullary (M1 x) = isNullary x

instance IsNullary (Rec1 f) where
    isNullary _ = False

instance IsNullary (f :*: g) where
    isNullary _ = False

instance IsNullary (f :.: g) where
    isNullary _ = False

instance IsNullary UChar where
    isNullary _ = False

instance IsNullary UDouble where
    isNullary _ = False

instance IsNullary UFloat where
    isNullary _ = False

instance IsNullary UInt where
    isNullary _ = False

instance IsNullary UWord where
    isNullary _ = False

-------------------------------------------------------------------------------

$(deriveTextShow ''ConType)

#if __GLASGOW_HASKELL__ < 800
$(deriveLift ''ConType)
$(deriveLift ''FromGeneric)

instance Lift (f a) => Lift (FromGeneric1 f a) where
    lift = $(makeLift ''FromGeneric1)
#endif

#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
$(Generics.deriveMeta           ''FromGeneric1)
$(Generics.deriveRepresentable1 ''FromGeneric1)
#endif
