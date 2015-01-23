{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
#if MIN_VERSION_base(4,4,0)
{-# LANGUAGE FlexibleContexts, TypeOperators #-}
#endif
#if MIN_VERSION_base(4,7,0)
{-# LANGUAGE TypeFamilies #-}
# if !(MIN_VERSION_base(4,8,0))
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
# endif
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.BaseAndFriends
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Provides 'Arbitrary' instances for data types located in @base@ and other
common libraries. This module also defines 'Show' instances for some data
types as well (e.g., those which do not derive 'Show' in older versions
of GHC).
-}
module Instances.BaseAndFriends () where

import           Control.Applicative (ZipList(..))
#if !(MIN_VERSION_base(4,8,0))
import           Control.Applicative ((<*>), pure)
#endif
import           Control.Exception
import           Control.Monad.ST (ST, fixST)

#if MIN_VERSION_bytestring(0,10,4)
import           Data.ByteString.Short (ShortByteString, pack)
#endif
import           Data.Char (GeneralCategory(..))
import qualified Data.Data as D (Fixity(..))
import           Data.Data (Constr, ConstrRep(..), DataRep(..), DataType,
                            mkConstr, mkDataType)
import           Data.Dynamic (Dynamic, toDyn)
import           Data.Functor ((<$>))
#if !(MIN_VERSION_transformers(0,4,0))
import           Data.Functor.Classes ()
#endif
import           Data.Functor.Identity (Identity(..))
import           Data.Monoid (All(..), Any(..), Dual(..), First(..),
                              Last(..), Product(..), Sum(..))
#if MIN_VERSION_base(4,8,0)
import           Data.Monoid (Alt(..))
#endif
#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
import qualified Data.OldTypeable.Internal as OldT (TyCon(..))
#endif
#if MIN_VERSION_base(4,6,0)
import           Data.Ord (Down(..))
#endif
import           Data.Proxy (Proxy(..))
import           Data.Text.Lazy.Builder (Builder, fromString)
#if MIN_VERSION_base(4,7,0)
import           Data.Coerce (Coercible)
import           Data.Type.Coercion (Coercion(..))
import           Data.Type.Equality ((:~:)(..))
#endif
#if MIN_VERSION_base(4,4,0)
import qualified Data.Typeable.Internal as NewT (TyCon(..))
import           GHC.Fingerprint.Type (Fingerprint(..))

#if !(MIN_VERSION_base(4,7,0))
import           Data.Word (Word64)
import           Numeric (showHex)
#endif
#endif
import           Data.Version (Version(..))

import           Foreign.C.Types
import           Foreign.Ptr (FunPtr, IntPtr, Ptr, WordPtr,
                              castPtrToFunPtr, nullPtr, plusPtr,
                              ptrToIntPtr, ptrToWordPtr)

import           GHC.Conc (BlockReason(..), ThreadStatus(..))
#if defined(mingw32_HOST_OS)
import           GHC.Conc.Windows (ConsoleEvent(..))
#endif
#if MIN_VERSION_base(4,4,0)
import           GHC.IO.Encoding.Failure (CodingFailureMode(..))
import           GHC.IO.Encoding.Types (CodingProgress(..))
import qualified GHC.Generics as G (Fixity(..))
import           GHC.Generics (U1(..), Par1(..), Rec1(..), K1(..),
                               M1(..), (:+:)(..), (:*:)(..), (:.:)(..),
                               Associativity(..), Arity(..))
#endif
#if MIN_VERSION_base(4,8,0)
import           GHC.RTS.Flags
import           GHC.StaticPtr (StaticPtrInfo(..))
#endif
#if MIN_VERSION_base(4,5,0)
import           GHC.Stats (GCStats(..))
#endif
#if MIN_VERSION_base(4,7,0)
import           GHC.TypeLits (SomeNat, SomeSymbol, someNatVal, someSymbolVal)
#endif

import           Numeric.Natural (Natural)

import           System.Exit (ExitCode(..))
import           System.IO (BufferMode(..), IOMode(..), Newline(..),
                            NewlineMode(..), SeekMode(..))
import           System.Posix.Types

import           Test.Tasty.QuickCheck (Arbitrary(arbitrary), Gen,
                                        arbitraryBoundedEnum, oneof, suchThat)

#include "HsBaseConfig.h"

instance Arbitrary Natural where
    arbitrary = fromInteger <$> arbitrary `suchThat` (>= 0)

instance Arbitrary Builder where
    arbitrary = fromString <$> arbitrary

#if MIN_VERSION_bytestring(0,10,4)
instance Arbitrary ShortByteString where
    arbitrary = pack <$> arbitrary
#endif

instance Arbitrary (Ptr a) where
    arbitrary = plusPtr nullPtr <$> arbitrary

instance Arbitrary (FunPtr a) where
    arbitrary = castPtrToFunPtr <$> arbitrary

instance Arbitrary IntPtr where
    arbitrary = ptrToIntPtr <$> arbitrary

instance Arbitrary WordPtr where
    arbitrary = ptrToWordPtr <$> arbitrary

-- TODO: instance Arbitrary (ForeignPtr a)

instance Arbitrary GeneralCategory where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Version where
    arbitrary = Version <$> arbitrary <*> arbitrary

-- TODO: Be more creative with this instance
instance Arbitrary SomeException where
    arbitrary = SomeException <$> (arbitrary :: Gen AssertionFailed)

-- instance Arbitrary IOException

deriving instance Bounded ArithException
deriving instance Enum ArithException
instance Arbitrary ArithException where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary ArrayException where
    arbitrary = oneof [ IndexOutOfBounds <$> arbitrary
                      , UndefinedElement <$> arbitrary
                      ]

instance Arbitrary AssertionFailed where
    arbitrary = AssertionFailed <$> arbitrary

#if MIN_VERSION_base(4,7,0)
-- TODO: Be more creative with this instance
instance Arbitrary SomeAsyncException where
    arbitrary = SomeAsyncException <$> (arbitrary :: Gen AsyncException)
#endif

deriving instance Bounded AsyncException
deriving instance Enum AsyncException
instance Arbitrary AsyncException where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary NonTermination where
    arbitrary = pure NonTermination

instance Arbitrary NestedAtomically where
    arbitrary = pure NestedAtomically

instance Arbitrary BlockedIndefinitelyOnMVar where
    arbitrary = pure BlockedIndefinitelyOnMVar

instance Arbitrary BlockedIndefinitelyOnSTM where
    arbitrary = pure BlockedIndefinitelyOnSTM

#if MIN_VERSION_base(4,8,0)
instance Arbitrary AllocationLimitExceeded where
    arbitrary = pure AllocationLimitExceeded
#endif

instance Arbitrary Deadlock where
    arbitrary = pure Deadlock

instance Arbitrary NoMethodError where
    arbitrary = NoMethodError <$> arbitrary

instance Arbitrary PatternMatchFail where
    arbitrary = PatternMatchFail <$> arbitrary

instance Arbitrary RecConError where
    arbitrary = RecConError <$> arbitrary

instance Arbitrary RecSelError where
    arbitrary = RecSelError <$> arbitrary

instance Arbitrary RecUpdError where
    arbitrary = RecUpdError <$> arbitrary

-- ErrorCall is a newtype starting with base-4.7.0.0, but we'll
-- manually derive Arbitrary to support older versions of GHC.
-- 
-- deriving instance Arbitrary ErrorCall
instance Arbitrary ErrorCall where
    arbitrary = ErrorCall <$> arbitrary

deriving instance Bounded MaskingState
deriving instance Enum MaskingState
instance Arbitrary MaskingState where
    arbitrary = arbitraryBoundedEnum

-- instance Arbitrary Lexeme
-- #if MIN_VERSION_base(4,7,0)
-- instance Arbitrary Number
-- #endif

instance Arbitrary (Proxy s) where
    arbitrary = pure Proxy

#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
-- TODO: Come up with an instance of TypeRep that doesn't take forever
-- instance Arbitrary OldT.TypeRep where

instance Arbitrary OldT.TyCon where
    arbitrary = OldT.TyCon <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
#endif

#if MIN_VERSION_base(4,4,0)
-- TODO: Come up with an instance of TypeRep that doesn't take forever
-- instance Arbitrary NewT.TypeRep where

instance Arbitrary NewT.TyCon where
    arbitrary = NewT.TyCon <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Fingerprint where
    arbitrary = Fingerprint <$> arbitrary <*> arbitrary

#if !(MIN_VERSION_base(4,7,0))
instance Show Fingerprint where
  show (Fingerprint w1 w2) = hex16 w1 ++ hex16 w2
    where
      -- | Formats a 64 bit number as 16 digits hex.
      hex16 :: Word64 -> String
      hex16 i = let hex = showHex i ""
                 in replicate (16 - length hex) '0' ++ hex
#endif
#endif

-- TODO: Be more creative with this instance
instance Arbitrary Dynamic where
    arbitrary = toDyn <$> (arbitrary :: Gen Int)

instance Arbitrary Constr where
    arbitrary = mkConstr <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ConstrRep where
    arbitrary = oneof [ AlgConstr   <$> arbitrary
                      , IntConstr   <$> arbitrary
                      , FloatConstr <$> arbitrary
                      , CharConstr  <$> arbitrary
                      ]

instance Arbitrary DataRep where
    arbitrary = oneof [ AlgRep <$> arbitrary
                      , pure IntRep
                      , pure FloatRep
                      , pure CharRep
                      , pure NoRep
                      ]

instance Arbitrary DataType where
    arbitrary = mkDataType <$> arbitrary <*> arbitrary

deriving instance Bounded D.Fixity
deriving instance Enum D.Fixity
instance Arbitrary D.Fixity where
    arbitrary = arbitraryBoundedEnum

#if MIN_VERSION_base(4,7,0)
instance Coercible a b => Arbitrary (Coercion a b) where
    arbitrary = pure Coercion

instance a ~ b => Arbitrary (a :~: b) where
    arbitrary = pure Refl
#endif

deriving instance Bounded BlockReason
deriving instance Enum BlockReason
instance Arbitrary BlockReason where
    arbitrary = arbitraryBoundedEnum

-- instance Arbitrary ThreadId

instance Arbitrary ThreadStatus where
    arbitrary = oneof [ pure ThreadRunning
                      , pure ThreadFinished
                      , ThreadBlocked <$> arbitrary
                      , pure ThreadDied
                      ]

#if defined(mingw32_HOST_OS)
deriving instance Bounded ConsoleEvent
instance Arbitrary ConsoleEvent where
    arbitrary = arbitraryBoundedEnum
#endif

instance Arbitrary (ST s a) where
    arbitrary = pure $ fixST undefined

-- instance Arbitrary Handle
-- instance Arbitrary HandlePosn

deriving instance Bounded IOMode
instance Arbitrary IOMode where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary BufferMode where
    arbitrary = oneof [ pure NoBuffering
                      , pure LineBuffering
                      , BlockBuffering <$> arbitrary
                      ]

deriving instance Bounded SeekMode
instance Arbitrary SeekMode where
    arbitrary = arbitraryBoundedEnum

deriving instance Bounded Newline
deriving instance Enum Newline
instance Arbitrary Newline where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary NewlineMode where
    arbitrary = NewlineMode <$> arbitrary <*> arbitrary

#if MIN_VERSION_base(4,3,0)
-- instance Arbitrary TextEncoding
#else
deriving instance Show Newline
deriving instance Show NewlineMode
#endif

#if MIN_VERSION_base(4,4,0)
deriving instance Bounded CodingProgress
deriving instance Enum CodingProgress
instance Arbitrary CodingProgress where
    arbitrary = arbitraryBoundedEnum

deriving instance Bounded CodingFailureMode
deriving instance Enum CodingFailureMode
instance Arbitrary CodingFailureMode where
    arbitrary = arbitraryBoundedEnum
#endif

#if MIN_VERSION_base(4,5,0)
instance Arbitrary GCStats where
    arbitrary = GCStats <$> arbitrary <*> arbitrary <*> arbitrary
                        <*> arbitrary <*> arbitrary <*> arbitrary
                        <*> arbitrary <*> arbitrary <*> arbitrary
                        <*> arbitrary <*> arbitrary <*> arbitrary
                        <*> arbitrary <*> arbitrary <*> arbitrary
                        <*> arbitrary <*> arbitrary <*> arbitrary
#endif

-- #if MIN_VERSION_base(4,4,0)
-- instance Arbitrary Event
-- instance Arbitrary FdKey
-- #endif

#if MIN_VERSION_base(4,4,0)
instance Arbitrary (U1 p) where
    arbitrary = pure U1

instance Arbitrary p => Arbitrary (Par1 p) where
    arbitrary = Par1 <$> arbitrary

instance Arbitrary (f p) => Arbitrary (Rec1 f p) where
    arbitrary = Rec1 <$> arbitrary

instance Arbitrary c => Arbitrary (K1 i c p) where
    arbitrary = K1 <$> arbitrary

instance Arbitrary (f p) => Arbitrary (M1 i c f p) where
    arbitrary = M1 <$> arbitrary

instance (Arbitrary (f p), Arbitrary (g p)) => Arbitrary ((f :+: g) p) where
    arbitrary = oneof [L1 <$> arbitrary, R1 <$> arbitrary]

instance (Arbitrary (f p), Arbitrary (g p)) => Arbitrary ((f :*: g) p) where
    arbitrary = (:*:) <$> arbitrary <*> arbitrary

instance Arbitrary (f (g p)) => Arbitrary ((f :.: g) p) where
    arbitrary = Comp1 <$> arbitrary

instance Arbitrary G.Fixity where
    arbitrary = oneof [pure G.Prefix, G.Infix <$> arbitrary <*> arbitrary]

deriving instance Bounded Associativity
deriving instance Enum Associativity
instance Arbitrary Associativity where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Arity where
    arbitrary = oneof [pure NoArity, Arity <$> arbitrary]

#if !(MIN_VERSION_base(4,7,0))
deriving instance                             Show (U1 p)
deriving instance Show p                   => Show (Par1 p)
deriving instance Show (f p)               => Show (Rec1 f p)
deriving instance Show c                   => Show (K1 i c p)
deriving instance Show (f p)               => Show (M1 i c f p)
deriving instance (Show (f p), Show (g p)) => Show ((f :+: g) p)

-- Due to a GHC bug (https://ghc.haskell.org/trac/ghc/ticket/9830), this Show
-- instance produces output with the wrong precedence on older versions of GHC.
-- I'll manually define the Show instance to get the correct behavior.
-- 
-- deriving instance (Show (f p), Show (g p)) => Show ((f :*: g) p)
instance (Show (f p), Show (g p)) => Show ((f :*: g) p) where
    showsPrec p (l :*: r) = showParen (p > prec) $
          showsPrec (prec + 1) l
        . showString " :*: "
        . showsPrec (prec + 1) r
      where prec = 6

deriving instance Show (f (g p))           => Show ((f :.: g) p)
#endif
#endif

#if MIN_VERSION_base(4,8,0)
-- instance Arbitrary RTSFlags
-- instance Arbitrary GCFlags

instance Arbitrary ConcFlags where
    arbitrary = ConcFlags <$> arbitrary <*> arbitrary

instance Arbitrary MiscFlags where
    arbitrary = MiscFlags <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary DebugFlags where
    arbitrary = DebugFlags <$> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary

-- instance Arbitrary CCFlags where
-- instance Arbitrary ProfFlags where
-- instance Arbitrary TraceFlags where

instance Arbitrary TickyFlags where
    arbitrary = TickyFlags <$> arbitrary <*> arbitrary

instance Arbitrary StaticPtrInfo where
    arbitrary = StaticPtrInfo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
#endif

-- #if MIN_VERSION_base(4,6,0) && !(MIN_VERSION_base(4,7,0))
-- instance Arbitrary (IsZero n)
-- instance Arbitrary (IsEven n)
-- #endif

#if MIN_VERSION_base(4,7,0)
instance Arbitrary SomeNat where
    arbitrary = do
        nat <- arbitrary `suchThat` (>= 0)
        case someNatVal nat of
             Just sn -> pure sn
             Nothing -> fail "Negative natural number"

instance Arbitrary SomeSymbol where
    arbitrary = someSymbolVal <$> arbitrary
#endif

deriving instance Arbitrary CChar
deriving instance Arbitrary CSChar
deriving instance Arbitrary CUChar
deriving instance Arbitrary CShort
deriving instance Arbitrary CUShort
deriving instance Arbitrary CInt
deriving instance Arbitrary CUInt
deriving instance Arbitrary CLong
deriving instance Arbitrary CULong
deriving instance Arbitrary CLLong
deriving instance Arbitrary CULLong
deriving instance Arbitrary CFloat
deriving instance Arbitrary CDouble
deriving instance Arbitrary CPtrdiff
deriving instance Arbitrary CSize
deriving instance Arbitrary CWchar
deriving instance Arbitrary CSigAtomic
deriving instance Arbitrary CClock
deriving instance Arbitrary CTime
deriving instance Arbitrary CUSeconds
deriving instance Arbitrary CSUSeconds
deriving instance Arbitrary CIntPtr
deriving instance Arbitrary CUIntPtr
deriving instance Arbitrary CIntMax
deriving instance Arbitrary CUIntMax

instance Arbitrary ExitCode where
    arbitrary = oneof [pure ExitSuccess, ExitFailure <$> arbitrary]

#if defined(HTYPE_DEV_T)
deriving instance Arbitrary CDev
#endif
#if defined(HTYPE_INO_T)
deriving instance Arbitrary CIno
#endif
#if defined(HTYPE_MODE_T)
deriving instance Arbitrary CMode
#endif
#if defined(HTYPE_OFF_T)
deriving instance Arbitrary COff
#endif
#if defined(HTYPE_PID_T)
deriving instance Arbitrary CPid
#endif
#if defined(HTYPE_SSIZE_T)
deriving instance Arbitrary CSsize
#endif
#if defined(HTYPE_GID_T)
deriving instance Arbitrary CGid
#endif
#if defined(HTYPE_NLINK_T)
deriving instance Arbitrary CNlink
#endif
#if defined(HTYPE_UID_T)
deriving instance Arbitrary CUid
#endif
#if defined(HTYPE_CC_T)
deriving instance Arbitrary CCc
#endif
#if defined(HTYPE_SPEED_T)
deriving instance Arbitrary CSpeed
#endif
#if defined(HTYPE_TCFLAG_T)
deriving instance Arbitrary CTcflag
#endif
#if defined(HTYPE_RLIM_T)
deriving instance Arbitrary CRLim
#endif
deriving instance Arbitrary Fd

deriving instance Arbitrary All
deriving instance Arbitrary Any
deriving instance Arbitrary a => Arbitrary (Dual a)
deriving instance Arbitrary a => Arbitrary (First a)
deriving instance Arbitrary a => Arbitrary (Last a)
deriving instance Arbitrary a => Arbitrary (Product a)
deriving instance Arbitrary a => Arbitrary (Sum a)
#if MIN_VERSION_base(4,8,0)
deriving instance Arbitrary (f a) => Arbitrary (Alt f a)
#endif

deriving instance Arbitrary a => Arbitrary (ZipList a)
#if !(MIN_VERSION_base(4,7,0))
deriving instance Show a => Show (ZipList a)
#endif

#if MIN_VERSION_base(4,6,0)
deriving instance Arbitrary a => Arbitrary (Down a)
#if !(MIN_VERSION_base(4,7,0))
deriving instance Show a => Show (Down a)
#endif
#endif

deriving instance Arbitrary a => Arbitrary (Identity a)