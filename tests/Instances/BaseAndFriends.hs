{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
#if MIN_VERSION_base(4,4,0)
{-# LANGUAGE FlexibleContexts, TypeOperators #-}
#endif
#if MIN_VERSION_base(4,7,0)
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Instances.BaseAndFriends
Copyright:   (C) 2014 Ryan Scott
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

import           Control.Applicative (ZipList(..), (<$>), (<*>), pure)
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
import           Data.Monoid (All(..), Any(..), Dual(..), First(..),
                              Last(..), Product(..), Sum(..))
#if MIN_VERSION_base(4,7,0)
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
#if MIN_VERSION_base(4,4,0)
import           GHC.IO.Encoding.Failure (CodingFailureMode(..))
import           GHC.IO.Encoding.Types (CodingProgress(..))
import qualified GHC.Generics as G (Fixity(..))
import           GHC.Generics (U1(..), Par1(..), Rec1(..), K1(..),
                               M1(..), (:+:)(..), (:*:)(..), (:.:)(..),
                               Associativity(..), Arity(..))
#endif
#if MIN_VERSION_base(4,5,0)
import           GHC.Stats (GCStats(..))
#endif

import           System.Exit (ExitCode(..))
import           System.IO (BufferMode(..), IOMode(..), Newline(..),
                            NewlineMode(..), SeekMode(..))
import           System.Posix.Types

import           Test.Tasty.QuickCheck (Arbitrary(arbitrary), Gen,
                                        arbitraryBoundedEnum, oneof)

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

instance Arbitrary ArithException where
    arbitrary = oneof $ map pure [ Overflow
                                 , Underflow
                                 , LossOfPrecision
                                 , DivideByZero
                                 , Denormal
#if MIN_VERSION_base(4,6,0)
                                 , RatioZeroDenominator
#endif
                                 ]

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

instance Arbitrary AsyncException where
    arbitrary = oneof $ map pure [ StackOverflow
                                 , HeapOverflow
                                 , ThreadKilled
                                 , UserInterrupt
                                 ]

instance Arbitrary NonTermination where
    arbitrary = pure NonTermination

instance Arbitrary NestedAtomically where
    arbitrary = pure NestedAtomically

instance Arbitrary BlockedIndefinitelyOnMVar where
    arbitrary = pure BlockedIndefinitelyOnMVar

instance Arbitrary BlockedIndefinitelyOnSTM where
    arbitrary = pure BlockedIndefinitelyOnSTM

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

instance Arbitrary MaskingState where
    arbitrary = oneof $ map pure [ Unmasked
                                 , MaskedInterruptible
                                 , MaskedUninterruptible
                                 ]

-- instance Arbitrary Lexeme
-- #if MIN_VERSION_base(4,7,0)
-- instance Arbitrary Number
-- #endif

instance Arbitrary (Proxy s) where
    arbitrary = pure Proxy

#if MIN_VERSION_base(4,7,0)
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

instance Arbitrary D.Fixity where
    arbitrary = oneof $ map pure [D.Prefix, D.Infix]

#if MIN_VERSION_base(4,7,0)
instance Coercible a b => Arbitrary (Coercion a b) where
    arbitrary = pure Coercion

instance a ~ b => Arbitrary (a :~: b) where
    arbitrary = pure Refl
#endif

instance Arbitrary BlockReason where
    arbitrary = oneof $ map pure [ BlockedOnMVar
                                 , BlockedOnBlackHole
                                 , BlockedOnException
                                 , BlockedOnSTM
                                 , BlockedOnForeignCall
                                 , BlockedOnOther
                                 ]

-- instance Arbitrary ThreadId

instance Arbitrary ThreadStatus where
    arbitrary = oneof [ pure ThreadRunning
                      , pure ThreadFinished
                      , ThreadBlocked <$> arbitrary
                      , pure ThreadDied
                      ]

instance Arbitrary (ST s a) where
    arbitrary = pure $ fixST undefined

-- instance Arbitrary Handle
-- instance Arbitrary HandlePosn

instance Arbitrary IOMode where
    arbitrary = oneof $ map pure [ReadMode, WriteMode, AppendMode, ReadWriteMode]

instance Arbitrary BufferMode where
    arbitrary = oneof [ pure NoBuffering
                      , pure LineBuffering
                      , BlockBuffering <$> arbitrary
                      ]

instance Arbitrary SeekMode where
    arbitrary = oneof $ map pure [AbsoluteSeek, RelativeSeek, SeekFromEnd]

instance Arbitrary Newline where
    arbitrary = oneof $ map pure [LF, CRLF]

instance Arbitrary NewlineMode where
    arbitrary = NewlineMode <$> arbitrary <*> arbitrary

#if MIN_VERSION_base(4,3,0)
-- instance Arbitrary TextEncoding
#else
deriving instance Show Newline
deriving instance Show NewlineMode
#endif

#if MIN_VERSION_base(4,4,0)
instance Arbitrary CodingProgress where
    arbitrary = oneof $ map pure [InputUnderflow, OutputUnderflow, InvalidSequence]

instance Arbitrary CodingFailureMode where
    arbitrary = oneof $ map pure [ ErrorOnCodingFailure
                                 , IgnoreCodingFailure
                                 , TransliterateCodingFailure
                                 , RoundtripFailure
                                 ]
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

instance Arbitrary Associativity where
    arbitrary = oneof $ map pure [LeftAssociative, RightAssociative, NotAssociative]

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

deriving instance Arbitrary CDev
deriving instance Arbitrary CIno
deriving instance Arbitrary CMode
deriving instance Arbitrary COff
deriving instance Arbitrary CPid
deriving instance Arbitrary CSsize
deriving instance Arbitrary CGid
deriving instance Arbitrary CNlink
deriving instance Arbitrary CUid
deriving instance Arbitrary CCc
deriving instance Arbitrary CSpeed
deriving instance Arbitrary CTcflag
deriving instance Arbitrary CRLim
deriving instance Arbitrary Fd

deriving instance Arbitrary All
deriving instance Arbitrary Any
deriving instance Arbitrary a => Arbitrary (Dual a)
deriving instance Arbitrary a => Arbitrary (First a)
deriving instance Arbitrary a => Arbitrary (Last a)
deriving instance Arbitrary a => Arbitrary (Product a)
deriving instance Arbitrary a => Arbitrary (Sum a)
 
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