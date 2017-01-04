{-|
Module:      TextShow.Instances
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Imports all orphan 'TextShow', 'TextShow1', and 'TextShow2' instances covered
by @text-show@ (except for the instances in `TextShow.Functions`, which are not
imported by default).
-}
module TextShow.Instances () where

import TextShow.Control.Applicative   ()
import TextShow.Control.Concurrent    ()
import TextShow.Control.Exception     ()
import TextShow.Control.Monad.ST      ()

import TextShow.Data.Array            ()
import TextShow.Data.Bool             ()
import TextShow.Data.ByteString       ()
import TextShow.Data.Char             ()
import TextShow.Data.Complex          ()
import TextShow.Data.Data             ()
import TextShow.Data.Dynamic          ()
import TextShow.Data.Either           ()
import TextShow.Data.Fixed            ()
import TextShow.Data.Floating         ()
import TextShow.Data.Functor.Compose  ()
import TextShow.Data.Functor.Identity ()
import TextShow.Data.Functor.Product  ()
import TextShow.Data.Functor.Sum      ()
import TextShow.Data.Integral         ()
import TextShow.Data.List             ()
import TextShow.Data.List.NonEmpty    ()
import TextShow.Data.Maybe            ()
import TextShow.Data.Monoid           ()
import TextShow.Data.OldTypeable      ()
import TextShow.Data.Ord              ()
import TextShow.Data.Proxy            ()
import TextShow.Data.Ratio            ()
import TextShow.Data.Semigroup        ()
import TextShow.Data.Text             ()
import TextShow.Data.Tuple            ()
import TextShow.Data.Type.Coercion    ()
import TextShow.Data.Type.Equality    ()
import TextShow.Data.Typeable         ()
import TextShow.Data.Version          ()
import TextShow.Data.Void             ()

import TextShow.Foreign.C.Types       ()
import TextShow.Foreign.Ptr           ()

import TextShow.GHC.Conc.Windows      ()
import TextShow.GHC.Event             ()
import TextShow.GHC.Fingerprint       ()
import TextShow.GHC.Generics          ()
import TextShow.GHC.RTS.Flags         ()
import TextShow.GHC.Stack             ()
import TextShow.GHC.StaticPtr         ()
import TextShow.GHC.Stats             ()
import TextShow.GHC.TypeLits          ()

import TextShow.Numeric.Natural       ()

import TextShow.System.Exit           ()
import TextShow.System.IO             ()
import TextShow.System.Posix.Types    ()

import TextShow.Text.Read             ()
