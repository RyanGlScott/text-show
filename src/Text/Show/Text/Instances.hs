{-|
Module:      Text.Show.Text.Instances
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Imports all orphan 'Show' instance covered by @text-show@ (except for the instance
in `Text.Show.Text.Functions`, which is not imported by default).
-}
module Text.Show.Text.Instances () where

import Text.Show.Text.Control.Applicative   ()
import Text.Show.Text.Control.Concurrent    ()
import Text.Show.Text.Control.Exception     ()
import Text.Show.Text.Control.Monad.ST      ()

import Text.Show.Text.Data.Array            ()
import Text.Show.Text.Data.Bool             ()
import Text.Show.Text.Data.ByteString       ()
import Text.Show.Text.Data.Char             ()
import Text.Show.Text.Data.Complex          ()
import Text.Show.Text.Data.Data             ()
import Text.Show.Text.Data.Dynamic          ()
import Text.Show.Text.Data.Either           ()
import Text.Show.Text.Data.Fixed            ()
import Text.Show.Text.Data.Floating         ()
import Text.Show.Text.Data.Functor.Identity ()
import Text.Show.Text.Data.Integral         ()
import Text.Show.Text.Data.List             ()
import Text.Show.Text.Data.Maybe            ()
import Text.Show.Text.Data.Monoid           ()
import Text.Show.Text.Data.OldTypeable      ()
import Text.Show.Text.Data.Ord              ()
import Text.Show.Text.Data.Proxy            ()
import Text.Show.Text.Data.Ratio            ()
import Text.Show.Text.Data.Text             ()
import Text.Show.Text.Data.Tuple            ()
import Text.Show.Text.Data.Type.Coercion    ()
import Text.Show.Text.Data.Type.Equality    ()
import Text.Show.Text.Data.Typeable         ()
import Text.Show.Text.Data.Version          ()
import Text.Show.Text.Data.Void             ()

import Text.Show.Text.Foreign.C.Types       ()
import Text.Show.Text.Foreign.Ptr           ()

import Text.Show.Text.GHC.Conc.Windows      ()
import Text.Show.Text.GHC.Event             ()
import Text.Show.Text.GHC.Fingerprint       ()
import Text.Show.Text.GHC.Generics          ()
import Text.Show.Text.GHC.RTS.Flags         ()
import Text.Show.Text.GHC.StaticPtr         ()
import Text.Show.Text.GHC.Stats             ()
import Text.Show.Text.GHC.TypeLits          ()

import Text.Show.Text.Numeric.Natural       ()

import Text.Show.Text.System.Exit           ()
import Text.Show.Text.System.IO             ()
import Text.Show.Text.System.Posix.Types    ()

import Text.Show.Text.Text.Read             ()
