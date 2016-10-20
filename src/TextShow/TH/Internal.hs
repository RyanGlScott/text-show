{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-|
Module:      TextShow.TH.Internal
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Functions to mechanically derive 'TextShow', 'TextShow1', or 'TextShow2' instances,
or to splice their functions directly into Haskell source code. You need to enable
the @TemplateHaskell@ language extension in order to use this module.

This implementation is loosely based off of the @Data.Aeson.TH@ module from the
@aeson@ library.
-}
module TextShow.TH.Internal (
      -- * 'deriveTextShow'
      -- $deriveTextShow
      deriveTextShow
      -- * 'deriveTextShow1'
      -- $deriveTextShow1
    , deriveTextShow1
      -- * 'deriveTextShow2'
      -- $deriveTextShow2
    , deriveTextShow2
      -- * @make-@ functions
      -- $make
    , makeShowt
    , makeShowtl
    , makeShowtPrec
    , makeShowtlPrec
    , makeShowtList
    , makeShowtlList
    , makeShowb
    , makeShowbPrec
    , makeShowbList
    , makePrintT
    , makePrintTL
    , makeHPrintT
    , makeHPrintTL
    , makeLiftShowbPrec
    , makeShowbPrec1
    , makeLiftShowbPrec2
    , makeShowbPrec2
    -- * 'Options'
    , Options(..)
    , defaultOptions
    , GenTextMethods(..)
    , deriveTextShowOptions
    , deriveTextShow1Options
    , deriveTextShow2Options
    ) where

import           Control.Monad (liftM, unless, when)
#if MIN_VERSION_template_haskell(2,11,0)
import           Control.Monad ((<=<))
#endif
import           Data.Foldable.Compat
import           Data.List.Compat
import qualified Data.List.NonEmpty as NE (drop, length, reverse, splitAt)
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.Map as Map (fromList, findWithDefault, keys, lookup, singleton)
import           Data.Map (Map)
import           Data.Maybe
import           Data.Monoid.Compat ((<>))
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Text    as TS
import qualified Data.Text.IO as TS (putStrLn, hPutStrLn)
import           Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.Builder as TB
import           Data.Text.Lazy.Builder (Builder, toLazyText)
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TL (putStrLn, hPutStrLn)

import           GHC.Exts (Char(..), Double(..), Float(..), Int(..), Word(..))
import           GHC.Prim (Char#, Double#, Float#, Int#, Word#)
import           GHC.Show (appPrec, appPrec1)

import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Ppr hiding (appPrec)
import           Language.Haskell.TH.Syntax

import           Prelude ()
import           Prelude.Compat

import           TextShow.Classes (TextShow(..), TextShow1(..), TextShow2(..),
                                   showbListWith, showbParen, showbSpace,
                                   showtParen, showtSpace, showtlParen, showtlSpace)
import           TextShow.Options (Options(..), GenTextMethods(..), defaultOptions)
import           TextShow.Utils (isInfixDataCon, isSymVar, isTupleString)

-------------------------------------------------------------------------------
-- User-facing API
-------------------------------------------------------------------------------

{- $deriveTextShow

'deriveTextShow' automatically generates a 'TextShow' instance declaration for a data
type, newtype, or data family instance. This emulates what would (hypothetically)
happen if you could attach a @deriving 'TextShow'@ clause to the end of a data
declaration.

Here are some examples of how to derive 'TextShow' for simple data types:

@
&#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
import TextShow.TH

data Letter = A | B | C
$('deriveTextShow' ''Letter) -- instance TextShow Letter where ...

newtype Box a = Box a
$('deriveTextShow' ''Box) -- instance TextShow a => TextShow (Box a) where ...
@

If you are using @template-haskell-2.7.0.0@ or later (i.e., GHC 7.4 or later),
'deriveTextShow' can also be used to derive 'TextShow' instances for data family
instances (which requires the @-XTypeFamilies@ extension). To do so, pass the name of
a data or newtype instance constructor (NOT a data family name!) to 'deriveTextShow'.
Note that the generated code may require the @-XFlexibleInstances@ extension.
Some examples:

@
&#123;-&#35; LANGUAGE FlexibleInstances, TemplateHaskell, TypeFamilies &#35;-&#125;
import TextShow.TH (deriveTextShow)

class AssocClass a where
    data AssocData a
instance AssocClass Int where
    data AssocData Int = AssocDataInt1 Int | AssocDataInt2 Int Int
$('deriveTextShow' 'AssocDataInt1) -- instance TextShow (AssocData Int) where ...
-- Alternatively, one could use $(deriveTextShow 'AssocDataInt2)

data family DataFam a b
newtype instance DataFam () b = DataFamB b
$('deriveTextShow' 'DataFamB) -- instance TextShow b => TextShow (DataFam () b)
@

Note that at the moment, there are some limitations:

* The 'Name' argument to 'deriveTextShow' must not be a type synonym.

* 'deriveTextShow' makes the assumption that all type variables of kind @*@ require a
  'TextShow' constraint when creating the type context. For example, if you have @data
  Phantom a = Phantom@, then @('deriveTextShow' ''Phantom)@ will generate @instance
  'TextShow' a => 'TextShow' (Phantom a) where ...@, even though @'TextShow' a@ is
  not required. If you want a proper 'TextShow' instance for @Phantom@, you will need
  to use 'makeShowbPrec' (see the documentation of the @make@ functions for more
  information).

* 'deriveTextShow' lacks the ability to properly detect data types with higher-kinded
   type parameters (e.g., @data HK f a = HK (f a)@) or with kinds other than @*@
   (e.g., @data List a (empty :: Bool)@). If you wish to derive 'TextShow'
   instances for these data types, you will need to use 'makeShowbPrec'.

* Some data constructors have arguments whose 'TextShow' instance depends on a
  typeclass besides 'TextShow'. For example, consider @newtype MyFixed a = MyFixed
  (Fixed a)@. @'Fixed' a@ is a 'TextShow' instance only if @a@ is an instance of both
  @HasResolution@ and 'TextShow'. Unfortunately, 'deriveTextShow' cannot infer that
  'a' must be an instance of 'HasResolution', so it cannot create a 'TextShow'
  instance for @MyFixed@. However, you can use 'makeShowbPrec' to get around this.

-}

-- | Generates a 'TextShow' instance declaration for the given data type or data
-- family instance.
--
-- /Since: 2/
deriveTextShow :: Name -> Q [Dec]
deriveTextShow = deriveTextShowOptions defaultOptions

-- | Like 'deriveTextShow', but takes an 'Options' argument.
--
-- /Since: 3.4/
deriveTextShowOptions :: Options -> Name -> Q [Dec]
deriveTextShowOptions = deriveTextShowClass TextShow

{- $deriveTextShow1

'deriveTextShow1' automatically generates a 'Show1' instance declaration for a data
type, newtype, or data family instance that has at least one type variable.
This emulates what would (hypothetically) happen if you could attach a @deriving
'TextShow1'@ clause to the end of a data declaration. Examples:

@
&#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
import TextShow.TH

data Stream a = Stream a (Stream a)
$('deriveTextShow1' ''Stream) -- instance TextShow1 TextStream where ...

newtype WrappedFunctor f a = WrapFunctor (f a)
$('deriveTextShow1' ''WrappedFunctor) -- instance TextShow1 f => TextShow1 (WrappedFunctor f) where ...
@

The same restrictions that apply to 'deriveTextShow' also apply to 'deriveTextShow1',
with some caveats:

* With 'deriveTextShow1', the last type variable must be of kind @*@. For other ones,
  type variables of kind @*@ are assumed to require a 'TextShow' context, and type
  variables of kind @* -> *@ are assumed to require a 'TextShow1' context. For more
  complicated scenarios, use 'makeLiftShowbPrec'.

* If using @-XDatatypeContexts@, a datatype constraint cannot mention the last type
  variable. For example, @data Ord a => Illegal a = Illegal a@ cannot have a derived
  'TextShow1' instance.

* If the last type variable is used within a data field of a constructor, it must only
  be used in the last argument of the data type constructor. For example, @data Legal a
  = Legal (Either Int a)@ can have a derived 'TextShow1' instance, but @data Illegal a
  = Illegal (Either a a)@ cannot.

* Data family instances must be able to eta-reduce the last type variable. In other
  words, if you have a instance of the form:

  @
  data family Family a1 ... an t
  data instance Family e1 ... e2 v = ...
  @

  Then the following conditions must hold:

  1. @v@ must be a type variable.
  2. @v@ must not be mentioned in any of @e1@, ..., @e2@.

-}

-- | Generates a 'TextShow1' instance declaration for the given data type or data
-- family instance.
--
-- /Since: 2/
deriveTextShow1 :: Name -> Q [Dec]
deriveTextShow1 = deriveTextShow1Options defaultOptions

-- | Like 'deriveTextShow1', but takes an 'Options' argument.
--
-- /Since: 3.4/
deriveTextShow1Options :: Options -> Name -> Q [Dec]
deriveTextShow1Options = deriveTextShowClass TextShow1

{- $deriveTextShow2

'deriveTextShow2' automatically generates a 'TextShow2' instance declaration for a data
type, newtype, or data family instance that has at least two type variables.
This emulates what would (hypothetically) happen if you could attach a @deriving
'TextShow2'@ clause to the end of a data declaration. Examples:

@
&#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
import TextShow.TH

data OneOrNone a b = OneL a | OneR b | None
$('deriveTextShow2' ''OneOrNone) -- instance TextShow2 OneOrNone where ...

newtype WrappedBifunctor f a b = WrapBifunctor (f a b)
$('deriveTextShow2' ''WrappedBifunctor) -- instance TextShow2 f => TextShow2 (WrappedBifunctor f) where ...
@

The same restrictions that apply to 'deriveTextShow' and 'deriveTextShow1' also apply
to 'deriveTextShow2', with some caveats:

* With 'deriveTextShow2', the last type variables must both be of kind @*@. For other
  ones, type variables of kind @*@ are assumed to require a 'TextShow' constraint, type
  variables of kind @* -> *@ are assumed to require a 'TextShow1' constraint, and type
  variables of kind @* -> * -> *@ are assumed to require a 'TextShow2' constraint. For
  more complicated scenarios, use 'makeLiftShowbPrec2'.

* If using @-XDatatypeContexts@, a datatype constraint cannot mention either of the last
  two type variables. For example, @data Ord a => Illegal a b = Illegal a b@ cannot
  have a derived 'TextShow2' instance.

* If either of the last two type variables is used within a data field of a constructor,
  it must only be used in the last two arguments of the data type constructor. For
  example, @data Legal a b = Legal (Int, Int, a, b)@ can have a derived 'TextShow2'
  instance, but @data Illegal a b = Illegal (a, b, a, b)@ cannot.

* Data family instances must be able to eta-reduce the last two type variables. In other
  words, if you have a instance of the form:

  @
  data family Family a1 ... an t1 t2
  data instance Family e1 ... e2 v1 v2 = ...
  @

  Then the following conditions must hold:

  1. @v1@ and @v2@ must be distinct type variables.
  2. Neither @v1@ not @v2@ must be mentioned in any of @e1@, ..., @e2@.

-}

-- | Generates a 'TextShow2' instance declaration for the given data type or data
-- family instance.
--
-- /Since: 2/
deriveTextShow2 :: Name -> Q [Dec]
deriveTextShow2 = deriveTextShow2Options defaultOptions

-- | Like 'deriveTextShow2', but takes an 'Options' argument.
--
-- /Since: 3.4/
deriveTextShow2Options :: Options -> Name -> Q [Dec]
deriveTextShow2Options = deriveTextShowClass TextShow2

{- $make

There may be scenarios in which you want to show an arbitrary data type or data
family instance without having to make the type an instance of 'TextShow'. For these
cases, this modules provides several functions (all prefixed with @make@-) that
splice the appropriate lambda expression into your source code. Example:

This is particularly useful for creating instances for sophisticated data types. For
example, 'deriveTextShow' cannot infer the correct type context for
@newtype HigherKinded f a = HigherKinded (f a)@, since @f@ is of kind @* -> *@.
However, it is still possible to derive a 'TextShow' instance for @HigherKinded@
without too much trouble using 'makeShowbPrec':

@
&#123;-&#35; LANGUAGE FlexibleContexts, TemplateHaskell &#35;-&#125;
import TextShow
import TextShow.TH

instance TextShow (f a) => TextShow (HigherKinded f a) where
    showbPrec = $(makeShowbPrec ''HigherKinded)
@

-}

-- | Generates a lambda expression which behaves like 'showt' (without requiring a
-- 'TextShow' instance).
--
-- /Since: 2/
makeShowt :: Name -> Q Exp
makeShowt name = makeShowtPrec name `appE` integerE 0

-- | Generates a lambda expression which behaves like 'showtl' (without requiring a
-- 'TextShow' instance).
--
-- /Since: 2/
makeShowtl :: Name -> Q Exp
makeShowtl name = makeShowtlPrec name `appE` integerE 0

-- | Generates a lambda expression which behaves like 'showtPrec' (without requiring a
-- 'TextShow' instance).
--
-- /Since: 2/
makeShowtPrec :: Name -> Q Exp
makeShowtPrec = makeShowbPrecClass TextShow ShowtPrec

-- | Generates a lambda expression which behaves like 'showtlPrec' (without
-- requiring a 'TextShow' instance).
--
-- /Since: 2/
makeShowtlPrec :: Name -> Q Exp
makeShowtlPrec = makeShowbPrecClass TextShow ShowtlPrec

-- | Generates a lambda expression which behaves like 'showtList' (without requiring a
-- 'TextShow' instance).
--
-- /Since: 2/
makeShowtList :: Name -> Q Exp
makeShowtList name = [| toStrict . $(makeShowtlList name) |]

-- | Generates a lambda expression which behaves like 'showtlList' (without
-- requiring a 'TextShow' instance).
--
-- /Since: 2/
makeShowtlList :: Name -> Q Exp
makeShowtlList name = [| toLazyText . $(makeShowbList name) |]

-- | Generates a lambda expression which behaves like 'showb' (without requiring a
-- 'TextShow' instance).
--
-- /Since: 2/
makeShowb :: Name -> Q Exp
makeShowb name = makeShowbPrec name `appE` integerE 0

-- | Generates a lambda expression which behaves like 'showbPrec' (without requiring a
-- 'TextShow' instance).
--
-- /Since: 2/
makeShowbPrec :: Name -> Q Exp
makeShowbPrec = makeShowbPrecClass TextShow ShowbPrec

-- | Generates a lambda expression which behaves like 'liftShowbPrec' (without
-- requiring a 'TextShow1' instance).
--
-- /Since: 3/
makeLiftShowbPrec :: Name -> Q Exp
makeLiftShowbPrec = makeShowbPrecClass TextShow1 ShowbPrec

-- | Generates a lambda expression which behaves like 'showbPrec1' (without
-- requiring a 'TextShow1' instance).
--
-- /Since: 2/
makeShowbPrec1 :: Name -> Q Exp
makeShowbPrec1 name = [| $(makeLiftShowbPrec name) showbPrec showbList |]

-- | Generates a lambda expression which behaves like 'liftShowbPrec2' (without
-- requiring a 'TextShow2' instance).
--
-- /Since: 3/
makeLiftShowbPrec2 :: Name -> Q Exp
makeLiftShowbPrec2 = makeShowbPrecClass TextShow2 ShowbPrec

-- | Generates a lambda expression which behaves like 'showbPrec2' (without
-- requiring a 'TextShow2' instance).
--
-- /Since: 2/
makeShowbPrec2 :: Name -> Q Exp
makeShowbPrec2 name = [| $(makeLiftShowbPrec2 name) showbPrec showbList showbPrec showbList |]

-- | Generates a lambda expression which behaves like 'showbList' (without requiring a
-- 'TextShow' instance).
--
-- /Since: 2/
makeShowbList :: Name -> Q Exp
makeShowbList name = [| showbListWith $(makeShowb name) |]

-- | Generates a lambda expression which behaves like 'printT' (without requiring a
-- 'TextShow' instance).
--
-- /Since: 2/
makePrintT :: Name -> Q Exp
makePrintT name = [| TS.putStrLn . $(makeShowt name) |]

-- | Generates a lambda expression which behaves like 'printTL' (without requiring a
-- 'TextShow' instance).
--
-- /Since: 2/
makePrintTL :: Name -> Q Exp
makePrintTL name = [| TL.putStrLn . $(makeShowtl name) |]

-- | Generates a lambda expression which behaves like 'hPrintT' (without requiring a
-- 'TextShow' instance).
--
-- /Since: 2/
makeHPrintT :: Name -> Q Exp
makeHPrintT name = [| \h -> TS.hPutStrLn h . $(makeShowt name) |]

-- | Generates a lambda expression which behaves like 'hPrintTL' (without
-- requiring a 'TextShow' instance).
--
-- /Since: 2/
makeHPrintTL :: Name -> Q Exp
makeHPrintTL name = [| \h -> TL.hPutStrLn h . $(makeShowtl name) |]

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

-- | Derive a TextShow(1)(2) instance declaration (depending on the TextShowClass
-- argument's value).
deriveTextShowClass :: TextShowClass -> Options -> Name -> Q [Dec]
deriveTextShowClass tsClass opts name = withType name fromCons
  where
    fromCons :: Name -> Cxt -> [TyVarBndr] -> [Con] -> Maybe [Type] -> Q [Dec]
    fromCons name' ctxt tvbs cons mbTys = (:[]) <$> do
        (instanceCxt, instanceType)
            <- buildTypeInstance tsClass name' ctxt tvbs mbTys
        instanceD (return instanceCxt)
                  (return instanceType)
                  (showbPrecDecs tsClass opts cons)

-- | Generates a declaration defining the primary function corresponding to a
-- particular class (showbPrec for TextShow, liftShowbPrec for TextShow1, and
-- liftShowbPrec2 for TextShow2).
showbPrecDecs :: TextShowClass -> Options -> [Con] -> [Q Dec]
showbPrecDecs tsClass opts cons =
    [genMethod ShowbPrec (showbPrecName tsClass)]
    ++ if tsClass == TextShow && shouldGenTextMethods
          then [genMethod ShowtPrec 'showtPrec, genMethod ShowtlPrec 'showtlPrec]
          else []
  where
    shouldGenTextMethods :: Bool
    shouldGenTextMethods = case genTextMethods opts of
      AlwaysTextMethods    -> True
      SometimesTextMethods -> all isNullaryCon cons
      NeverTextMethods     -> False

    genMethod :: TextShowFun -> Name -> Q Dec
    genMethod method methodName
      = funD methodName
             [ clause []
                      (normalB $ makeTextShowForCons tsClass method cons)
                      []
             ]


-- | Generates a lambda expression which behaves like showbPrec (for TextShow),
-- liftShowbPrec (for TextShow1), or liftShowbPrec2 (for TextShow2).
makeShowbPrecClass :: TextShowClass -> TextShowFun -> Name -> Q Exp
makeShowbPrecClass tsClass tsFun name = withType name fromCons
  where
    fromCons :: Name -> Cxt -> [TyVarBndr] -> [Con] -> Maybe [Type] -> Q Exp
    fromCons name' ctxt tvbs cons mbTys =
        -- We force buildTypeInstance here since it performs some checks for whether
        -- or not the provided datatype can actually have showbPrec/liftShowbPrec/etc.
        -- implemented for it, and produces errors if it can't.
        buildTypeInstance tsClass name' ctxt tvbs mbTys
          `seq` makeTextShowForCons tsClass tsFun cons

-- | Generates a lambda expression for showbPrec/liftShowbPrec/etc. for the
-- given constructors. All constructors must be from the same type.
makeTextShowForCons :: TextShowClass -> TextShowFun -> [Con] -> Q Exp
makeTextShowForCons _ _ [] = error "Must have at least one data constructor"
makeTextShowForCons tsClass tsFun cons = do
    p       <- newName "p"
    value   <- newName "value"
    sps     <- newNameList "sp" $ fromEnum tsClass
    sls     <- newNameList "sl" $ fromEnum tsClass
    let spls      = zip sps sls
        spsAndSls = interleave sps sls
    matches <- concatMapM (makeTextShowForCon p tsClass tsFun spls) cons
    lamE (map varP $ spsAndSls ++ [p, value])
        . appsE
        $ [ varE $ showPrecConstName tsClass tsFun
          , caseE (varE value) (map return matches)
          ] ++ map varE spsAndSls
            ++ [varE p, varE value]

-- | Generates a lambda expression for howbPrec/liftShowbPrec/etc. for a
-- single constructor.
makeTextShowForCon :: Name
                   -> TextShowClass
                   -> TextShowFun
                   -> [(Name, Name)]
                   -> Con
                   -> Q [Match]
makeTextShowForCon _ _ tsFun _ (NormalC conName []) = do
    m <- match
           (conP conName [])
           (normalB  $ varE (fromStringName tsFun) `appE` stringE (parenInfixConName conName ""))
           []
    return [m]
makeTextShowForCon p tsClass tsFun spls (NormalC conName [_]) = do
    ([argTy], tvMap) <- reifyConTys tsClass spls conName
    arg <- newName "arg"

    let showArg  = makeTextShowForArg appPrec1 tsClass tsFun conName tvMap argTy arg
        namedArg = infixApp (varE (fromStringName tsFun) `appE` stringE (parenInfixConName conName " "))
                            [| (<>) |]
                            showArg

    m <- match
           (conP conName [varP arg])
           (normalB $ varE (showParenName tsFun)
                       `appE` infixApp (varE p) [| (>) |] (integerE appPrec)
                       `appE` namedArg)
           []
    return [m]
makeTextShowForCon p tsClass tsFun spls (NormalC conName _) = do
    (argTys, tvMap) <- reifyConTys tsClass spls conName
    args <- newNameList "arg" $ length argTys

    m <- if isNonUnitTuple conName
         then do
           let showArgs       = zipWith (makeTextShowForArg 0 tsClass tsFun conName tvMap) argTys args
               parenCommaArgs = (varE (singletonName tsFun) `appE` charE '(')
                                : intersperse (varE (singletonName tsFun) `appE` charE ',') showArgs
               mappendArgs    = foldr' (`infixApp` [| (<>) |])
                                       (varE (singletonName tsFun) `appE` charE ')')
                                       parenCommaArgs

           match (conP conName $ map varP args)
                 (normalB mappendArgs)
                 []
         else do
           let showArgs    = zipWith (makeTextShowForArg appPrec1 tsClass tsFun conName tvMap) argTys args
               mappendArgs = foldr1 (\v q -> infixApp v
                                                      [| (<>) |]
                                                      (infixApp (varE $ showSpaceName tsFun)
                                                                [| (<>) |]
                                                                q)) showArgs
               namedArgs   = infixApp (varE (fromStringName tsFun) `appE` stringE (parenInfixConName conName " "))
                                      [| (<>) |]
                                      mappendArgs

           match (conP conName $ map varP args)
                 (normalB $ varE (showParenName tsFun)
                              `appE` infixApp (varE p) [| (>) |] (integerE appPrec)
                              `appE` namedArgs)
                 []
    return [m]
makeTextShowForCon p tsClass tsFun spls (RecC conName []) =
    makeTextShowForCon p tsClass tsFun spls $ NormalC conName []
makeTextShowForCon p tsClass tsFun spls (RecC conName ts) = do
    (argTys, tvMap) <- reifyConTys tsClass spls conName
    args <- newNameList "arg" $ length argTys

    let showArgs       = concatMap (\((argName, _, _), argTy, arg)
                                      -> let argNameBase = nameBase argName
                                             infixRec    = showParen (isSymVar argNameBase)
                                                                     (showString argNameBase) ""
                                         in [ varE (fromStringName tsFun) `appE` stringE (infixRec ++ " = ")
                                            , makeTextShowForArg 0 tsClass tsFun conName tvMap argTy arg
                                            , varE (fromStringName tsFun) `appE` stringE ", "
                                            ]
                                   )
                                   (zip3 ts argTys args)
        braceCommaArgs = (varE (singletonName tsFun) `appE` charE '{') : take (length showArgs - 1) showArgs
        mappendArgs    = foldr' (`infixApp` [| (<>) |])
                                (varE (singletonName tsFun) `appE` charE '}')
                                braceCommaArgs
        namedArgs      = infixApp (varE (fromStringName tsFun) `appE` stringE (parenInfixConName conName " "))
                                  [| (<>) |]
                                  mappendArgs

    m <- match
           (conP conName $ map varP args)
           (normalB $ varE (showParenName tsFun)
                        `appE` infixApp (varE p) [| (>) |] (integerE appPrec)
                        `appE` namedArgs)
           []
    return [m]
makeTextShowForCon p tsClass tsFun spls (InfixC _ conName _) = do
    ([alTy, arTy], tvMap) <- reifyConTys tsClass spls conName
    al   <- newName "argL"
    ar   <- newName "argR"
    info <- reify conName

#if MIN_VERSION_template_haskell(2,11,0)
    conPrec <- case info of
                        DataConI{} -> do
                            fi <- fromMaybe defaultFixity <$> reifyFixity conName
                            case fi of
                                 Fixity prec _ -> return prec
#else
    let conPrec  = case info of
                        DataConI _ _ _ (Fixity prec _) -> prec
#endif
                        _ -> error $ "TextShow.TH.makeTextShowForCon: Unsupported type: " ++ show info

    let opName   = nameBase conName
        infixOpE = appE (varE $ fromStringName tsFun) . stringE $
                     if isInfixDataCon opName
                        then " "  ++ opName ++ " "
                        else " `" ++ opName ++ "` "

    m <- match
           (infixP (varP al) conName (varP ar))
           (normalB $ (varE (showParenName tsFun) `appE` infixApp (varE p) [| (>) |] (integerE conPrec))
                        `appE` (infixApp (makeTextShowForArg (conPrec + 1) tsClass tsFun conName tvMap alTy al)
                                         [| (<>) |]
                                         (infixApp infixOpE
                                                   [| (<>) |]
                                                   (makeTextShowForArg (conPrec + 1) tsClass tsFun conName tvMap arTy ar)))
           )
           []
    return [m]
makeTextShowForCon p tsClass tsFun spls (ForallC _ _ con) =
    makeTextShowForCon p tsClass tsFun spls con
#if MIN_VERSION_template_haskell(2,11,0)
makeTextShowForCon p tsClass tsFun spls (GadtC conNames ts _) =
    let con :: Name -> Q Con
        con conName = do
            mbFi <- reifyFixity conName
            return $ if isInfixDataCon (nameBase conName)
                        && length ts == 2
                        && isJust mbFi
                      then let [t1, t2] = ts in InfixC t1 conName t2
                      else NormalC conName ts

    in concatMapM (makeTextShowForCon p tsClass tsFun spls <=< con) conNames
makeTextShowForCon p tsClass tsFun spls (RecGadtC conNames ts _) =
    concatMapM (makeTextShowForCon p tsClass tsFun spls . flip RecC ts) conNames
#endif

-- | Generates a lambda expression for howbPrec/liftShowbPrec/etc. for an
-- argument of a constructor.
makeTextShowForArg :: Int
                   -> TextShowClass
                   -> TextShowFun
                   -> Name
                   -> TyVarMap
                   -> Type
                   -> Name
                   -> Q Exp
makeTextShowForArg p _ tsFun _ _ (ConT tyName) tyExpName =
    showE
  where
    tyVarE, showPrecE :: Q Exp
    tyVarE    = varE tyExpName
    showPrecE = varE (showPrecName TextShow tsFun)

    showE :: Q Exp
    showE | tyName == ''Char#   = showPrimE 'C# oneHashE
          | tyName == ''Double# = showPrimE 'D# twoHashE
          | tyName == ''Float#  = showPrimE 'F# oneHashE
          | tyName == ''Int#    = showPrimE 'I# oneHashE
          | tyName == ''Word#   = showPrimE 'W# twoHashE
          | otherwise = showPrecE `appE` integerE p `appE` tyVarE

    -- Starting with GHC 7.10, data types containing unlifted types with derived Show
    -- instances show hashed literals with actual hash signs, and negative hashed
    -- literals are not surrounded with parentheses.
    showPrimE :: Name -> Q Exp -> Q Exp
    showPrimE con _hashE
#if __GLASGOW_HASKELL__ >= 711
      = infixApp (showPrecE `appE` integerE 0 `appE` (conE con `appE` tyVarE))
                 [| (<>) |]
                 _hashE
#else
      = showPrecE `appE` integerE p `appE` (conE con `appE` tyVarE)
#endif

    oneHashE, twoHashE :: Q Exp
    oneHashE = varE (singletonName  tsFun) `appE` charE   '#'
    twoHashE = varE (fromStringName tsFun) `appE` stringE "##"
makeTextShowForArg p tsClass tsFun conName tvMap ty tyExpName =
    [| $(makeTextShowForType tsClass tsFun conName tvMap False ty) p $(varE tyExpName) |]

-- | Generates a lambda expression for howbPrec/liftShowbPrec/etc. for a
-- specific type. The generated expression depends on the number of type variables.
--
-- 1. If the type is of kind * (T), apply showbPrec.
-- 2. If the type is of kind * -> * (T a), apply liftShowbPrec $(makeTextShowForType a)
-- 3. If the type is of kind * -> * -> * (T a b), apply
--    liftShowbPrec2 $(makeTextShowForType a) $(makeTextShowForType b)
makeTextShowForType :: TextShowClass
                    -> TextShowFun
                    -> Name
                    -> TyVarMap
                    -> Bool -- ^ True if we are using the function of type ([a] -> Builder),
                            --   False if we are using the function of type (Int -> a -> Builder).
                    -> Type
                    -> Q Exp
makeTextShowForType _ tsFun _ tvMap sl (VarT tyName) =
    varE $ case Map.lookup tyName tvMap of
         Just (spExp, slExp) -> if sl then slExp else spExp
         Nothing             -> if sl then showListName TextShow tsFun
                                      else showPrecName TextShow tsFun
makeTextShowForType tsClass tsFun conName tvMap sl (SigT ty _) =
    makeTextShowForType tsClass tsFun conName tvMap sl ty
makeTextShowForType tsClass tsFun conName tvMap sl (ForallT _ _ ty) =
    makeTextShowForType tsClass tsFun conName tvMap sl ty
makeTextShowForType tsClass tsFun conName tvMap sl ty = do
    let tyCon :: Type
        tyArgs :: [Type]
        tyCon :| tyArgs = unapplyTy ty

        numLastArgs :: Int
        numLastArgs = min (fromEnum tsClass) (length tyArgs)

        lhsArgs, rhsArgs :: [Type]
        (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

        tyVarNames :: [Name]
        tyVarNames = Map.keys tvMap

    itf <- isTyFamily tyCon
    if any (`mentionsName` tyVarNames) lhsArgs
          || itf && any (`mentionsName` tyVarNames) tyArgs
       then outOfPlaceTyVarError tsClass conName
       else if any (`mentionsName` tyVarNames) rhsArgs
               then appsE $ [ varE $ showPrecOrListName sl (toEnum numLastArgs) tsFun]
                            ++ zipWith (makeTextShowForType tsClass tsFun conName tvMap)
                                       (cycle [False,True])
                                       (interleave rhsArgs rhsArgs)
               else varE $ if sl then showListName TextShow tsFun
                                 else showPrecName TextShow tsFun

-------------------------------------------------------------------------------
-- Template Haskell reifying and AST manipulation
-------------------------------------------------------------------------------

-- | Extracts a plain type constructor's information.
-- | Boilerplate for top level splices.
--
-- The given Name must meet one of two criteria:
--
-- 1. It must be the name of a type constructor of a plain data type or newtype.
-- 2. It must be the name of a data family instance or newtype instance constructor.
--
-- Any other value will result in an exception.
withType :: Name
         -> (Name -> Cxt -> [TyVarBndr] -> [Con] -> Maybe [Type] -> Q a)
         -> Q a
withType name f = do
  info <- reify name
  case info of
    TyConI dec ->
      case dec of
        DataD ctxt _ tvbs
#if MIN_VERSION_template_haskell(2,11,0)
              _
#endif
              cons _ -> f name ctxt tvbs cons Nothing
        NewtypeD ctxt _ tvbs
#if MIN_VERSION_template_haskell(2,11,0)
                 _
#endif
                 con _ -> f name ctxt tvbs [con] Nothing
        _ -> error $ ns ++ "Unsupported type: " ++ show dec
#if MIN_VERSION_template_haskell(2,7,0)
# if MIN_VERSION_template_haskell(2,11,0)
    DataConI _ _ parentName   -> do
# else
    DataConI _ _ parentName _ -> do
# endif
      parentInfo <- reify parentName
      case parentInfo of
# if MIN_VERSION_template_haskell(2,11,0)
        FamilyI (DataFamilyD _ tvbs _) decs ->
# else
        FamilyI (FamilyD DataFam _ tvbs _) decs ->
# endif
          let instDec = flip find decs $ \dec -> case dec of
                DataInstD _ _ _
# if MIN_VERSION_template_haskell(2,11,0)
                          _
# endif
                          cons _ -> any ((name ==) . constructorName) cons
                NewtypeInstD _ _ _
# if MIN_VERSION_template_haskell(2,11,0)
                             _
# endif
                             con _ -> name == constructorName con
                _ -> error $ ns ++ "Must be a data or newtype instance."
           in case instDec of
                Just (DataInstD ctxt _ instTys
# if MIN_VERSION_template_haskell(2,11,0)
                                _
# endif
                                cons _)
                  -> f parentName ctxt tvbs cons $ Just instTys
                Just (NewtypeInstD ctxt _ instTys
# if MIN_VERSION_template_haskell(2,11,0)
                                   _
# endif
                                   con _)
                  -> f parentName ctxt tvbs [con] $ Just instTys
                _ -> error $ ns ++
                  "Could not find data or newtype instance constructor."
        _ -> error $ ns ++ "Data constructor " ++ show name ++
          " is not from a data family instance constructor."
# if MIN_VERSION_template_haskell(2,11,0)
    FamilyI DataFamilyD{} _ ->
# else
    FamilyI (FamilyD DataFam _ _ _) _ ->
# endif
      error $ ns ++
        "Cannot use a data family name. Use a data family instance constructor instead."
    _ -> error $ ns ++ "The name must be of a plain data type constructor, "
                    ++ "or a data family instance constructor."
#else
    DataConI{} -> dataConIError
    _          -> error $ ns ++ "The name must be of a plain type constructor."
#endif
  where
    ns :: String
    ns = "TextShow.TH.withType: "

-- | Deduces the instance context and head for an instance.
buildTypeInstance :: TextShowClass
                  -- ^ TextShow, TextShow1, or TextShow2
                  -> Name
                  -- ^ The type constructor or data family name
                  -> Cxt
                  -- ^ The datatype context
                  -> [TyVarBndr]
                  -- ^ The type variables from the data type/data family declaration
                  -> Maybe [Type]
                  -- ^ 'Just' the types used to instantiate a data family instance,
                  -- or 'Nothing' if it's a plain data type
                  -> Q (Cxt, Type)
-- Plain data type/newtype case
buildTypeInstance tsClass tyConName dataCxt tvbs Nothing =
    let varTys :: [Type]
        varTys = map tvbToType tvbs
    in buildTypeInstanceFromTys tsClass tyConName dataCxt varTys False
-- Data family instance case
--
-- The CPP is present to work around a couple of annoying old GHC bugs.
-- See Note [Polykinded data families in Template Haskell]
buildTypeInstance tsClass parentName dataCxt tvbs (Just instTysAndKinds) = do
#if !(MIN_VERSION_template_haskell(2,8,0)) || MIN_VERSION_template_haskell(2,10,0)
    let instTys :: [Type]
        instTys = zipWith stealKindForType tvbs instTysAndKinds
#else
    let kindVarNames :: [Name]
        kindVarNames = nub $ concatMap (tyVarNamesOfType . tvbKind) tvbs

        numKindVars :: Int
        numKindVars = length kindVarNames

        givenKinds, givenKinds' :: [Kind]
        givenTys                :: [Type]
        (givenKinds, givenTys) = splitAt numKindVars instTysAndKinds
        givenKinds' = map sanitizeStars givenKinds

        -- A GHC 7.6-specific bug requires us to replace all occurrences of
        -- (ConT GHC.Prim.*) with StarT, or else Template Haskell will reject it.
        -- Luckily, (ConT GHC.Prim.*) only seems to occur in this one spot.
        sanitizeStars :: Kind -> Kind
        sanitizeStars = go
          where
            go :: Kind -> Kind
            go (AppT t1 t2)                 = AppT (go t1) (go t2)
            go (SigT t k)                   = SigT (go t) (go k)
            go (ConT n) | n == starKindName = StarT
            go t                            = t

            -- It's quite awkward to import * from GHC.Prim, so we'll just
            -- hack our way around it.
            starKindName :: Name
            starKindName = mkNameG_tc "ghc-prim" "GHC.Prim" "*"

    -- If we run this code with GHC 7.8, we might have to generate extra type
    -- variables to compensate for any type variables that Template Haskell
    -- eta-reduced away.
    -- See Note [Polykinded data families in Template Haskell]
    xTypeNames <- newNameList "tExtra" (length tvbs - length givenTys)

    let xTys   :: [Type]
        xTys = map VarT xTypeNames
        -- ^ Because these type variables were eta-reduced away, we can only
        --   determine their kind by using stealKindForType. Therefore, we mark
        --   them as VarT to ensure they will be given an explicit kind annotation
        --   (and so the kind inference machinery has the right information).

        substNamesWithKinds :: [(Name, Kind)] -> Type -> Type
        substNamesWithKinds nks t = foldr' (uncurry substNameWithKind) t nks

        -- The types from the data family instance might not have explicit kind
        -- annotations, which the kind machinery needs to work correctly. To
        -- compensate, we use stealKindForType to explicitly annotate any
        -- types without kind annotations.
        instTys :: [Type]
        instTys = map (substNamesWithKinds (zip kindVarNames givenKinds'))
                  -- ^ Note that due to a GHC 7.8-specific bug
                  --   (see Note [Polykinded data families in Template Haskell]),
                  --   there may be more kind variable names than there are kinds
                  --   to substitute. But this is OK! If a kind is eta-reduced, it
                  --   means that is was not instantiated to something more specific,
                  --   so we need not substitute it. Using stealKindForType will
                  --   grab the correct kind.
                $ zipWith stealKindForType tvbs (givenTys ++ xTys)
#endif
    buildTypeInstanceFromTys tsClass parentName dataCxt instTys True

-- For the given Types, generate an instance context and head. Coming up with
-- the instance type isn't as simple as dropping the last types, as you need to
-- be wary of kinds being instantiated with *.
-- See Note [Type inference in derived instances]
buildTypeInstanceFromTys :: TextShowClass
                         -- ^ TextShow, TextShow1, or TextShow2
                         -> Name
                         -- ^ The type constructor or data family name
                         -> Cxt
                         -- ^ The datatype context
                         -> [Type]
                         -- ^ The types to instantiate the instance with
                         -> Bool
                         -- ^ True if it's a data family, False otherwise
                         -> Q (Cxt, Type)
buildTypeInstanceFromTys tsClass tyConName dataCxt varTysOrig isDataFamily = do
    -- Make sure to expand through type/kind synonyms! Otherwise, the
    -- eta-reduction check might get tripped up over type variables in a
    -- synonym that are actually dropped.
    -- (See GHC Trac #11416 for a scenario where this actually happened.)
    varTysExp <- mapM expandSyn varTysOrig

    let remainingLength :: Int
        remainingLength = length varTysOrig - fromEnum tsClass

        droppedTysExp :: [Type]
        droppedTysExp = drop remainingLength varTysExp

        droppedStarKindStati :: [StarKindStatus]
        droppedStarKindStati = map canRealizeKindStar droppedTysExp

    -- Check there are enough types to drop and that all of them are either of
    -- kind * or kind k (for some kind variable k). If not, throw an error.
    when (remainingLength < 0 || any (== NotKindStar) droppedStarKindStati) $
      derivingKindError tsClass tyConName

    let droppedKindVarNames :: [Name]
        droppedKindVarNames = catKindVarNames droppedStarKindStati

        -- Substitute kind * for any dropped kind variables
        varTysExpSubst :: [Type]
        varTysExpSubst = map (substNamesWithKindStar droppedKindVarNames) varTysExp

        remainingTysExpSubst, droppedTysExpSubst :: [Type]
        (remainingTysExpSubst, droppedTysExpSubst) =
          splitAt remainingLength varTysExpSubst

        -- All of the type variables mentioned in the dropped types
        -- (post-synonym expansion)
        droppedTyVarNames :: [Name]
        droppedTyVarNames = concatMap tyVarNamesOfType droppedTysExpSubst

    -- If any of the dropped types were polykinded, ensure that they are of kind *
    -- after substituting * for the dropped kind variables. If not, throw an error.
    unless (all hasKindStar droppedTysExpSubst) $
      derivingKindError tsClass tyConName

    let preds    :: [Maybe Pred]
        kvNames  :: [[Name]]
        kvNames' :: [Name]
        -- Derive instance constraints (and any kind variables which are specialized
        -- to * in those constraints)
        (preds, kvNames) = unzip $ map (deriveConstraint tsClass) remainingTysExpSubst
        kvNames' = concat kvNames

        -- Substitute the kind variables specialized in the constraints with *
        remainingTysExpSubst' :: [Type]
        remainingTysExpSubst' =
          map (substNamesWithKindStar kvNames') remainingTysExpSubst

        -- We now substitute all of the specialized-to-* kind variable names with
        -- *, but in the original types, not the synonym-expanded types. The reason
        -- we do this is a superficial one: we want the derived instance to resemble
        -- the datatype written in source code as closely as possible. For example,
        -- for the following data family instance:
        --
        --   data family Fam a
        --   newtype instance Fam String = Fam String
        --
        -- We'd want to generate the instance:
        --
        --   instance C (Fam String)
        --
        -- Not:
        --
        --   instance C (Fam [Char])
        remainingTysOrigSubst :: [Type]
        remainingTysOrigSubst =
          map (substNamesWithKindStar (union droppedKindVarNames kvNames'))
            $ take remainingLength varTysOrig

        remainingTysOrigSubst' :: [Type]
        -- See Note [Kind signatures in derived instances] for an explanation
        -- of the isDataFamily check.
        remainingTysOrigSubst' =
          if isDataFamily
             then remainingTysOrigSubst
             else map unSigT remainingTysOrigSubst

        instanceCxt :: Cxt
        instanceCxt = catMaybes preds

        instanceType :: Type
        instanceType = AppT (ConT $ textShowClassName tsClass)
                     $ applyTyCon tyConName remainingTysOrigSubst'

    -- If the datatype context mentions any of the dropped type variables,
    -- we can't derive an instance, so throw an error.
    when (any (`predMentionsName` droppedTyVarNames) dataCxt) $
      datatypeContextError tyConName instanceType
    -- Also ensure the dropped types can be safely eta-reduced. Otherwise,
    -- throw an error.
    unless (canEtaReduce remainingTysExpSubst' droppedTysExpSubst) $
      etaReductionError instanceType
    return (instanceCxt, instanceType)

-- | Attempt to derive a constraint on a Type. If successful, return
-- Just the constraint and any kind variable names constrained to *.
-- Otherwise, return Nothing and the empty list.
--
-- See Note [Type inference in derived instances] for the heuristics used to
-- come up with constraints.
deriveConstraint :: TextShowClass -> Type -> (Maybe Pred, [Name])
deriveConstraint tsClass t
  | not (isTyVar t) = (Nothing, [])
  | hasKindStar t   = (Just (applyClass ''TextShow tName), [])
  | otherwise = case hasKindVarChain 1 t of
      Just ns | tsClass >= TextShow1
              -> (Just (applyClass ''TextShow1 tName), ns)
      _ -> case hasKindVarChain 2 t of
           Just ns | tsClass == TextShow2
                   -> (Just (applyClass ''TextShow2 tName), ns)
           _ -> (Nothing, [])
  where
    tName :: Name
    tName = varTToName t

{-
Note [Polykinded data families in Template Haskell]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to come up with the correct instance context and head for an instance, e.g.,

  instance C a => C (Data a) where ...

We need to know the exact types and kinds used to instantiate the instance. For
plain old datatypes, this is simple: every type must be a type variable, and
Template Haskell reliably tells us the type variables and their kinds.

Doing the same for data families proves to be much harder for three reasons:

1. On any version of Template Haskell, it may not tell you what an instantiated
   type's kind is. For instance, in the following data family instance:

     data family Fam (f :: * -> *) (a :: *)
     data instance Fam f a

   Then if we use TH's reify function, it would tell us the TyVarBndrs of the
   data family declaration are:

     [KindedTV f (AppT (AppT ArrowT StarT) StarT),KindedTV a StarT]

   and the instantiated types of the data family instance are:

     [VarT f1,VarT a1]

   We can't just pass [VarT f1,VarT a1] to buildTypeInstanceFromTys, since we
   have no way of knowing their kinds. Luckily, the TyVarBndrs tell us what the
   kind is in case an instantiated type isn't a SigT, so we use the stealKindForType
   function to ensure all of the instantiated types are SigTs before passing them
   to buildTypeInstanceFromTys.
2. On GHC 7.6 and 7.8, a bug is present in which Template Haskell lists all of
   the specified kinds of a data family instance efore any of the instantiated
   types. Fortunately, this is easy to deal with: you simply count the number of
   distinct kind variables in the data family declaration, take that many elements
   from the front of the  Types list of the data family instance, substitute the
   kind variables with their respective instantiated kinds (which you took earlier),
   and proceed as normal.
3. On GHC 7.8, an even uglier bug is present (GHC Trac #9692) in which Template
   Haskell might not even list all of the Types of a data family instance, since
   they are eta-reduced away! And yes, kinds can be eta-reduced too.

   The simplest workaround is to count how many instantiated types are missing from
   the list and generate extra type variables to use in their place. Luckily, we
   needn't worry much if its kind was eta-reduced away, since using stealKindForType
   will get it back.

Note [Kind signatures in derived instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is possible to put explicit kind signatures into the derived instances, e.g.,

  instance C a => C (Data (f :: * -> *)) where ...

But it is preferable to avoid this if possible. If we come up with an incorrect
kind signature (which is entirely possible, since our type inferencer is pretty
unsophisticated - see Note [Type inference in derived instances]), then GHC will
flat-out reject the instance, which is quite unfortunate.

Plain old datatypes have the advantage that you can avoid using any kind signatures
at all in their instances. This is because a datatype declaration uses all type
variables, so the types that we use in a derived instance uniquely determine their
kinds. As long as we plug in the right types, the kind inferencer can do the rest
of the work. For this reason, we use unSigT to remove all kind signatures before
splicing in the instance context and head.

Data family instances are trickier, since a data family can have two instances that
are distinguished by kind alone, e.g.,

  data family Fam (a :: k)
  data instance Fam (a :: * -> *)
  data instance Fam (a :: *)

If we dropped the kind signatures for C (Fam a), then GHC will have no way of
knowing which instance we are talking about. To avoid this scenario, we always
include explicit kind signatures in data family instances. There is a chance that
the inferred kind signatures will be incorrect, but if so, we can always fall back
on the make- functions.

Note [Type inference in derived instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Type inference is can be tricky to get right, and we want to avoid recreating the
entirety of GHC's type inferencer in Template Haskell. For this reason, we will
probably never come up with derived instance contexts that are as accurate as
GHC's. But that doesn't mean we can't do anything! There are a couple of simple
things we can do to make instance contexts that work for 80% of use cases:

1. If one of the last type parameters is polykinded, then its kind will be
   specialized to * in the derived instance. We note what kind variable the type
   parameter had and substitute it with * in the other types as well. For example,
   imagine you had

     data Data (a :: k) (b :: k)

   Then you'd want to derived instance to be:

     instance C (Data (a :: *))

   Not:

     instance C (Data (a :: k))

2. We naïvely come up with instance constraints using the following criteria:

   (i)   If there's a type parameter n of kind *, generate a TextShow n constraint.
   (ii)  If there's a type parameter n of kind k1 -> k2 (where k1/k2 are * or kind
         variables), then generate a TextShow1 n constraint, and if k1/k2 are kind
         variables, then substitute k1/k2 with * elsewhere in the types. We must
         consider the case where they are kind variables because you might have a
         scenario like this:

           newtype Compose (f :: k2 -> *) (g :: k1 -> k2) (a :: k1)
             = Compose (f (g a))

         Which would have a derived TextShow1 instance of:

           instance (TextShow1 f, TextShow1 g) => TextShow1 (Compose f g) where ...
   (iii) If there's a type parameter n of kind k1 -> k2 -> k3 (where k1/k2/k3 are
         * or kind variables), then generate a TextShow2 constraint and perform
         kind substitution as in the other cases.
-}

-------------------------------------------------------------------------------
-- Error messages
-------------------------------------------------------------------------------

-- | Either the given data type doesn't have enough type variables, or one of
-- the type variables to be eta-reduced cannot realize kind *.
derivingKindError :: TextShowClass -> Name -> a
derivingKindError tsClass tyConName = error
    . showString "Cannot derive well-kinded instance of form ‘"
    . showString className
    . showChar ' '
    . showParen True
      ( showString (nameBase tyConName)
      . showString " ..."
      )
    . showString "‘\n\tClass "
    . showString className
    . showString " expects an argument of kind "
    . showString (pprint . createKindChain $ fromEnum tsClass)
    $ ""
  where
    className :: String
    className = nameBase $ textShowClassName tsClass

-- | One of the last type variables cannot be eta-reduced (see the canEtaReduce
-- function for the criteria it would have to meet).
etaReductionError :: Type -> a
etaReductionError instanceType = error $
    "Cannot eta-reduce to an instance of form \n\tinstance (...) => "
    ++ pprint instanceType

-- | The data type has a DatatypeContext which mentions one of the eta-reduced
-- type variables.
datatypeContextError :: Name -> Type -> a
datatypeContextError dataName instanceType = error
    . showString "Can't make a derived instance of ‘"
    . showString (pprint instanceType)
    . showString "‘:\n\tData type ‘"
    . showString (nameBase dataName)
    . showString "‘ must not have a class context involving the last type argument(s)"
    $ ""

-- | The data type mentions one of the n eta-reduced type variables in a place other
-- than the last nth positions of a data type in a constructor's field.
outOfPlaceTyVarError :: TextShowClass -> Name -> a
outOfPlaceTyVarError tsClass conName = error
    . showString "Constructor ‘"
    . showString (nameBase conName)
    . showString "‘ must only use its last "
    . shows n
    . showString " type variable(s) within the last "
    . shows n
    . showString " argument(s) of a data type"
    $ ""
  where
    n :: Int
    n = fromEnum tsClass

#if !(MIN_VERSION_template_haskell(2,7,0))
-- | Template Haskell didn't list all of a data family's instances upon reification
-- until template-haskell-2.7.0.0, which is necessary for a derived TextShow(1)(2)
-- instance to work.
dataConIError :: a
dataConIError = error
    . showString "Cannot use a data constructor."
    . showString "\n\t(Note: if you are trying to derive TextShow for a"
    . showString "\n\ttype family, use GHC >= 7.4 instead.)"
    $ ""
#endif

-------------------------------------------------------------------------------
-- Expanding type synonyms
-------------------------------------------------------------------------------

-- | Expands all type synonyms in a type. Written by Dan Rosén in the
-- @genifunctors@ package (licensed under BSD3).
expandSyn :: Type -> Q Type
expandSyn (ForallT tvs ctx t) = fmap (ForallT tvs ctx) $ expandSyn t
expandSyn t@AppT{}            = expandSynApp t []
expandSyn t@ConT{}            = expandSynApp t []
expandSyn (SigT t k)          = do t' <- expandSyn t
                                   k' <- expandSynKind k
                                   return (SigT t' k')
expandSyn t                   = return t

expandSynKind :: Kind -> Q Kind
#if MIN_VERSION_template_haskell(2,8,0)
expandSynKind = expandSyn
#else
expandSynKind = return -- There are no kind synonyms to deal with
#endif

expandSynApp :: Type -> [Type] -> Q Type
expandSynApp (AppT t1 t2) ts = do
    t2' <- expandSyn t2
    expandSynApp t1 (t2':ts)
expandSynApp (ConT n) ts | nameBase n == "[]" = return $ foldl' AppT ListT ts
expandSynApp t@(ConT n) ts = do
    info <- reify n
    case info of
        TyConI (TySynD _ tvs rhs) ->
            let (ts', ts'') = splitAt (length tvs) ts
                subs = mkSubst tvs ts'
                rhs' = substType subs rhs
             in expandSynApp rhs' ts''
        _ -> return $ foldl' AppT t ts
expandSynApp t ts = do
    t' <- expandSyn t
    return $ foldl' AppT t' ts

type TypeSubst = Map Name Type
type KindSubst = Map Name Kind

mkSubst :: [TyVarBndr] -> [Type] -> TypeSubst
mkSubst vs ts =
   let vs' = map un vs
       un (PlainTV v)    = v
       un (KindedTV v _) = v
   in Map.fromList $ zip vs' ts

substType :: TypeSubst -> Type -> Type
substType subs (ForallT v c t) = ForallT v c $ substType subs t
substType subs t@(VarT n)      = Map.findWithDefault t n subs
substType subs (AppT t1 t2)    = AppT (substType subs t1) (substType subs t2)
substType subs (SigT t k)      = SigT (substType subs t)
#if MIN_VERSION_template_haskell(2,8,0)
                                      (substType subs k)
#else
                                      k
#endif
substType _ t                  = t

substKind :: KindSubst -> Type -> Type
#if MIN_VERSION_template_haskell(2,8,0)
substKind = substType
#else
substKind _ = id -- There are no kind variables!
#endif

substNameWithKind :: Name -> Kind -> Type -> Type
substNameWithKind n k = substKind (Map.singleton n k)

substNamesWithKindStar :: [Name] -> Type -> Type
substNamesWithKindStar ns t = foldr' (flip substNameWithKind starK) t ns

-------------------------------------------------------------------------------
-- Class-specific constants
-------------------------------------------------------------------------------

-- | A representation of which TextShow variant is being derived.
data TextShowClass = TextShow | TextShow1 | TextShow2
  deriving (Enum, Eq, Ord)

-- | A representation of which TextShow method is being used to
-- implement something.
data TextShowFun = ShowbPrec | ShowtPrec | ShowtlPrec

fromStringName :: TextShowFun -> Name
fromStringName ShowbPrec  = 'TB.fromString
fromStringName ShowtPrec  = 'TS.pack
fromStringName ShowtlPrec = 'TL.pack

singletonName :: TextShowFun -> Name
singletonName ShowbPrec  = 'TB.singleton
singletonName ShowtPrec  = 'TS.singleton
singletonName ShowtlPrec = 'TL.singleton

showParenName :: TextShowFun -> Name
showParenName ShowbPrec  = 'showbParen
showParenName ShowtPrec  = 'showtParen
showParenName ShowtlPrec = 'showtlParen

showSpaceName :: TextShowFun -> Name
showSpaceName ShowbPrec  = 'showbSpace
showSpaceName ShowtPrec  = 'showtSpace
showSpaceName ShowtlPrec = 'showtlSpace

showPrecConstName :: TextShowClass -> TextShowFun -> Name
showPrecConstName tsClass  ShowbPrec  = showbPrecConstName tsClass
showPrecConstName TextShow ShowtPrec  = 'showtPrecConst
showPrecConstName TextShow ShowtlPrec = 'showtlPrecConst
showPrecConstName _        _          = error "showPrecConstName"

showbPrecConstName :: TextShowClass -> Name
showbPrecConstName TextShow  = 'showbPrecConst
showbPrecConstName TextShow1 = 'liftShowbPrecConst
showbPrecConstName TextShow2 = 'liftShowbPrec2Const

textShowClassName :: TextShowClass -> Name
textShowClassName TextShow  = ''TextShow
textShowClassName TextShow1 = ''TextShow1
textShowClassName TextShow2 = ''TextShow2

showPrecName :: TextShowClass -> TextShowFun -> Name
showPrecName tsClass  ShowbPrec  = showbPrecName tsClass
showPrecName TextShow ShowtPrec  = 'showtPrec
showPrecName TextShow ShowtlPrec = 'showtlPrec
showPrecName _        _          = error "showPrecName"

showbPrecName :: TextShowClass -> Name
showbPrecName TextShow  = 'showbPrec
showbPrecName TextShow1 = 'liftShowbPrec
showbPrecName TextShow2 = 'liftShowbPrec2

showListName :: TextShowClass -> TextShowFun -> Name
showListName tsClass  ShowbPrec  = showbListName tsClass
showListName TextShow ShowtPrec  = 'showtPrec
showListName TextShow ShowtlPrec = 'showtlPrec
showListName _        _          = error "showListName"

showbListName :: TextShowClass -> Name
showbListName TextShow  = 'showbList
showbListName TextShow1 = 'liftShowbList
showbListName TextShow2 = 'liftShowbList2

showPrecOrListName :: Bool -- ^ showbListName if True, showbPrecName if False
                   -> TextShowClass
                   -> TextShowFun
                   -> Name
showPrecOrListName False = showPrecName
showPrecOrListName True  = showListName

-- | A type-restricted version of 'const'. This is useful when generating the lambda
-- expression in 'makeShowbPrec' for a data type with only nullary constructors (since
-- the expression wouldn't depend on the precedence). For example, if you had @data
-- Nullary = Nullary@ and attempted to run @$(makeShowbPrec ''Nullary) Nullary@, simply
-- ignoring the precedence argument would cause the type signature of @$(makeShowbPrec
-- ''Nullary)@ to be @a -> Nullary -> Builder@, not @Int -> Nullary -> Builder@.
showbPrecConst :: Builder
               -> Int -> a -> Builder
showbPrecConst b _ _ = b

showtPrecConst :: TS.Text
               -> Int -> a -> TS.Text
showtPrecConst t _ _ = t

showtlPrecConst :: TL.Text
                -> Int -> a -> TL.Text
showtlPrecConst tl _ _ = tl

liftShowbPrecConst :: Builder
                   -> (Int -> a -> Builder) -> ([a] -> Builder)
                   -> Int -> f a -> Builder
liftShowbPrecConst b _ _ _ _ = b

liftShowbPrec2Const :: Builder
                    -> (Int -> a -> Builder) -> ([a] -> Builder)
                    -> (Int -> b -> Builder) -> ([b] -> Builder)
                    -> Int -> f a b -> Builder
liftShowbPrec2Const b _ _ _ _ _ _ = b

-------------------------------------------------------------------------------
-- StarKindStatus
-------------------------------------------------------------------------------

-- | Whether a type is not of kind *, is of kind *, or is a kind variable.
data StarKindStatus = NotKindStar
                    | KindStar
                    | IsKindVar Name
  deriving Eq

-- | Does a Type have kind * or k (for some kind variable k)?
canRealizeKindStar :: Type -> StarKindStatus
canRealizeKindStar t
  | hasKindStar t = KindStar
  | otherwise = case t of
#if MIN_VERSION_template_haskell(2,8,0)
                     SigT _ (VarT k) -> IsKindVar k
#endif
                     _               -> NotKindStar

-- | Returns 'Just' the kind variable 'Name' of a 'StarKindStatus' if it exists.
-- Otherwise, returns 'Nothing'.
starKindStatusToName :: StarKindStatus -> Maybe Name
starKindStatusToName (IsKindVar n) = Just n
starKindStatusToName _             = Nothing

-- | Concat together all of the StarKindStatuses that are IsKindVar and extract
-- the kind variables' Names out.
catKindVarNames :: [StarKindStatus] -> [Name]
catKindVarNames = mapMaybe starKindStatusToName

-------------------------------------------------------------------------------
-- Assorted utilities
-------------------------------------------------------------------------------

integerE :: Int -> Q Exp
integerE = litE . integerL . fromIntegral

charE :: Char -> Q Exp
charE = litE . charL

-- | Returns True if a Type has kind *.
hasKindStar :: Type -> Bool
hasKindStar VarT{}         = True
#if MIN_VERSION_template_haskell(2,8,0)
hasKindStar (SigT _ StarT) = True
#else
hasKindStar (SigT _ StarK) = True
#endif
hasKindStar _              = False

-- Returns True is a kind is equal to *, or if it is a kind variable.
isStarOrVar :: Kind -> Bool
#if MIN_VERSION_template_haskell(2,8,0)
isStarOrVar StarT  = True
isStarOrVar VarT{} = True
#else
isStarOrVar StarK  = True
#endif
isStarOrVar _      = False

-- Generate a list of fresh names with a common prefix, and numbered suffixes.
newNameList :: String -> Int -> Q [Name]
newNameList prefix n = mapM (newName . (prefix ++) . show) [1..n]

-- | Gets all of the type/kind variable names mentioned somewhere in a Type.
tyVarNamesOfType :: Type -> [Name]
tyVarNamesOfType = go
  where
    go :: Type -> [Name]
    go (AppT t1 t2) = go t1 ++ go t2
    go (SigT t _k)  = go t
#if MIN_VERSION_template_haskell(2,8,0)
                           ++ go _k
#endif
    go (VarT n)     = [n]
    go _            = []

-- | Gets all of the type/kind variable names mentioned somewhere in a Kind.
tyVarNamesOfKind :: Kind -> [Name]
#if MIN_VERSION_template_haskell(2,8,0)
tyVarNamesOfKind = tyVarNamesOfType
#else
tyVarNamesOfKind _ = [] -- There are no kind variables
#endif

-- | @hasKindVarChain n kind@ Checks if @kind@ is of the form
-- k_0 -> k_1 -> ... -> k_(n-1), where k0, k1, ..., and k_(n-1) can be * or
-- kind variables.
hasKindVarChain :: Int -> Type -> Maybe [Name]
hasKindVarChain kindArrows t =
  let uk = uncurryKind (tyKind t)
  in if (length uk - 1 == kindArrows) && all isStarOrVar uk
        then Just (concatMap tyVarNamesOfKind uk)
        else Nothing

-- | If a Type is a SigT, returns its kind signature. Otherwise, return *.
tyKind :: Type -> Kind
tyKind (SigT _ k) = k
tyKind _          = starK

-- | If a VarT is missing an explicit kind signature, steal it from a TyVarBndr.
stealKindForType :: TyVarBndr -> Type -> Type
stealKindForType tvb t@VarT{} = SigT t (tvbKind tvb)
stealKindForType _   t        = t

-- | Monadic version of concatMap
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

-- | A mapping of type variable Names to their show function Names. For example, in a
-- TextShow2 declaration, a TyVarMap might look like (a ~> sp1, b ~> sp2), where
-- a and b are the last two type variables of the datatype, and sp1 and sp2 are the two
-- functions which show their respective type variables.
type TyVarMap = Map Name (Name, Name)

-- | Checks if a 'Name' represents a tuple type constructor (other than '()')
isNonUnitTuple :: Name -> Bool
isNonUnitTuple = isTupleString . nameBase

-- | Parenthesize an infix constructor name if it is being applied as a prefix
-- function (e.g., data Amp a = (:&) a a)
parenInfixConName :: Name -> ShowS
parenInfixConName conName =
    let conNameBase = nameBase conName
     in showParen (isInfixDataCon conNameBase) $ showString conNameBase

-- | Extracts the kind from a TyVarBndr.
tvbKind :: TyVarBndr -> Kind
tvbKind (PlainTV  _)   = starK
tvbKind (KindedTV _ k) = k

-- | Convert a TyVarBndr to a Type.
tvbToType :: TyVarBndr -> Type
tvbToType (PlainTV n)    = VarT n
tvbToType (KindedTV n k) = SigT (VarT n) k

-- | Applies a typeclass constraint to a type.
applyClass :: Name -> Name -> Pred
#if MIN_VERSION_template_haskell(2,10,0)
applyClass con t = AppT (ConT con) (VarT t)
#else
applyClass con t = ClassP con [VarT t]
#endif

-- | Checks to see if the last types in a data family instance can be safely eta-
-- reduced (i.e., dropped), given the other types. This checks for three conditions:
--
-- (1) All of the dropped types are type variables
-- (2) All of the dropped types are distinct
-- (3) None of the remaining types mention any of the dropped types
canEtaReduce :: [Type] -> [Type] -> Bool
canEtaReduce remaining dropped =
       all isTyVar dropped
    && allDistinct droppedNames -- Make sure not to pass something of type [Type], since Type
                                -- didn't have an Ord instance until template-haskell-2.10.0.0
    && not (any (`mentionsName` droppedNames) remaining)
  where
    droppedNames :: [Name]
    droppedNames = map varTToName dropped

-- | Extract Just the Name from a type variable. If the argument Type is not a
-- type variable, return Nothing.
varTToName_maybe :: Type -> Maybe Name
varTToName_maybe (VarT n)   = Just n
varTToName_maybe (SigT t _) = varTToName_maybe t
varTToName_maybe _          = Nothing

-- | Extract the Name from a type variable. If the argument Type is not a
-- type variable, throw an error.
varTToName :: Type -> Name
varTToName = fromMaybe (error "Not a type variable!") . varTToName_maybe

-- | Peel off a kind signature from a Type (if it has one).
unSigT :: Type -> Type
unSigT (SigT t _) = t
unSigT t          = t

-- | Is the given type a variable?
isTyVar :: Type -> Bool
isTyVar (VarT _)   = True
isTyVar (SigT t _) = isTyVar t
isTyVar _          = False

-- | Is the given type a type family constructor (and not a data family constructor)?
isTyFamily :: Type -> Q Bool
isTyFamily (ConT n) = do
    info <- reify n
    return $ case info of
#if MIN_VERSION_template_haskell(2,11,0)
         FamilyI OpenTypeFamilyD{} _       -> True
#elif MIN_VERSION_template_haskell(2,7,0)
         FamilyI (FamilyD TypeFam _ _ _) _ -> True
#else
         TyConI  (FamilyD TypeFam _ _ _)   -> True
#endif
#if MIN_VERSION_template_haskell(2,9,0)
         FamilyI ClosedTypeFamilyD{} _     -> True
#endif
         _ -> False
isTyFamily _ = return False

-- | Are all of the items in a list (which have an ordering) distinct?
--
-- This uses Set (as opposed to nub) for better asymptotic time complexity.
allDistinct :: Ord a => [a] -> Bool
allDistinct = allDistinct' Set.empty
  where
    allDistinct' :: Ord a => Set a -> [a] -> Bool
    allDistinct' uniqs (x:xs)
        | x `Set.member` uniqs = False
        | otherwise            = allDistinct' (Set.insert x uniqs) xs
    allDistinct' _ _           = True

-- | Does the given type mention any of the Names in the list?
mentionsName :: Type -> [Name] -> Bool
mentionsName = go
  where
    go :: Type -> [Name] -> Bool
    go (AppT t1 t2) names = go t1 names || go t2 names
    go (SigT t _k)  names = go t names
#if MIN_VERSION_template_haskell(2,8,0)
                              || go _k names
#endif
    go (VarT n)     names = n `elem` names
    go _            _     = False

-- | Does an instance predicate mention any of the Names in the list?
predMentionsName :: Pred -> [Name] -> Bool
#if MIN_VERSION_template_haskell(2,10,0)
predMentionsName = mentionsName
#else
predMentionsName (ClassP n tys) names = n `elem` names || any (`mentionsName` names) tys
predMentionsName (EqualP t1 t2) names = mentionsName t1 names || mentionsName t2 names
#endif

-- | Construct a type via curried application.
applyTy :: Type -> [Type] -> Type
applyTy = foldl' AppT

-- | Fully applies a type constructor to its type variables.
applyTyCon :: Name -> [Type] -> Type
applyTyCon = applyTy . ConT

-- | Split an applied type into its individual components. For example, this:
--
-- @
-- Either Int Char
-- @
--
-- would split to this:
--
-- @
-- [Either, Int, Char]
-- @
unapplyTy :: Type -> NonEmpty Type
unapplyTy = NE.reverse . go
  where
    go :: Type -> NonEmpty Type
    go (AppT t1 t2)    = t2 <| go t1
    go (SigT t _)      = go t
    go (ForallT _ _ t) = go t
    go t               = t :| []

-- | Split a type signature by the arrows on its spine. For example, this:
--
-- @
-- (Int -> String) -> Char -> ()
-- @
--
-- would split to this:
--
-- @
-- [Int -> String, Char, ()]
-- @
uncurryTy :: Type -> NonEmpty Type
uncurryTy (AppT (AppT ArrowT t1) t2) = t1 <| uncurryTy t2
uncurryTy (SigT t _)                 = uncurryTy t
uncurryTy (ForallT _ _ t)            = uncurryTy t
uncurryTy t                          = t :| []

-- | Like uncurryType, except on a kind level.
uncurryKind :: Kind -> NonEmpty Kind
#if MIN_VERSION_template_haskell(2,8,0)
uncurryKind = uncurryTy
#else
uncurryKind (ArrowK k1 k2) = k1 <| uncurryKind k2
uncurryKind k              = k :| []
#endif

createKindChain :: Int -> Kind
createKindChain = go starK
  where
    go :: Kind -> Int -> Kind
    go k !0 = k
#if MIN_VERSION_template_haskell(2,8,0)
    go k !n = go (AppT (AppT ArrowT StarT) k) (n - 1)
#else
    go k !n = go (ArrowK StarK k) (n - 1)
#endif

#if MIN_VERSION_template_haskell(2,7,0)
-- | Extracts the name of a constructor.
constructorName :: Con -> Name
constructorName (NormalC name      _  ) = name
constructorName (RecC    name      _  ) = name
constructorName (InfixC  _    name _  ) = name
constructorName (ForallC _    _    con) = constructorName con
# if MIN_VERSION_template_haskell(2,11,0)
constructorName (GadtC    names _ _)    = head names
constructorName (RecGadtC names _ _)    = head names
# endif
#endif

isNullaryCon :: Con -> Bool
isNullaryCon (NormalC _ [])    = True
isNullaryCon (RecC    _ [])    = True
isNullaryCon InfixC{}          = False
isNullaryCon (ForallC _ _ con) = isNullaryCon con
#if MIN_VERSION_template_haskell(2,11,0)
isNullaryCon (GadtC    _ [] _) = True
isNullaryCon (RecGadtC _ [] _) = True
#endif
isNullaryCon _                 = False

interleave :: [a] -> [a] -> [a]
interleave (a1:a1s) (a2:a2s) = a1:a2:interleave a1s a2s
interleave _        _        = []

-- Determines the types of a constructor's arguments as well as the last type
-- parameters (mapped to their show functions), expanding through any type synonyms.
-- The type parameters are determined on a constructor-by-constructor basis since
-- they may be refined to be particular types in a GADT.
reifyConTys :: TextShowClass
            -> [(Name, Name)]
            -> Name
            -> Q ([Type], TyVarMap)
reifyConTys tsClass spls conName = do
    info  <- reify conName
    uncTy <- case info of
                  DataConI _ ty _
#if !(MIN_VERSION_template_haskell(2,11,0))
                           _
#endif
                           -> fmap uncurryTy (expandSyn ty)
                  _ -> error "Must be a data constructor"
    let (argTys, [resTy]) = NE.splitAt (length uncTy - 1) uncTy
        unapResTy = unapplyTy resTy
        -- If one of the last type variables is refined to a particular type
        -- (i.e., not truly polymorphic), we mark it with Nothing and filter
        -- it out later, since we only apply show functions to arguments of
        -- a type that it (1) one of the last type variables, and (2)
        -- of a truly polymorphic type.
        mbTvNames = map varTToName_maybe $
                        NE.drop (NE.length unapResTy - fromEnum tsClass) unapResTy
        -- We use Map.fromList to ensure that if there are any duplicate type
        -- variables (as can happen in a GADT), the rightmost type variable gets
        -- associated with the show function.
        --
        -- See Note [Matching functions with GADT type variables]
        tvMap = Map.fromList
                    . catMaybes -- Drop refined types
                    $ zipWith (\mbTvName sp ->
                                  fmap (\tvName -> (tvName, sp)) mbTvName)
                              mbTvNames spls
    return (argTys, tvMap)

{-
Note [Matching functions with GADT type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When deriving TextShow2, there is a tricky corner case to consider:

  data Both a b where
    BothCon :: x -> x -> Both x x

Which show functions should be applied to which arguments of BothCon? We have a
choice, since both the function of type (Int -> a -> Builder) and of type
(Int -> b -> Builder) can be applied to either argument. In such a scenario, the
second show function takes precedence over the first show function, so the
derived TextShow2 instance would be:

  instance TextShow Both where
    liftShowsPrec2 sp1 sp2 p (BothCon x1 x2) =
      showbParen (p > appPrec) $
        "BothCon " <> sp2 appPrec1 x1 <> showbSpace <> sp2 appPrec1 x2

This is not an arbitrary choice, as this definition ensures that
liftShowsPrec2 showsPrec = liftShowsPrec for a derived TextShow1 instance for
Both.
-}
