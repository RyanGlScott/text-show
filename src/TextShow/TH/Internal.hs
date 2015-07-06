{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-|
Module:      TextShow.TH.Internal
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Functions to mechanically derive 'TextShow', 'TextShow1', or 'TextShow2' instances,
or to splice @show@-related expressions into Haskell source code. You need to enable
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
    , makeShowbPrecWith
    , makeShowbPrec1
    , makeShowbPrecWith2
    , makeShowbPrec2
    ) where

import           Data.Function (on)
import           Data.List.Compat (foldl', intersperse)
#if MIN_VERSION_template_haskell(2,7,0)
import           Data.List.Compat (find)
#endif
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.Map as Map (fromList, lookup)
import           Data.Map (Map)
import           Data.Maybe (fromMaybe)
import           Data.Monoid.Compat ((<>))
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Text    as TS ()
import qualified Data.Text.IO as TS (putStrLn, hPutStrLn)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (Builder, fromString, singleton, toLazyText)
import qualified Data.Text.Lazy    as TL ()
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
                                   showbListWith, showbParen, showbSpace)
import           TextShow.Utils (isInfixTypeCon, isTupleString)

-------------------------------------------------------------------------------
-- User-facing API
-------------------------------------------------------------------------------

{- $deriveTextShow

'deriveTextShow' automatically generates a 'TextShow' instance declaration for a data
type, a newtype, or a data family instance. This emulates what would (hypothetically)
happen if you could attach a @deriving 'TextShow'@ clause to the end of a data
declaration.

Here are some examples of how to derive 'TextShow' for simple data types:

@
&#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
import TextShow.TH (deriveTextShow)

data Letter = A | B | C
$('deriveTextShow' ''Letter) -- instance TextShow Letter where ...

newtype Box a = Box a
$('deriveTextShow' ''Box) -- instance TextShow a => TextShow (Box a) where ...
@

If you are using @template-haskell-2.7.0.0@ or later (i.e., GHC 7.4 or later),
'deriveTextShow' can also be used to derive 'TextShow' instances for data family
instances (which requires the @-XTypeFamilies@ extension). To do so, pass the name of
a data or newtype instance constructor to 'deriveTextShow'.  Note that the generated
code may require the @-XFlexibleInstances@ extension. Some examples:

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
  to use 'makeShowbPrec' (see the documentation of the 'make' functions for more
  information).

* 'deriveTextShow' lacks the ability to properly detect data types with higher-kinded
   type parameters (e.g., @data HK f a = HK (f a)@) or with kinds other than @*@
   (e.g., @data List a (empty :: Bool)@). If you wish to derive 'TextShow'
   instances for these data types, you will need to use 'makeShowbPrec' (see the
   documentation of the 'make' functions for more information).

* Some data constructors have arguments whose 'TextShow' instance depends on a
  typeclass besides 'TextShow'. For example, consider @newtype MyFixed a = MyFixed
  (Fixed a)@. @'Fixed' a@ is a 'TextShow' instance only if @a@ is an instance of both
  @HasResolution@ and 'TextShow'. Unfortunately, 'deriveTextShow' cannot infer that
  'a' must be an instance of 'HasResolution', so it cannot create a 'TextShow'
  instance for @MyFixed@. However, you can use 'makeShowbPrec' to get around this
  (see the documentation of the 'make' functions for more information).

-}

-- | Generates a 'TextShow' instance declaration for the given data type or data
-- family instance.
--
-- /Since: 2/
deriveTextShow :: Name -> Q [Dec]
deriveTextShow = deriveTextShowClass TextShow

{- $deriveTextShow1

'deriveTextShow1' automatically generates a 'Show1' instance declaration for a data
type, a newtype, or a data family instance that has at least one type variable.
This emulates what would (hypothetically) happen if you could attach a @deriving
'TextShow1'@ clause to the end of a data declaration. Examples:

@
&#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
import TextShow.TH (deriveTextShow1)

data Stream a = Stream a (Stream a)
$('deriveTextShow1' ''Stream) -- instance Show1 TextStream where ...

newtype WrappedFunctor f a = WrapFunctor (f a)
$('deriveTextShow1' ''WrappedFunctor) -- instance TextShow1 f => TextShow1 (WrappedFunctor f) where ...
@

The same restrictions that apply to 'deriveTextShow' also apply to 'deriveTextShow1',
with some caveats:

* With 'deriveTextShow1', the last type variable must be of kind @*@. For other ones,
  type variables of kind @*@ are assumed to require a 'TextShow' context, and type
  variables of kind @* -> *@ are assumed to require a 'TextShow1' context. For more
  complicated scenarios, use 'makeShowbPrecWith'.

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

* In GHC 7.8, a bug exists that can cause problems when a data family declaration and
  one of its data instances use different type variables, e.g.,

  @
  data family Foo a b c
  data instance Foo Int y z = Foo Int y z
  $(deriveTextShow1 'Foo)
  @

  To avoid this issue, it is recommened that you use the same type variables in the
  same positions in which they appeared in the data family declaration:

  @
  data family Foo a b c
  data instance Foo Int b c = Foo Int b c
  $(deriveTextShow1 'Foo)
  @

-}

-- | Generates a 'TextShow1' instance declaration for the given data type or data
-- family instance.
--
-- /Since: 2/
deriveTextShow1 :: Name -> Q [Dec]
deriveTextShow1 = deriveTextShowClass TextShow1

{- $deriveTextShow2

'deriveTextShow2' automatically generates a 'TextShow2' instance declaration for a data
type, a newtype, or a data family instance that has at least two type variables.
This emulates what would (hypothetically) happen if you could attach a @deriving
'TextShow2'@ clause to the end of a data declaration. Examples:

@
&#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
import TextShow.TH (deriveShow2)

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
  more complicated scenarios, use 'makeShowbPrecWith2'.

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
deriveTextShow2 = deriveTextShowClass TextShow2

{- $make

There may be scenarios in which you want to show an arbitrary data type or data
family instance without having to make the type an instance of 'TextShow'. For these
cases, "TextShow.TH" provide several functions (all prefixed with @make@) that
splice the appropriate lambda expression into your source code. Example:

@
&#123;-&#35; LANGUAGE OverloadedStrings, TemplateHaskell &#35;-&#125;
import TextShow.TH (makeShowT)

data ADT = ADT

whichADT :: Bool
whichADT = $(makeShowT ''ADT) ADT == \"ADT\"
@

@make@ functions are also useful for creating 'TextShow' instances for data types with
sophisticated type parameters. For example, 'deriveTextShow' cannot infer the correct
type context for @newtype HigherKinded f a = HigherKinded (f a)@, since @f@ is a
higher-kinded type parameter. However, it is still possible to derive a 'TextShow'
instance for @HigherKinded@ without too much trouble using 'makeShowbPrec':

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
makeShowt name = [| toStrict . $(makeShowtl name) |]

-- | Generates a lambda expression which behaves like 'showtl' (without requiring a
-- 'TextShow' instance).
--
-- /Since: 2/
makeShowtl :: Name -> Q Exp
makeShowtl name = [| toLazyText . $(makeShowb name) |]

-- | Generates a lambda expression which behaves like 'showtPrec' (without requiring a
-- 'TextShow' instance).
--
-- /Since: 2/
makeShowtPrec :: Name -> Q Exp
makeShowtPrec name = [| \p -> toStrict . $(makeShowtlPrec name) p |]

-- | Generates a lambda expression which behaves like 'showtlPrec' (without
-- requiring a 'TextShow' instance).
--
-- /Since: 2/
makeShowtlPrec :: Name -> Q Exp
makeShowtlPrec name = [| \p -> toLazyText . $(makeShowbPrec name) p |]

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
makeShowb name = makeShowbPrec name `appE` [| zero |]
  where
    -- To prevent the generated TH code from having a type ascription
    zero :: Int
    zero = 0

-- | Generates a lambda expression which behaves like 'showbPrec' (without requiring a
-- 'TextShow' instance).
--
-- /Since: 2/
makeShowbPrec :: Name -> Q Exp
makeShowbPrec = makeShowbPrecClass TextShow

-- | Generates a lambda expression which behaves like 'showbPrecWith' (without
-- requiring a 'TextShow1' instance).
--
-- /Since: 2/
makeShowbPrecWith :: Name -> Q Exp
makeShowbPrecWith = makeShowbPrecClass TextShow1

-- | Generates a lambda expression which behaves like 'showbPrec1' (without
-- requiring a 'TextShow1' instance).
--
-- /Since: 2/
makeShowbPrec1 :: Name -> Q Exp
makeShowbPrec1 name = [| $(makeShowbPrecWith name) showbPrec |]

-- | Generates a lambda expression which behaves like 'showbPrecWith2' (without
-- requiring a 'TextShow2' instance).
--
-- /Since: 2/
makeShowbPrecWith2 :: Name -> Q Exp
makeShowbPrecWith2 = makeShowbPrecClass TextShow2

-- | Generates a lambda expression which behaves like 'showbPrecWith2' (without
-- requiring a 'TextShow2' instance).
--
-- /Since: 2/
makeShowbPrec2 :: Name -> Q Exp
makeShowbPrec2 name = [| $(makeShowbPrecWith2 name) showbPrec showbPrec |]

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
deriveTextShowClass :: TextShowClass -> Name -> Q [Dec]
deriveTextShowClass tsClass tyConName = do
    info <- reify tyConName
    case info of
        TyConI{} -> deriveTextShowPlainTy tsClass tyConName
#if MIN_VERSION_template_haskell(2,7,0)
        DataConI{} -> deriveTextShowDataFamInst tsClass tyConName
        FamilyI (FamilyD DataFam _ _ _) _ ->
            error $ ns ++ "Cannot use a data family name. Use a data family instance constructor instead."
        FamilyI (FamilyD TypeFam _ _ _) _ ->
            error $ ns ++ "Cannot use a type family name."
        _ -> error $ ns ++ "The name must be of a plain type constructor or data family instance constructor."
#else
        DataConI{} -> dataConIError
        _          -> error $ ns ++ "The name must be of a plain type constructor."
#endif
  where
    ns :: String
    ns = "TextShow.TH.deriveTextShow: "

-- | Generates a TextShow(1)(2) instance declaration for a plain type constructor.
deriveTextShowPlainTy :: TextShowClass -> Name -> Q [Dec]
deriveTextShowPlainTy tsClass tyConName =
    withTyCon tyConName fromCons
  where
    className :: Name
    className = textShowClassNameTable tsClass

    fromCons :: Cxt -> [TyVarBndr] -> [Con] -> Q [Dec]
    fromCons ctxt tvbs cons = (:[]) <$>
        instanceD (return instanceCxt)
                  (return $ AppT (ConT className) instanceType)
                  (showbPrecDecs droppedNbs cons)
      where
        (instanceCxt, instanceType, droppedNbs) =
            cxtAndTypePlainTy tsClass tyConName ctxt tvbs

#if MIN_VERSION_template_haskell(2,7,0)
-- | Generates a TextShow(1)(2) instance declaration for a data family instance
-- constructor.
deriveTextShowDataFamInst :: TextShowClass -> Name -> Q [Dec]
deriveTextShowDataFamInst tsClass dataFamInstName =
    withDataFamInstCon dataFamInstName fromDec
  where
    className :: Name
    className = textShowClassNameTable tsClass

    fromDec :: [TyVarBndr] -> Cxt -> Name -> [Type] -> [Con] -> Q [Dec]
    fromDec famTvbs ctxt parentName instTys cons = (:[]) <$>
        instanceD (return instanceCxt)
                  (return $ AppT (ConT className) instanceType)
                  (showbPrecDecs droppedNbs cons)
      where
        (instanceCxt, instanceType, droppedNbs) =
            cxtAndTypeDataFamInstCon tsClass parentName ctxt famTvbs instTys
#endif

-- | Generates a declaration defining the primary function corresponding to a
-- particular class (showbPrec for TextShow, showbPrecWith for TextShow1, and
-- showbPrecWith2 for TextShow2).
showbPrecDecs :: [NameBase] -> [Con] -> [Q Dec]
showbPrecDecs nbs cons =
    [ funD classFuncName
           [ clause []
                    (normalB $ makeTextShowForCons nbs cons)
                    []
           ]
    ]
  where
    classFuncName :: Name
    classFuncName  = showbPrecNameTable . toEnum $ length nbs

-- | Generates a lambda expression which behaves like showbPrec (for TextShow),
-- showbPrecWith (for TextShow1), or showbPrecWth2 (for TextShow2).
makeShowbPrecClass :: TextShowClass -> Name -> Q Exp
makeShowbPrecClass tsClass tyConName = do
    info <- reify tyConName
    case info of
        TyConI{} -> withTyCon tyConName $ \ctxt tvbs decs ->
            let (_, _, nbs) = cxtAndTypePlainTy tsClass tyConName ctxt tvbs
             in makeTextShowForCons nbs decs
#if MIN_VERSION_template_haskell(2,7,0)
        DataConI{} -> withDataFamInstCon tyConName $ \famTvbs ctxt parentName instTys cons ->
            let (_, _, nbs) = cxtAndTypeDataFamInstCon tsClass parentName ctxt famTvbs instTys
             in makeTextShowForCons nbs cons
        FamilyI (FamilyD DataFam _ _ _) _ ->
            error $ ns ++ "Cannot use a data family name. Use a data family instance constructor instead."
        FamilyI (FamilyD TypeFam _ _ _) _ ->
            error $ ns ++ "Cannot use a type family name."
        _ -> error $ ns ++ "The name must be of a plain type constructor or data family instance constructor."
#else
        DataConI{} -> dataConIError
        _          -> error $ ns ++ "The name must be of a plain type constructor."
#endif
  where
    ns :: String
    ns = "TextShow.TH.makeShowbPrec: "

-- | Generates a lambda expression for showbPrec(With)(2) for the given constructors.
-- All constructors must be from the same type.
makeTextShowForCons :: [NameBase] -> [Con] -> Q Exp
makeTextShowForCons _   []   = error "Must have at least one data constructor"
makeTextShowForCons nbs cons = do
    p     <- newName "p"
    value <- newName "value"
    sps   <- newNameList "sp" $ length nbs
    let tvis   = zip nbs sps
        tsClass = toEnum $ length nbs
    lamE (map varP $ sps ++ [p, value])
        . appsE
        $ [ varE $ showbPrecConstNameTable tsClass
          , caseE (varE value) $ map (makeTextShowForCon p tsClass tvis) cons
          ] ++ map varE sps
            ++ [varE p, varE value]

-- | Generates a lambda expression for showbPrec(With)(2) for a single constructor.
makeTextShowForCon :: Name -> TextShowClass -> [TyVarInfo] -> Con -> Q Match
makeTextShowForCon _ _ _ (NormalC conName [])
    = match (conP conName [])
            (normalB [| fromString $(stringE (parenInfixConName conName "")) |])
            []
makeTextShowForCon p tsClass tvis (NormalC conName [(_, argTy)]) = do
    arg <- newName "arg"

    let showArg  = makeTextShowForArg appPrec1 tsClass (nameBase conName) tvis argTy arg
        namedArg = [| fromString $(stringE (parenInfixConName conName " ")) <> $(showArg) |]

    match (conP conName [varP arg])
          (normalB [| showbParen ($(varE p) > $(lift appPrec)) $(namedArg) |])
          []
makeTextShowForCon p tsClass tvis (NormalC conName ts) = do
    args <- newNameList "arg" $ length ts

    if isNonUnitTuple conName
       then do
           let showArgs       = map (\(arg, (_, argTy)) -> makeTextShowForArg 0 tsClass (nameBase conName) tvis argTy arg)
                                    (zip args ts)
               parenCommaArgs = [| singleton '(' |] : intersperse [| singleton ',' |] showArgs
               mappendArgs    = foldr (`infixApp` [| (<>) |])
                                      [| singleton ')' |]
                                      parenCommaArgs

           match (conP conName $ map varP args)
                 (normalB mappendArgs)
                 []
       else do
           let showArgs = map (\(arg, (_, argTy)) -> makeTextShowForArg appPrec1 tsClass (nameBase conName) tvis argTy arg)
                              (zip args ts)
               mappendArgs = foldr1 (\v q -> [| $(v) <> showbSpace <> $(q) |]) showArgs
               namedArgs   = [| fromString $(stringE (parenInfixConName conName " ")) <> $(mappendArgs) |]

           match (conP conName $ map varP args)
                 (normalB [| showbParen ($(varE p) > $(lift appPrec)) $(namedArgs) |])
                 []
makeTextShowForCon p  tsClass tvis (RecC conName []) = makeTextShowForCon p tsClass tvis $ NormalC conName []
makeTextShowForCon _p tsClass tvis (RecC conName ts) = do
    args <- newNameList "arg" $ length ts

    let showArgs       = concatMap (\(arg, (argName, _, argTy))
                                      -> [ [| fromString $(stringE (nameBase argName ++ " = ")) |]
                                         , makeTextShowForArg 0 tsClass (nameBase conName) tvis argTy arg
                                         , [| fromString ", "                                   |]
                                         ]
                                   )
                                   (zip args ts)
        braceCommaArgs = [| singleton '{' |] : take (length showArgs - 1) showArgs
        mappendArgs    = foldr (`infixApp` [| (<>) |])
                           [| singleton '}' |]
                           braceCommaArgs
        namedArgs      = [| fromString $(stringE (parenInfixConName conName " ")) <> $(mappendArgs) |]

    match (conP conName $ map varP args)
          (normalB
#if __GLASGOW_HASKELL__ >= 711
                    namedArgs
#else
                    [| showbParen ($(varE _p) > $(lift appPrec)) $(namedArgs) |]
#endif
          )
          []
makeTextShowForCon p tsClass tvis (InfixC (_, alTy) conName (_, arTy)) = do
    al   <- newName "argL"
    ar   <- newName "argR"
    info <- reify conName

    let conPrec  = case info of
                        DataConI _ _ _ (Fixity prec _) -> prec
                        other -> error $ "TextShow.TH.makeTextShowForCon: Unsupported type: " ++ show other
        opName   = nameBase conName
        infixOpE = if isInfixTypeCon opName
                      then [| fromString $(stringE $ " "  ++ opName ++ " " ) |]
                      else [| fromString $(stringE $ " `" ++ opName ++ "` ") |]

    match (infixP (varP al) conName (varP ar))
          (normalB $ appE [| showbParen ($(varE p) > conPrec) |]
                          [| $(makeTextShowForArg (conPrec + 1) tsClass opName tvis alTy al)
                          <> $(infixOpE)
                          <> $(makeTextShowForArg (conPrec + 1) tsClass opName tvis arTy ar)
                          |]
          )
          []
makeTextShowForCon p tsClass tvis (ForallC tvbs _ con) = makeTextShowForCon p tsClass (removeForalled tvbs tvis) con

-- | Generates a lambda expression for showbPrec(With)(2) for an argument of a
-- constructor.
makeTextShowForArg :: Int
                   -> TextShowClass
                   -> String
                   -> [TyVarInfo]
                   -> Type
                   -> Name
                   -> Q Exp
makeTextShowForArg p tsClass conName tvis ty tyExpName = do
    ty' <- expandSyn ty
    makeTextShowForArg' p tsClass conName tvis ty' tyExpName

-- | Generates a lambda expression for showbPrec(With)(2) for an argument of a
-- constructor, after expanding all type synonyms.
makeTextShowForArg' :: Int
                    -> TextShowClass
                    -> String
                    -> [TyVarInfo]
                    -> Type
                    -> Name
                    -> Q Exp
makeTextShowForArg' p _ _ _ (ConT tyName) tyExpName =
#if __GLASGOW_HASKELL__ >= 711
-- Starting with GHC 7.10, data types containing unlifted types with derived @Show@
-- instances show hashed literals with actual hash signs, and negative hashed
-- literals are not surrounded with parentheses.
    showE
  where
    tyVarE :: Q Exp
    tyVarE = varE tyExpName

    showE :: Q Exp
    showE | tyName == ''Char#   = [| showbPrec 0 (C# $(tyVarE)) <> singleton '#'   |]
          | tyName == ''Double# = [| showbPrec 0 (D# $(tyVarE)) <> fromString "##" |]
          | tyName == ''Float#  = [| showbPrec 0 (F# $(tyVarE)) <> singleton '#'   |]
          | tyName == ''Int#    = [| showbPrec 0 (I# $(tyVarE)) <> singleton '#'   |]
          | tyName == ''Word#   = [| showbPrec 0 (W# $(tyVarE)) <> fromString "##" |]
          | otherwise = [| showbPrec p $(tyVarE) |]
#else
    [| showbPrec p $(expr) |]
  where
    tyVarE :: Q Exp
    tyVarE = varE tyExpName

    expr :: Q Exp
    expr | tyName == ''Char#   = [| C# $(tyVarE) |]
         | tyName == ''Double# = [| D# $(tyVarE) |]
         | tyName == ''Float#  = [| F# $(tyVarE) |]
         | tyName == ''Int#    = [| I# $(tyVarE) |]
         | tyName == ''Word#   = [| W# $(tyVarE) |]
         | otherwise = tyVarE
#endif
makeTextShowForArg' p tsClass conName tvis ty tyExpName =
    [| $(makeTextShowForType tsClass conName tvis ty) p $(varE tyExpName) |]

-- | Generates a lambda expression for showbPrec(With)(2) for a specific type.
-- The generated expression depends on the number of type variables.
--
-- 1. If the type is of kind * (T), apply showbPrec.
-- 2. If the type is of kind * -> * (T a), apply showbPrecWith $(makeTextShowForType a)
-- 3. If the type is of kind * -> * -> * (T a b), apply
--    showbPrecWith2 $(makeTextShowForType a) $(makeTextShowForType b)
makeTextShowForType :: TextShowClass
                    -> String
                    -> [TyVarInfo]
                    -> Type
                    -> Q Exp
makeTextShowForType _ _ tvis (VarT tyName) =
    case lookup (NameBase tyName) tvis of
         Just spExp -> varE spExp
         Nothing    -> [| showbPrec |]
makeTextShowForType tsClass conName tvis (SigT ty _)         = makeTextShowForType tsClass conName tvis ty
makeTextShowForType tsClass conName tvis (ForallT tvbs _ ty) = makeTextShowForType tsClass conName (removeForalled tvbs tvis) ty
makeTextShowForType tsClass conName tvis ty = do
    let tyArgs :: [Type]
        tyCon :| tyArgs = unapplyTy ty

        numLastArgs :: Int
        numLastArgs = min (fromEnum tsClass) (length tyArgs)

        lhsArgs, rhsArgs :: [Type]
        (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

        tyVarNameBases :: [NameBase]
        tyVarNameBases = map fst tvis

    itf <- isTyFamily tyCon
    if any (`mentionsNameBase` tyVarNameBases) lhsArgs
          || itf && any (`mentionsNameBase` tyVarNameBases) tyArgs
       then outOfPlaceTyVarError conName tyVarNameBases numLastArgs
       else appsE $ [ varE . showbPrecNameTable $ toEnum numLastArgs]
                    ++ map (makeTextShowForType tsClass conName tvis) rhsArgs

-------------------------------------------------------------------------------
-- Template Haskell reifying and AST manipulation
-------------------------------------------------------------------------------

-- | Extracts a plain type constructor's information.
withTyCon :: Name -- ^ Name of the plain type constructor
          -> (Cxt -> [TyVarBndr] -> [Con] -> Q a)
          -> Q a
withTyCon name f = do
    info <- reify name
    case info of
        TyConI dec ->
            case dec of
                DataD    ctxt _ tvbs cons _ -> f ctxt tvbs cons
                NewtypeD ctxt _ tvbs con  _ -> f ctxt tvbs [con]
                other -> error $ ns ++ "Unsupported type " ++ show other ++ ". Must be a data type or newtype."
        _ -> error $ ns ++ "The name must be of a plain type constructor."
  where
    ns :: String
    ns = "TextShow.TH.withTyCon: "

#if MIN_VERSION_template_haskell(2,7,0)
-- | Extracts a data family name's information.
withDataFam :: Name -- ^ Name of the data family
            -> ([TyVarBndr] -> [Dec] -> Q a)
            -> Q a
withDataFam name f = do
    info <- reify name
    case info of
        FamilyI (FamilyD DataFam _ tvbs _) decs -> f tvbs decs
        FamilyI (FamilyD TypeFam _ _    _) _    ->
            error $ ns ++ "Cannot use a type family name."
        other -> error $ ns ++ "Unsupported type " ++ show other ++ ". Must be a data family name."
  where
    ns :: String
    ns = "TextShow.TH.withDataFam: "

-- | Extracts a data family instance constructor's information.
withDataFamInstCon :: Name -- ^ Name of the data family instance constructor
                   -> ([TyVarBndr] -> Cxt -> Name -> [Type] -> [Con] -> Q a)
                   -> Q a
withDataFamInstCon dficName f = do
    dficInfo <- reify dficName
    case dficInfo of
        DataConI _ _ parentName _ -> do
            parentInfo <- reify parentName
            case parentInfo of
                FamilyI (FamilyD DataFam _ _ _) _ -> withDataFam parentName $ \famTvbs decs ->
                    let sameDefDec = flip find decs $ \dec ->
                          case dec of
                              DataInstD    _ _ _ cons' _ -> any ((dficName ==) . constructorName) cons'
                              NewtypeInstD _ _ _ con   _ -> dficName == constructorName con
                              _ -> error $ ns ++ "Must be a data or newtype instance."

                        (ctxt, instTys, cons) = case sameDefDec of
                              Just (DataInstD    ctxt' _ instTys' cons' _) -> (ctxt', instTys', cons')
                              Just (NewtypeInstD ctxt' _ instTys' con   _) -> (ctxt', instTys', [con])
                              _ -> error $ ns ++ "Could not find data or newtype instance constructor."

                    in f famTvbs ctxt parentName instTys cons
                _ -> error $ ns ++ "Data constructor " ++ show dficName ++ " is not from a data family instance."
        other -> error $ ns ++ "Unsupported type " ++ show other ++ ". Must be a data family instance constructor."
  where
    ns :: String
    ns = "TextShow.TH.withDataFamInstCon: "
#endif

-- | Deduces the TextShow(1)(2) instance context, instance head, and eta-reduced
-- type variables for a plain data type constructor.
cxtAndTypePlainTy :: TextShowClass -- TextShow, TextShow1, or TextShow2
                  -> Name          -- The datatype's name
                  -> Cxt           -- The datatype context
                  -> [TyVarBndr]   -- The type variables
                  -> (Cxt, Type, [NameBase])
cxtAndTypePlainTy tsClass tyConName dataCxt tvbs =
    if remainingLength < 0 || not (wellKinded droppedKinds) -- If we have enough well-kinded type variables
       then derivingKindError tsClass tyConName
    else if any (`predMentionsNameBase` droppedNbs) dataCxt -- If the last type variable(s) are mentioned in a datatype context
       then datatypeContextError tsClass instanceType
    else (instanceCxt, instanceType, droppedNbs)
  where
    instanceCxt :: Cxt
    instanceCxt = map (applyShowConstraint)
                $ filter (needsConstraint tsClass . tvbKind) remaining

    instanceType :: Type
    instanceType = applyTyCon tyConName $ map (VarT . tvbName) remaining

    remainingLength :: Int
    remainingLength = length tvbs - fromEnum tsClass

    remaining, dropped :: [TyVarBndr]
    (remaining, dropped) = splitAt remainingLength tvbs

    droppedKinds :: [Kind]
    droppedKinds = map tvbKind dropped

    droppedNbs :: [NameBase]
    droppedNbs = map (NameBase . tvbName) dropped

#if MIN_VERSION_template_haskell(2,7,0)
-- | Deduces the TextShow(1)(2) instance context, instance head, and eta-reduced
-- type variables for a data family instnce constructor.
cxtAndTypeDataFamInstCon :: TextShowClass -- TextShow, TextShow1, or TextShow2
                         -> Name          -- The data family name
                         -> Cxt           -- The datatype context
                         -> [TyVarBndr]   -- The data family declaration's type variables
                         -> [Type]        -- The data family instance types
                         -> (Cxt, Type, [NameBase])
cxtAndTypeDataFamInstCon tsClass parentName dataCxt famTvbs instTysAndKinds =
    if remainingLength < 0 || not (wellKinded droppedKinds) -- If we have enough well-kinded type variables
       then derivingKindError tsClass parentName
    else if any (`predMentionsNameBase` droppedNbs) dataCxt -- If the last type variable(s) are mentioned in a datatype context
       then datatypeContextError tsClass instanceType
    else if canEtaReduce remaining dropped -- If it is safe to drop the type variables
       then (instanceCxt, instanceType, droppedNbs)
    else etaReductionError instanceType
  where
    instanceCxt :: Cxt
    instanceCxt = map (applyShowConstraint)
                $ filter (needsConstraint tsClass . tvbKind) lhsTvbs

    -- We need to make sure that type variables in the instance head which have
    -- TextShow constrains aren't poly-kinded, e.g.,
    --
    -- @
    -- instance TextShow a => TextShow (Foo (a :: k)) where
    -- @
    --
    -- To do this, we remove every kind ascription (i.e., strip off every 'SigT').
    instanceType :: Type
    instanceType = applyTyCon parentName
                 $ map unSigT remaining

    remainingLength :: Int
    remainingLength = length famTvbs - fromEnum tsClass

    remaining, dropped :: [Type]
    (remaining, dropped) = splitAt remainingLength rhsTypes

    droppedKinds :: [Kind]
    droppedKinds = map tvbKind . snd $ splitAt remainingLength famTvbs

    droppedNbs :: [NameBase]
    droppedNbs = map varTToNameBase dropped

    -- We need to mindful of an old GHC bug which causes kind variables appear in
    -- @instTysAndKinds@ (as the name suggests) if (1) @PolyKinds@ is enabled, and
    -- (2) either GHC 7.6 or 7.8 is being used (for more info, see
    -- https://ghc.haskell.org/trac/ghc/ticket/9692).
    --
    -- Since Template Haskell doesn't seem to have a mechanism for detecting which
    -- language extensions are enabled, we do the next-best thing by counting
    -- the number of distinct kind variables in the data family declaration, and
    -- then dropping that number of entries from @instTysAndKinds@
    instTypes :: [Type]
    instTypes =
# if __GLASGOW_HASKELL__ >= 710 || !(MIN_VERSION_template_haskell(2,8,0))
        instTysAndKinds
# else
        drop (Set.size . Set.unions $ map (distinctKindVars . tvbKind) famTvbs)
             instTysAndKinds
# endif

    lhsTvbs :: [TyVarBndr]
    lhsTvbs = map (uncurry replaceTyVarName)
            . filter (isTyVar . snd)
            . take remainingLength
            $ zip famTvbs rhsTypes

    -- In GHC 7.8, only the @Type@s up to the rightmost non-eta-reduced type variable
    -- in @instTypes@ are provided (as a result of this extremely annoying bug:
    -- https://ghc.haskell.org/trac/ghc/ticket/9692). This is pretty inconvenient,
    -- as it makes it impossible to come up with the correct 'TextShow1' or 'TextShow2'
    -- instances in some cases. For example, consider the following code:
    --
    -- @
    -- data family Foo a b c
    -- data instance Foo Int y z = Foo Int y z
    -- $(deriveTextShow2 'Foo)
    -- @
    --
    -- Due to the aformentioned bug, Template Haskell doesn't tell us the names of
    -- either of type variables in the data instance (@y@ and @z@). As a result, we
    -- won't know which fields of the 'Foo' constructor to apply the show functions,
    -- which will result in an incorrect instance. Urgh.
    --
    -- A workaround is to ensure that you use the exact same type variables, in the
    -- exact same order, in the data family declaration and any data or newtype
    -- instances:
    --
    -- @
    -- data family Foo a b c
    -- data instance Foo Int b c = Foo Int b c
    -- $(deriveTextShow2 'Foo)
    -- @
    --
    -- Thankfully, other versions of GHC don't seem to have this bug.
    rhsTypes :: [Type]
    rhsTypes =
# if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
            instTypes ++ map tvbToType
                             (drop (length instTypes)
                                   famTvbs)
# else
            instTypes
# endif
#endif

-- | Given a TyVarBndr, apply a TextShow(1)(2) constraint to it, depending
-- on its kind.
applyShowConstraint :: TyVarBndr -> Pred
applyShowConstraint (PlainTV  name)      = applyClass ''TextShow  name
applyShowConstraint (KindedTV name kind) = applyClass className   name
  where
    className :: Name
    className = textShowClassNameTable . toEnum $ numKindArrows kind

-- | Can a kind signature inhabit a TextShow(1)(2) constraint?
--
-- TextShow:  k
-- TextShow1: k1 -> k2
-- TextShow2: k1 -> k2 -> k3
needsConstraint :: TextShowClass -> Kind -> Bool
needsConstraint tsClass kind =
       fromEnum tsClass >= numKindArrows kind
    && canRealizeKindStarChain kind

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
    className = nameBase $ textShowClassNameTable tsClass

-- | One of the last type variables cannot be eta-reduced (see the canEtaReduce
-- function for the criteria it would have to meet).
etaReductionError :: Type -> a
etaReductionError instanceType = error $
    "Cannot eta-reduce to an instance of form \n\tinstance (...) => "
    ++ pprint instanceType

-- | The data type has a DatatypeContext which mentions one of the eta-reduced
-- type variables.
datatypeContextError :: TextShowClass -> Type -> a
datatypeContextError tsClass instanceType = error
    . showString "Can't make a derived instance of ‘"
    . showString (pprint instanceType)
    . showString "‘:\n\tData type ‘"
    . showString className
    . showString "‘ must not have a class context involving the last type argument(s)"
    $ ""
  where
    className :: String
    className = nameBase $ textShowClassNameTable tsClass

-- | The data type mentions one of the n eta-reduced type variables in a place other
-- than the last nth positions of a data type in a constructor's field.
outOfPlaceTyVarError :: String -> [NameBase] -> Int -> a
outOfPlaceTyVarError conName tyVarNames numLastArgs = error
    . showString "Constructor ‘"
    . showString conName
    . showString "‘ must use the type variable"
    . plural id (showChar 's')
    . showString " "
    . showsPrec 0 tyVarNames
    . showString " only in the last "
    . plural id (showsPrec 0 numLastArgs)
    . showString "argument"
    . plural id (showChar 's')
    . showString " of a data type"
    $ ""
    where
      plural :: ShowS -> ShowS -> ShowS
      plural one many = case numLastArgs of
          1 -> one
          _ -> many

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
expandSyn (SigT t _)          = expandSyn t   -- Ignore kind synonyms
expandSyn t                   = return t

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
                rhs' = subst subs rhs
             in expandSynApp rhs' ts''
        _ -> return $ foldl' AppT t ts
expandSynApp t ts = do
    t' <- expandSyn t
    return $ foldl' AppT t' ts

type Subst = Map Name Type

mkSubst :: [TyVarBndr] -> [Type] -> Subst
mkSubst vs ts =
   let vs' = map un vs
       un (PlainTV v)    = v
       un (KindedTV v _) = v
   in Map.fromList $ zip vs' ts

subst :: Subst -> Type -> Type
subst subs (ForallT v c t) = ForallT v c $ subst subs t
subst subs t@(VarT n)      = fromMaybe t $ Map.lookup n subs
subst subs (AppT t1 t2)    = AppT (subst subs t1) (subst subs t2)
subst subs (SigT t k)      = SigT (subst subs t) k
subst _ t                  = t

-------------------------------------------------------------------------------
-- Class-specific constants
-------------------------------------------------------------------------------

-- | A representation of which TextShow variant is being derived.
data TextShowClass = TextShow | TextShow1 | TextShow2
  deriving (Enum, Eq, Ord)

showbPrecConstNameTable :: TextShowClass -> Name
showbPrecConstNameTable TextShow  = 'showbPrecConst
showbPrecConstNameTable TextShow1 = 'showbPrecWithConst
showbPrecConstNameTable TextShow2 = 'showbPrecWith2Const

textShowClassNameTable :: TextShowClass -> Name
textShowClassNameTable TextShow  = ''TextShow
textShowClassNameTable TextShow1 = ''TextShow1
textShowClassNameTable TextShow2 = ''TextShow2

showbPrecNameTable :: TextShowClass -> Name
showbPrecNameTable TextShow  = 'showbPrec
showbPrecNameTable TextShow1 = 'showbPrecWith
showbPrecNameTable TextShow2 = 'showbPrecWith2

-- | A type-restricted version of 'const'. This is useful when generating the lambda
-- expression in 'makeShowbPrec' for a data type with only nullary constructors (since
-- the expression wouldn't depend on the precedence). For example, if you had @data
-- Nullary = Nullary@ and attempted to run @$(makeShowbPrec ''Nullary) Nullary@, simply
-- ignoring the precedence argument would cause the type signature of @$(makeShowbPrec
-- ''Nullary)@ to be @a -> Nullary -> Builder@, not @Int -> Nullary -> Builder@.
showbPrecConst :: Builder -> Int -> a -> Builder
showbPrecConst = const . const
{-# INLINE showbPrecConst #-}

showbPrecWithConst :: Builder -> (Int -> a -> Builder) -> Int -> f a -> Builder
showbPrecWithConst = const . const . const
{-# INLINE showbPrecWithConst #-}

showbPrecWith2Const :: Builder -> (Int -> a -> Builder) -> (Int -> b -> Builder)
                    -> Int -> f a b -> Builder
showbPrecWith2Const = const . const . const . const
{-# INLINE showbPrecWith2Const #-}

-------------------------------------------------------------------------------
-- NameBase
-------------------------------------------------------------------------------

-- | A wrapper around Name which only uses the 'nameBase' (not the entire Name)
-- to compare for equality. For example, if you had two Names a_123 and a_456,
-- they are not equal as Names, but they are equal as NameBases.
--
-- This is useful when inspecting type variables, since a type variable in an
-- instance context may have a distinct Name from a type variable within an
-- actual constructor declaration, but we'd want to treat them as the same
-- if they have the same 'nameBase' (since that's what the programmer uses to
-- begin with).
newtype NameBase = NameBase { getName :: Name }

getNameBase :: NameBase -> String
getNameBase = nameBase . getName

instance Eq NameBase where
    (==) = (==) `on` getNameBase

instance Ord NameBase where
    compare = compare `on` getNameBase

instance Show NameBase where
    showsPrec p = showsPrec p . getNameBase

-- | A NameBase paired with the name of its show function. For example, in a
-- TextShow2 declaration, a list of TyVarInfos might look like [(a, 'sp1), (b, 'sp2)].
type TyVarInfo = (NameBase, Name)

-------------------------------------------------------------------------------
-- Assorted utilities
-------------------------------------------------------------------------------

-- | Generate a list of fresh names with a common prefix, and numbered suffixes.
newNameList :: String -> Int -> Q [Name]
newNameList prefix n = mapM (newName . (prefix ++) . show) [1..n]

-- | Remove any occurrences of a forall-ed type variable from a list of @TyVarInfo@s.
removeForalled :: [TyVarBndr] -> [TyVarInfo] -> [TyVarInfo]
removeForalled tvbs = filter (not . foralled tvbs)
  where
    foralled :: [TyVarBndr] -> TyVarInfo -> Bool
    foralled tvbs' tvi = fst tvi `elem` map (NameBase . tvbName) tvbs'

-- | Checks if a 'Name' represents a tuple type constructor (other than '()')
isNonUnitTuple :: Name -> Bool
isNonUnitTuple = isTupleString . nameBase

-- | Parenthesize an infix constructor name if it is being applied as a prefix
-- function (e.g., data Amp a = (:&) a a)
parenInfixConName :: Name -> ShowS
parenInfixConName conName =
    let conNameBase = nameBase conName
     in showParen (isInfixTypeCon conNameBase) $ showString conNameBase

-- | Extracts the name from a TyVarBndr.
tvbName :: TyVarBndr -> Name
tvbName (PlainTV  name)   = name
tvbName (KindedTV name _) = name

-- | Extracts the kind from a TyVarBndr.
tvbKind :: TyVarBndr -> Kind
tvbKind (PlainTV  _)   = starK
tvbKind (KindedTV _ k) = k

-- | Replace the Name of a TyVarBndr with one from a Type (if the Type has a Name).
replaceTyVarName :: TyVarBndr -> Type -> TyVarBndr
replaceTyVarName tvb            (SigT t _) = replaceTyVarName tvb t
replaceTyVarName (PlainTV  _)   (VarT n)   = PlainTV  n
replaceTyVarName (KindedTV _ k) (VarT n)   = KindedTV n k
replaceTyVarName tvb            _          = tvb

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
    && allDistinct nbs -- Make sure not to pass something of type [Type], since Type
                       -- didn't have an Ord instance until template-haskell-2.10.0.0
    && not (any (`mentionsNameBase` nbs) remaining)
  where
    nbs :: [NameBase]
    nbs = map varTToNameBase dropped

-- | Extract the Name from a type variable.
varTToName :: Type -> Name
varTToName (VarT n)   = n
varTToName (SigT t _) = varTToName t
varTToName _          = error "Not a type variable!"

-- | Extract the NameBase from a type variable.
varTToNameBase :: Type -> NameBase
varTToNameBase = NameBase . varTToName

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
#if MIN_VERSION_template_haskell(2,7,0)
         FamilyI (FamilyD TypeFam _ _ _) _ -> True
#else
         TyConI  (FamilyD TypeFam _ _ _)   -> True
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

-- | Does the given type mention any of the NameBases in the list?
mentionsNameBase :: Type -> [NameBase] -> Bool
mentionsNameBase = go Set.empty
  where
    go :: Set NameBase -> Type -> [NameBase] -> Bool
    go foralls (ForallT tvbs _ t) nbs =
        go (foralls `Set.union` Set.fromList (map (NameBase . tvbName) tvbs)) t nbs
    go foralls (AppT t1 t2) nbs = go foralls t1 nbs || go foralls t2 nbs
    go foralls (SigT t _)   nbs = go foralls t nbs
    go foralls (VarT n)     nbs = varNb `elem` nbs && not (varNb `Set.member` foralls)
      where
        varNb = NameBase n
    go _       _            _   = False

-- | Does an instance predicate mention any of the NameBases in the list?
predMentionsNameBase :: Pred -> [NameBase] -> Bool
#if MIN_VERSION_template_haskell(2,10,0)
predMentionsNameBase = mentionsNameBase
#else
predMentionsNameBase (ClassP _ tys) nbs = any (`mentionsNameBase` nbs) tys
predMentionsNameBase (EqualP t1 t2) nbs = mentionsNameBase t1 nbs || mentionsNameBase t2 nbs
#endif

-- | The number of arrows that compose the spine of a kind signature
-- (e.g., (* -> *) -> k -> * has two arrows on its spine).
numKindArrows :: Kind -> Int
numKindArrows k = length (uncurryKind k) - 1

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
    go (AppT t1 t2) = t2 <| go t1
    go (SigT t _)   = go t
    go t            = t :| []

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
uncurryTy t                          = t :| []

-- | Like uncurryType, except on a kind level.
uncurryKind :: Kind -> NonEmpty Kind
#if MIN_VERSION_template_haskell(2,8,0)
uncurryKind = uncurryTy
#else
uncurryKind (ArrowK k1 k2) = k1 <| uncurryKind k2
uncurryKind k              = k :| []
#endif

wellKinded :: [Kind] -> Bool
wellKinded = all canRealizeKindStar

-- | Of form k1 -> k2 -> ... -> kn, where k is either a single kind variable or *.
canRealizeKindStarChain :: Kind -> Bool
canRealizeKindStarChain = all canRealizeKindStar . uncurryKind

canRealizeKindStar :: Kind -> Bool
canRealizeKindStar k = case uncurryKind k of
    k' :| [] -> case k' of
#if MIN_VERSION_template_haskell(2,8,0)
                     StarT    -> True
                     (VarT _) -> True -- Kind k can be instantiated with *
#else
                     StarK    -> True
#endif
                     _ -> False
    _ -> False

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

# if MIN_VERSION_template_haskell(2,8,0) && __GLASGOW_HASKELL__ < 710
distinctKindVars :: Kind -> Set Name
distinctKindVars (AppT k1 k2) = distinctKindVars k1 `Set.union` distinctKindVars k2
distinctKindVars (SigT k _)   = distinctKindVars k
distinctKindVars (VarT k)     = Set.singleton k
distinctKindVars _            = Set.empty
#endif

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
tvbToType :: TyVarBndr -> Type
tvbToType (PlainTV n)    = VarT n
tvbToType (KindedTV n k) = SigT (VarT n) k
#endif

#if MIN_VERSION_template_haskell(2,7,0)
-- | Extracts the name of a constructor.
constructorName :: Con -> Name
constructorName (NormalC name      _  ) = name
constructorName (RecC    name      _  ) = name
constructorName (InfixC  _    name _  ) = name
constructorName (ForallC _    _    con) = constructorName con
#endif
