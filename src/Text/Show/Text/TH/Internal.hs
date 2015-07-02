{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-|
Module:      Text.Show.Text.TH.Internal
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Functions to mechanically derive 'T.Show', 'Show1', or 'Show2' instances, or to
splice @show@-related expressions into Haskell source code. You need to enable
the @TemplateHaskell@ language extension in order to use this module.

This implementation is loosely based off of the @Data.Aeson.TH@ module from the
@aeson@ library.
-}
module Text.Show.Text.TH.Internal (
      -- * 'deriveShow'
      -- $deriveShow
      deriveShow
      -- * 'deriveShow1'
      -- $deriveShow1
    , deriveShow1
      -- * 'deriveShow2'
      -- $deriveShow2
    , deriveShow2
      -- * @mk@ functions
      -- $mk
    , mkShow
    , mkShowLazy
    , mkShowPrec
    , mkShowPrecLazy
    , mkShowList
    , mkShowListLazy
    , mkShowb
    , mkShowbPrec
    , mkShowbList
    , mkPrint
    , mkPrintLazy
    , mkHPrint
    , mkHPrintLazy
    , mkShowbPrecWith
    , mkShowbPrec1
    , mkShowbPrecWith2
    , mkShowbPrec2
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
import           Prelude.Compat hiding (Show)

import qualified Text.Show as S (Show(show))
import qualified Text.Show.Text.Classes as T
import           Text.Show.Text.Classes (Show1(..), Show2(..), showbPrec,
                                         showbListWith, showbParen, showbSpace)
import           Text.Show.Text.Utils (isInfixTypeCon, isTupleString)

-------------------------------------------------------------------------------
-- User-facing API
-------------------------------------------------------------------------------

{- $deriveShow

'deriveShow' automatically generates a 'T.Show' instance declaration for a @data@
type, a @newtype@, or a data family instance. This emulates what would
(hypothetically) happen if you could attach a @deriving 'T.Show'@ clause to the end
of a data declaration.

Here are some examples of how to derive simple data types:

@
&#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
import Text.Show.Text.TH (deriveShow)

data Letter = A | B | C
$('deriveShow' ''Letter) -- instance Show Letter where ...

newtype Box a = Box a
$('deriveShow' ''Box)    -- instance Show a => Show (Box a) where ...
@

If you are using @template-haskell-2.7.0.0@ or later, 'deriveShow' can also be used
to derive 'T.Show' instances for data family instances (which requires the
@-XTypeFamilies@ extension). To do so, pass the name of a @data instance@ or @newtype
instance@ constructor to 'deriveShow'.  Note that the generated code may require the
@-XFlexibleInstances@ extension. Some examples:

@
&#123;-&#35; LANGUAGE FlexibleInstances, TemplateHaskell, TypeFamilies &#35;-&#125;
import Text.Show.Text.TH (deriveShow)

class AssocClass a where
    data AssocData a
instance AssocClass Int where
    data AssocData Int = AssocDataInt1 Int | AssocDataInt2 Int Int
$('deriveShow' 'AssocDataInt1) -- instance Show (AssocData Int) where ...
-- Alternatively, one could use $(deriveShow 'AssocDataInt2)

data family DataFam a b
newtype instance DataFam () b = DataFamB b
$('deriveShow' 'DataFamB)      -- instance Show b => Show (DataFam () b)
@

Note that at the moment, there are some limitations:

* The 'Name' argument to 'deriveShow' must not be a type synonym.

* 'deriveShow' makes the assumption that all type variables of kind @*@ require a
  'T.Show' constraint when creating the type context. For example, if you have @data
  Phantom a = Phantom@, then @('deriveShow' ''Phantom)@ will generate @instance
  'T.Show' a => 'T.Show' (Phantom a) where@, even though @'T.Show' a@ is not required.
  If you want a proper 'T.Show' instance for @Phantom@, you will need to use
  'mkShowbPrec' (see the documentation of the 'mk' functions for more information).

* 'deriveShow' lacks the ability to properly detect data types with higher-kinded
   type parameters (e.g., @data HK f a = HK (f a)@) or with kinds other than @*@
   (e.g., @data List a (empty :: Bool)@). If you wish to derive 'T.Show'
   instances for these data types, you will need to use 'mkShowbPrec' (see the
   documentation of the 'mk' functions for more information).

* Some data constructors have arguments whose 'T.Show' instance depends on a
  typeclass besides 'T.Show'. For example, consider @newtype MyFixed a = MyFixed
  (Fixed a)@. @'Fixed' a@ is a 'T.Show' instance only if @a@ is an instance of both
  @HasResolution@ and 'T.Show'. Unfortunately, 'deriveShow' cannot infer that 'a' must
  be an instance of 'HasResolution', so it cannot create a 'T.Show' instance for
  @MyFixed@. However, you can use 'mkShowbPrec' to get around this (see the
  documentation of the 'mk' functions for more information).

-}

-- | Generates a 'T.Show' instance declaration for the given data type or data
-- family instance.
--
-- /Since: 0.3/
deriveShow :: Name -> Q [Dec]
deriveShow = deriveShowClass Show

{- $deriveShow1

'deriveShow1' automatically generates a 'Show1' instance declaration for a @data@
type, a @newtype@, or a data family instance that has at least one type variable.
This emulates what would (hypothetically) happen if you could attach a @deriving
'Show1'@ clause to the end of a data declaration. Examples:

@
&#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
import Text.Show.Text.TH (deriveShow1)

data Stream a = Stream a (Stream a)
$('deriveShow1' ''Stream)         -- instance Show1 Stream where ...

newtype WrappedFunctor f a = WrapFunctor (f a)
$('deriveShow1' ''WrappedFunctor) -- instance Show1 f => Show1 (WrappedFunctor f) where ...
@

The same restrictions that apply to 'deriveShow' also apply to 'deriveShow1', with
some caveats:

* With 'deriveShow1', the last type variable must be of kind @*@. For other ones, type
  variables of kind @*@ are assumed to require a 'T.Show' context, and type variables
  of kind @* -> *@ are assumed to require a 'Show1' context. For more complicated
  scenarios, use 'mkShowbPrecWith'.

* If using @DatatypeContexts@, a datatype constraint cannot mention the last type
  variable. For example, @data Show a => Illegal a = Illegal a@ cannot have a derived
  'Show1' instance.

* If the last type variable is used within a data field of a constructor, it must only
  be used in the last argument of the data type constructor. For example, @data Legal a
  = Legal (Either Int a)@ can have a derived 'Show1' instance, but @data Illegal a =
  Illegal (Either a a)@ cannot.

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
  $(deriveShow1 'Foo)
  @

  To avoid this issue, it is recommened that you use the same type variables in the
  same positions in which they appeared in the data family declaration:

  @
  data family Foo a b c
  data instance Foo Int b c = Foo Int b c
  $(deriveShow 'Foo)
  @

-}

-- | Generates a 'Show1' instance declaration for the given data type or data
-- family instance.
--
-- /Since: 1/
deriveShow1 :: Name -> Q [Dec]
deriveShow1 = deriveShowClass Show1

{- $deriveShow2

'deriveShow2' automatically generates a 'Show2' instance declaration for a @data@
type, a @newtype@, or a data family instance that has at least two type variables.
This emulates what would (hypothetically) happen if you could attach a @deriving
'Show2'@ clause to the end of a data declaration. Examples:

@
&#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
import Text.Show.Text.TH (deriveShow2)

data OneOrNone a b = OneL a | OneR b | None
$('deriveShow2' ''OneOrNone)        -- instance Show2 OneOrNone where ...

newtype WrappedBifunctor f a b = WrapBifunctor (f a b)
$('deriveShow2' ''WrappedBifunctor) -- instance Show2 f => Show2 (WrappedBifunctor f) where ...
@

The same restrictions that apply to 'deriveShow' and 'deriveShow1' also apply to
'deriveShow2', with some caveats:

* With 'deriveShow2', the last type variables must both be of kind @*@. For other ones,
  type variables of kind @*@ are assumed to require a 'T.Show' constraint, type
  variables of kind @* -> *@ are assumed to require a 'Show1' constraint, and type
  variables of kind @* -> * -> *@ are assumed to require a 'Show2' constraint. For more
  complicated scenarios, use 'mkShowbPrecWith2'.

* If using @DatatypeContexts@, a datatype constraint cannot mention either of the last
  two type variables. For example, @data Show a => Illegal a b = Illegal a b@ cannot
  have a derived 'Show2' instance.

* If either of the last two type variables is used within a data field of a constructor,
  it must only be used in the last two arguments of the data type constructor. For
  example, @data Legal a b = Legal (Int, Int, a, b)@ can have a derived 'Show2'
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

-- | Generates a 'Show2' instance declaration for the given data type or data
-- family instance.
--
-- /Since: 1/
deriveShow2 :: Name -> Q [Dec]
deriveShow2 = deriveShowClass Show2

{- $mk

There may be scenarios in which you want to show an arbitrary data type or data
family instance without having to make the type an instance of 'T.Show'. For these
cases, "Text.Show.Text.TH" provide several functions (all prefixed with @mk@) that
splice the appropriate lambda expression into your source code.

As an example, suppose you have @data ADT = ADT@, which is not an instance of 'T.Show'.
With @mkShow@, you can still convert it to 'Text':

@
&#123;-&#35; LANGUAGE OverloadedStrings, TemplateHaskell &#35;-&#125;
import Text.Show.Text.TH (mkShow)

whichADT :: Bool
whichADT = $(mkShow ''ADT) ADT == \"ADT\"
@

'mk' functions are also useful for creating 'T.Show' instances for data types with
sophisticated type parameters. For example, 'deriveShow' cannot infer the correct type
context for @newtype HigherKinded f a = HigherKinded (f a)@, since @f@ is a
higher-kinded type parameter. However, it is still possible to derive a 'T.Show'
instance for @HigherKinded@ without too much trouble using 'mkShowbPrec':

@
&#123;-&#35; LANGUAGE FlexibleContexts, TemplateHaskell &#35;-&#125;
import Prelude hiding (Show)
import Text.Show.Text (Show(showbPrec))
import Text.Show.Text.TH (mkShowbPrec)

instance Show (f a) => Show (HigherKinded f a) where
    showbPrec = $(mkShowbPrec ''HigherKinded)
@

-}

-- | Generates a lambda expression which behaves like 'T.show' (without requiring a
-- 'T.Show' instance).
--
-- /Since: 0.3.1/
mkShow :: Name -> Q Exp
mkShow name = [| toStrict . $(mkShowLazy name) |]

-- | Generates a lambda expression which behaves like 'T.showLazy' (without requiring a
-- 'T.Show' instance).
--
-- /Since: 0.3.1/
mkShowLazy :: Name -> Q Exp
mkShowLazy name = [| toLazyText . $(mkShowb name) |]

-- | Generates a lambda expression which behaves like 'T.showPrec' (without requiring a
-- 'T.Show' instance).
--
-- /Since: 0.3.1/
mkShowPrec :: Name -> Q Exp
mkShowPrec name = [| \p -> toStrict . $(mkShowPrecLazy name) p |]

-- | Generates a lambda expression which behaves like 'T.showPrecLazy' (without
-- requiring a 'T.Show' instance).
--
-- /Since: 0.3.1/
mkShowPrecLazy :: Name -> Q Exp
mkShowPrecLazy name = [| \p -> toLazyText . $(mkShowbPrec name) p |]

-- | Generates a lambda expression which behaves like 'T.showList' (without requiring a
-- 'T.Show' instance).
--
-- /Since: 0.5/
mkShowList :: Name -> Q Exp
mkShowList name = [| toStrict . $(mkShowListLazy name) |]

-- | Generates a lambda expression which behaves like 'T.showListLazy' (without
-- requiring a 'T.Show' instance).
--
-- /Since: 0.5/
mkShowListLazy :: Name -> Q Exp
mkShowListLazy name = [| toLazyText . $(mkShowbList name) |]

-- | Generates a lambda expression which behaves like 'T.showb' (without requiring a
-- 'T.Show' instance).
--
-- /Since: 0.3.1/
mkShowb :: Name -> Q Exp
mkShowb name = mkShowbPrec name `appE` [| zero |]
  where
    -- To prevent the generated TH code from having a type ascription
    zero :: Int
    zero = 0

-- | Generates a lambda expression which behaves like 'T.showPrec' (without requiring a
-- 'T.Show' instance).
--
-- /Since: 0.3.1/
mkShowbPrec :: Name -> Q Exp
mkShowbPrec = mkShowbPrecClass Show

-- | Generates a lambda expression which behaves like 'T.showbPrecWith' (without
-- requiring a 'Show1' instance).
--
-- /Since: 1/
mkShowbPrecWith :: Name -> Q Exp
mkShowbPrecWith = mkShowbPrecClass Show1

-- | Generates a lambda expression which behaves like 'T.showbPrec1' (without
-- requiring a 'Show1' instance).
--
-- /Since: 1/
mkShowbPrec1 :: Name -> Q Exp
mkShowbPrec1 name = [| $(mkShowbPrecWith name) showbPrec |]

-- | Generates a lambda expression which behaves like 'T.showbPrecWith2' (without
-- requiring a 'Show2' instance).
--
-- /Since: 1/
mkShowbPrecWith2 :: Name -> Q Exp
mkShowbPrecWith2 = mkShowbPrecClass Show2

-- | Generates a lambda expression which behaves like 'T.showbPrecWith2' (without
-- requiring a 'Show2' instance).
--
-- /Since: 1/
mkShowbPrec2 :: Name -> Q Exp
mkShowbPrec2 name = [| $(mkShowbPrecWith2 name) showbPrec showbPrec |]

-- | Generates a lambda expression which behaves like 'T.showbList' (without requiring a
-- 'T.Show' instance).
--
-- /Since: 0.5/
mkShowbList :: Name -> Q Exp
mkShowbList name = [| showbListWith $(mkShowb name) |]

-- | Generates a lambda expression which behaves like 'T.print' (without requiring a
-- 'T.Show' instance).
--
-- /Since: 0.3.1/
mkPrint :: Name -> Q Exp
mkPrint name = [| TS.putStrLn . $(mkShow name) |]

-- | Generates a lambda expression which behaves like 'T.printLazy' (without requiring a
-- 'T.Show' instance).
--
-- /Since: 0.3.1/
mkPrintLazy :: Name -> Q Exp
mkPrintLazy name = [| TL.putStrLn . $(mkShowLazy name) |]

-- | Generates a lambda expression which behaves like 'T.hPrint' (without requiring a
-- 'T.Show' instance).
--
-- /Since: 0.3.1/
mkHPrint :: Name -> Q Exp
mkHPrint name = [| \h -> TS.hPutStrLn h . $(mkShow name) |]

-- | Generates a lambda expression which behaves like 'T.hPrintLazy' (without
-- requiring a 'T.Show' instance).
--
-- /Since: 0.3.1/
mkHPrintLazy :: Name -> Q Exp
mkHPrintLazy name = [| \h -> TL.hPutStrLn h . $(mkShowLazy name) |]

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

-- | Derive a Show/Show1/Show2 instance declaration (depending on the ShowClass
-- argument's value).
deriveShowClass :: ShowClass -> Name -> Q [Dec]
deriveShowClass sClass tyConName = do
    info <- reify tyConName
    case info of
        TyConI{} -> deriveShowPlainTy sClass tyConName
#if MIN_VERSION_template_haskell(2,7,0)
        DataConI{} -> deriveShowDataFamInst sClass tyConName
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
    ns = "Text.Show.Text.TH.deriveShow: "

-- | Generates a Show/Show1/Show2 instance declaration for a plain type constructor.
deriveShowPlainTy :: ShowClass -> Name -> Q [Dec]
deriveShowPlainTy sClass tyConName =
    withTyCon tyConName fromCons
  where
    className :: Name
    className = showClassNameTable sClass

    fromCons :: Cxt -> [TyVarBndr] -> [Con] -> Q [Dec]
    fromCons ctxt tvbs cons = (:[]) <$>
        instanceD (return instanceCxt)
                  (return $ AppT (ConT className) instanceType)
                  (showbPrecDecs droppedNbs cons)
      where
        (instanceCxt, instanceType, droppedNbs) =
            cxtAndTypePlainTy sClass tyConName ctxt tvbs

#if MIN_VERSION_template_haskell(2,7,0)
-- | Generates a Show/Show1/Show2 instance declaration for a data family instance
-- constructor.
deriveShowDataFamInst :: ShowClass -> Name -> Q [Dec]
deriveShowDataFamInst sClass dataFamInstName =
    withDataFamInstCon dataFamInstName fromDec
  where
    className :: Name
    className = showClassNameTable sClass

    fromDec :: [TyVarBndr] -> Cxt -> Name -> [Type] -> [Con] -> Q [Dec]
    fromDec famTvbs ctxt parentName instTys cons = (:[]) <$>
        instanceD (return instanceCxt)
                  (return $ AppT (ConT className) instanceType)
                  (showbPrecDecs droppedNbs cons)
      where
        (instanceCxt, instanceType, droppedNbs) =
            cxtAndTypeDataFamInstCon sClass parentName ctxt famTvbs instTys
#endif

-- | Generates a declaration defining the primary function corresponding to a
-- particular class (showbPrec for Show, showbPrecWith for Show1, and showbPrecWith2
-- for Show2).
showbPrecDecs :: [NameBase] -> [Con] -> [Q Dec]
showbPrecDecs nbs cons =
    [ funD classFuncName
           [ clause []
                    (normalB $ mkShowForCons nbs cons)
                    []
           ]
    ]
  where
    classFuncName :: Name
    classFuncName  = showbPrecNameTable . toEnum $ length nbs

-- | Generates a lambda expression which behaves like showbPrec (for Show),
-- showbPrecWith (for Show1), or showbPrecWth2 (for Show2).
mkShowbPrecClass :: ShowClass -> Name -> Q Exp
mkShowbPrecClass sClass tyConName = do
    info <- reify tyConName
    case info of
        TyConI{} -> withTyCon tyConName $ \ctxt tvbs decs ->
            let (_, _, nbs) = cxtAndTypePlainTy sClass tyConName ctxt tvbs
             in mkShowForCons nbs decs
#if MIN_VERSION_template_haskell(2,7,0)
        DataConI{} -> withDataFamInstCon tyConName $ \famTvbs ctxt parentName instTys cons ->
            let (_, _, nbs) = cxtAndTypeDataFamInstCon sClass parentName ctxt famTvbs instTys
             in mkShowForCons nbs cons
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
    ns = "Text.Show.Text.TH.mkShowbPrec: "

-- | Generates a lambda expression for showbPrec(With)(2) for the given constructors.
-- All constructors must be from the same type.
mkShowForCons :: [NameBase] -> [Con] -> Q Exp
mkShowForCons _   []   = error "Must have at least one data constructor"
mkShowForCons nbs cons = do
    p     <- newName "p"
    value <- newName "value"
    sps   <- newNameList "sp" $ length nbs
    let tvis   = zip nbs sps
        sClass = toEnum $ length nbs
    lamE (map varP $ sps ++ [p, value])
        . appsE
        $ [ varE $ showbPrecConstNameTable sClass
          , caseE (varE value) $ map (mkShowForCon p sClass tvis) cons
          ] ++ map varE sps
            ++ [varE p, varE value]

-- | Generates a lambda expression for showbPrec(With)(2) for a single constructor.
mkShowForCon :: Name -> ShowClass -> [TyVarInfo] -> Con -> Q Match
mkShowForCon _ _ _ (NormalC conName [])
    = match (conP conName [])
            (normalB [| fromString $(stringE (parenInfixConName conName "")) |])
            []
mkShowForCon p sClass tvis (NormalC conName [(_, argTy)]) = do
    arg <- newName "arg"

    let showArg  = mkShowForArg appPrec1 sClass (nameBase conName) tvis argTy arg
        namedArg = [| fromString $(stringE (parenInfixConName conName " ")) <> $(showArg) |]

    match (conP conName [varP arg])
          (normalB [| showbParen ($(varE p) > $(lift appPrec)) $(namedArg) |])
          []
mkShowForCon p sClass tvis (NormalC conName ts) = do
    args <- newNameList "arg" $ length ts

    if isNonUnitTuple conName
       then do
           let showArgs       = map (\(arg, (_, argTy)) -> mkShowForArg 0 sClass (nameBase conName) tvis argTy arg)
                                    (zip args ts)
               parenCommaArgs = [| singleton '(' |] : intersperse [| singleton ',' |] showArgs
               mappendArgs    = foldr (`infixApp` [| (<>) |])
                                      [| singleton ')' |]
                                      parenCommaArgs

           match (conP conName $ map varP args)
                 (normalB mappendArgs)
                 []
       else do
           let showArgs = map (\(arg, (_, argTy)) -> mkShowForArg appPrec1 sClass (nameBase conName) tvis argTy arg)
                              (zip args ts)
               mappendArgs = foldr1 (\v q -> [| $(v) <> showbSpace <> $(q) |]) showArgs
               namedArgs   = [| fromString $(stringE (parenInfixConName conName " ")) <> $(mappendArgs) |]

           match (conP conName $ map varP args)
                 (normalB [| showbParen ($(varE p) > $(lift appPrec)) $(namedArgs) |])
                 []
mkShowForCon p sClass tvis (RecC conName []) = mkShowForCon p sClass tvis $ NormalC conName []
mkShowForCon _p sClass tvis (RecC conName ts) = do
    args <- newNameList "arg" $ length ts

    let showArgs       = concatMap (\(arg, (argName, _, argTy))
                                      -> [ [| fromString $(stringE (nameBase argName ++ " = ")) |]
                                         , mkShowForArg 0 sClass (nameBase conName) tvis argTy arg
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
mkShowForCon p sClass tvis (InfixC (_, alTy) conName (_, arTy)) = do
    al   <- newName "argL"
    ar   <- newName "argR"
    info <- reify conName

    let conPrec  = case info of
                        DataConI _ _ _ (Fixity prec _) -> prec
                        other -> error $ "Text.Show.Text.TH.mkShowForCon: Unsupported type: " ++ S.show other
        opName   = nameBase conName
        infixOpE = if isInfixTypeCon opName
                      then [| fromString $(stringE $ " "  ++ opName ++ " " ) |]
                      else [| fromString $(stringE $ " `" ++ opName ++ "` ") |]

    match (infixP (varP al) conName (varP ar))
          (normalB $ appE [| showbParen ($(varE p) > conPrec) |]
                          [| $(mkShowForArg (conPrec + 1) sClass opName tvis alTy al)
                          <> $(infixOpE)
                          <> $(mkShowForArg (conPrec + 1) sClass opName tvis arTy ar)
                          |]
          )
          []
mkShowForCon p sClass tvis (ForallC tvbs _ con) = mkShowForCon p sClass (removeForalled tvbs tvis) con

-- | Generates a lambda expression for showbPrec(With)(2) for an argument of a
-- constructor.
mkShowForArg :: Int
            -> ShowClass
            -> String
            -> [TyVarInfo]
            -> Type
            -> Name
            -> Q Exp
mkShowForArg p sClass conName tvis ty tyExpName = do
    ty' <- expandSyn ty
    mkShowForArg' p sClass conName tvis ty' tyExpName

-- | Generates a lambda expression for showbPrec(With)(2) for an argument of a
-- constructor, after expanding all type synonyms.
mkShowForArg' :: Int
              -> ShowClass
              -> String
              -> [TyVarInfo]
              -> Type
              -> Name
              -> Q Exp
mkShowForArg' p _ _ _ (ConT tyName) tyExpName =
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
mkShowForArg' p sClass conName tvis ty tyExpName =
    [| $(mkShowForType sClass conName tvis ty) p $(varE tyExpName) |]

-- | Generates a lambda expression for showbPrec(With)(2) for a specific type.
-- The generated expression depends on the number of type variables.
--
-- 1. If the type is of kind * (T), apply showbPrec.
-- 2. If the type is of kind * -> * (T a), apply showbPrecWith $(mkShowbForType a)
-- 3. If the type is of kind * -> * -> * (T a b), apply
--    showbPrecWith2 $(mkShowbForType a) $(mkShowbForType b)
mkShowForType :: ShowClass
              -> String
              -> [TyVarInfo]
              -> Type
              -> Q Exp
mkShowForType _ _ tvis (VarT tyName) =
    case lookup (NameBase tyName) tvis of
         Just spExp -> varE spExp
         Nothing    -> [| showbPrec |]
mkShowForType sClass conName tvis (SigT ty _)         = mkShowForType sClass conName tvis ty
mkShowForType sClass conName tvis (ForallT tvbs _ ty) = mkShowForType sClass conName (removeForalled tvbs tvis) ty
mkShowForType sClass conName tvis ty = do
    let tyArgs :: [Type]
        tyCon :| tyArgs = unapplyTy ty

        numLastArgs :: Int
        numLastArgs = min (fromEnum sClass) (length tyArgs)

        lhsArgs, rhsArgs :: [Type]
        (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

        tyVarNameBases :: [NameBase]
        tyVarNameBases = map fst tvis

    itf <- isTyFamily tyCon
    if any (`mentionsNameBase` tyVarNameBases) lhsArgs
          || itf && any (`mentionsNameBase` tyVarNameBases) tyArgs
       then outOfPlaceTyVarError conName tyVarNameBases numLastArgs
       else appsE $ [ varE . showbPrecNameTable $ toEnum numLastArgs]
                    ++ map (mkShowForType sClass conName tvis) rhsArgs

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
                other -> error $ ns ++ "Unsupported type " ++ S.show other ++ ". Must be a data type or newtype."
        _ -> error $ ns ++ "The name must be of a plain type constructor."
  where
    ns :: String
    ns = "Text.Show.Text.TH.withTyCon: "

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
        other -> error $ ns ++ "Unsupported type " ++ S.show other ++ ". Must be a data family name."
  where
    ns :: String
    ns = "Text.Show.Text.TH.withDataFam: "

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
                _ -> error $ ns ++ "Data constructor " ++ S.show dficName ++ " is not from a data family instance."
        other -> error $ ns ++ "Unsupported type " ++ S.show other ++ ". Must be a data family instance constructor."
  where
    ns :: String
    ns = "Text.Show.Text.TH.withDataFamInstCon: "
#endif

-- | Deduces the Show/Show1/Show2 instance context, instance head, and eta-reduced
-- type variables for a plain data type constructor.
cxtAndTypePlainTy :: ShowClass   -- Show, Show1, or Show2
                  -> Name        -- The datatype's name
                  -> Cxt         -- The datatype context
                  -> [TyVarBndr] -- The type variables
                  -> (Cxt, Type, [NameBase])
cxtAndTypePlainTy sClass tyConName dataCxt tvbs =
    if remainingLength < 0 || not (wellKinded droppedKinds) -- If we have enough well-kinded type variables
       then derivingKindError sClass tyConName
    else if any (`predMentionsNameBase` droppedNbs) dataCxt -- If the last type variable(s) are mentioned in a datatype context
       then datatypeContextError sClass instanceType
    else (instanceCxt, instanceType, droppedNbs)
  where
    instanceCxt :: Cxt
    instanceCxt = map (applyShowConstraint)
                $ filter (needsConstraint sClass . tvbKind) remaining

    instanceType :: Type
    instanceType = applyTyCon tyConName $ map (VarT . tvbName) remaining

    remainingLength :: Int
    remainingLength = length tvbs - fromEnum sClass

    remaining, dropped :: [TyVarBndr]
    (remaining, dropped) = splitAt remainingLength tvbs

    droppedKinds :: [Kind]
    droppedKinds = map tvbKind dropped

    droppedNbs :: [NameBase]
    droppedNbs = map (NameBase . tvbName) dropped

#if MIN_VERSION_template_haskell(2,7,0)
-- | Deduces the Show/Show1/Show2 instance context, instance head, and eta-reduced
-- type variables for a data family instnce constructor.
cxtAndTypeDataFamInstCon :: ShowClass   -- Show, Show1, or Show2
                         -> Name        -- The data family name
                         -> Cxt         -- The datatype context
                         -> [TyVarBndr] -- The data family declaration's type variables
                         -> [Type]      -- The data family instance types
                         -> (Cxt, Type, [NameBase])
cxtAndTypeDataFamInstCon sClass parentName dataCxt famTvbs instTysAndKinds =
    if remainingLength < 0 || not (wellKinded droppedKinds) -- If we have enough well-kinded type variables
       then derivingKindError sClass parentName
    else if any (`predMentionsNameBase` droppedNbs) dataCxt -- If the last type variable(s) are mentioned in a datatype context
       then datatypeContextError sClass instanceType
    else if canEtaReduce remaining dropped -- If it is safe to drop the type variables
       then (instanceCxt, instanceType, droppedNbs)
    else etaReductionError instanceType
  where
    instanceCxt :: Cxt
    instanceCxt = map (applyShowConstraint)
                $ filter (needsConstraint sClass . tvbKind) lhsTvbs

    -- We need to make sure that type variables in the instance head which have
    -- Show constrains aren't poly-kinded, e.g.,
    --
    -- @
    -- instance Show a => Show (Foo (a :: k)) where
    -- @
    --
    -- To do this, we remove every kind ascription (i.e., strip off every 'SigT').
    instanceType :: Type
    instanceType = applyTyCon parentName
                 $ map unSigT remaining

    remainingLength :: Int
    remainingLength = length famTvbs - fromEnum sClass

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
    -- as it makes it impossible to come up with the correct 'Show1' or 'Show2'
    -- instances in some cases. For example, consider the following code:
    --
    -- @
    -- data family Foo a b c
    -- data instance Foo Int y z = Foo Int y z
    -- $(deriveShow2 'Foo)
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
    -- $(deriveShow2 'Foo)
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

-- | Given a TyVarBndr, apply a Show, Show1, or Show2 constraint to it, depending
-- on its kind.
applyShowConstraint :: TyVarBndr -> Pred
applyShowConstraint (PlainTV  name)      = applyClass ''T.Show  name
applyShowConstraint (KindedTV name kind) = applyClass className name
  where
    className :: Name
    className = showClassNameTable . toEnum $ numKindArrows kind

-- | Can a kind signature inhabit a Show constraint?
--
-- Show:  Kind k
-- Show1: Kind k1 -> k2
-- Show2: Kind k1 -> k2 -> k3
needsConstraint :: ShowClass -> Kind -> Bool
needsConstraint sClass kind =
       sClass >= toEnum (numKindArrows kind)
    && canRealizeKindStarChain kind

-------------------------------------------------------------------------------
-- Error messages
-------------------------------------------------------------------------------

-- | Either the given data type doesn't have enough type variables, or one of
-- the type variables to be eta-reduced cannot realize kind *.
derivingKindError :: ShowClass -> Name -> a
derivingKindError sClass tyConName = error
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
    . showString (pprint . createKindChain $ fromEnum sClass)
    $ ""
  where
    className :: String
    className = nameBase $ showClassNameTable sClass

-- | One of the last type variables cannot be eta-reduced (see the canEtaReduce
-- function for the criteria it would have to meet).
etaReductionError :: Type -> a
etaReductionError instanceType = error $
    "Cannot eta-reduce to an instance of form \n\tinstance (...) => "
    ++ pprint instanceType

-- | The data type has a DatatypeContext which mentions one of the eta-reduced
-- type variables.
datatypeContextError :: ShowClass -> Type -> a
datatypeContextError sClass instanceType = error
    . showString "Can't make a derived instance of ‘"
    . showString (pprint instanceType)
    . showString "‘:\n\tData type ‘"
    . showString className
    . showString "‘ must not have a class context involving the last type argument(s)"
    $ ""
  where
    className :: String
    className = nameBase $ showClassNameTable sClass

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
-- until template-haskell-2.7.0.0, which is necessary for a derived Show instance
-- to work.
dataConIError :: a
dataConIError = error
    . showString "Cannot use a data constructor."
    . showString "\n\t(Note: if you are trying to derive Show for a type family,"
    . showString "\n\tuse GHC >= 7.4 instead.)"
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
       un (PlainTV v) = v
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

-- | A representation of which @Show@ variant is being derived.
data ShowClass = Show | Show1 | Show2
  deriving (Enum, Eq, Ord)

showbPrecConstNameTable :: ShowClass -> Name
showbPrecConstNameTable Show  = 'showbPrecConst
showbPrecConstNameTable Show1 = 'showbPrecWithConst
showbPrecConstNameTable Show2 = 'showbPrecWith2Const

showClassNameTable :: ShowClass -> Name
showClassNameTable Show  = ''T.Show
showClassNameTable Show1 = ''Show1
showClassNameTable Show2 = ''Show2

showbPrecNameTable :: ShowClass -> Name
showbPrecNameTable Show  = 'showbPrec
showbPrecNameTable Show1 = 'showbPrecWith
showbPrecNameTable Show2 = 'showbPrecWith2

-- | A type-restricted version of 'const'. This is useful when generating the lambda
-- expression in 'mkShowbPrec' for a data type with only nullary constructors (since
-- the expression wouldn't depend on the precedence). For example, if you had @data
-- Nullary = Nullary@ and attempted to run @$(mkShowbPrec ''Nullary) Nullary@, simply
-- ignoring the precedence argument would cause the type signature of @$(mkShowbPrec
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

instance S.Show NameBase where
    showsPrec p = showsPrec p . getNameBase

-- | A NameBase paired with the name of its show function. For example, in a
-- Show2 declaration, a list of TyVarInfos might look like [(a, 'sp1), (b, 'sp2)].
type TyVarInfo = (NameBase, Name)

-------------------------------------------------------------------------------
-- Assorted utilities
-------------------------------------------------------------------------------

-- | Generate a list of fresh names with a common prefix, and numbered suffixes.
newNameList :: String -> Int -> Q [Name]
newNameList prefix n = mapM (newName . (prefix ++) . S.show) [1..n]

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
