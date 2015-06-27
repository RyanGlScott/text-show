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
deriveShow :: Name -- ^ Name of the data type to make an instance of 'T.Show'
           -> Q [Dec]
deriveShow = deriveShowNumber Show

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

-}

-- | Generates a 'Show1' instance declaration for the given data type or data
-- family instance.
--
-- /Since: 1/
deriveShow1 :: Name -- ^ Name of the data type to make an instance of 'Show1'
            -> Q [Dec]
deriveShow1 = deriveShowNumber Show1

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

The same restrictions that apply to 'deriveShow' also apply to 'deriveShow2', with
some caveats:

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
deriveShow2 :: Name -- ^ Name of the data type to make an instance of 'Show2'
            -> Q [Dec]
deriveShow2 = deriveShowNumber Show2

deriveShowNumber :: ShowClass
                 -> Name
                 -> Q [Dec]
deriveShowNumber numToDrop tyConName = do
    info <- reify tyConName
    case info of
        TyConI{} -> deriveShowTyCon numToDrop tyConName
#if MIN_VERSION_template_haskell(2,7,0)
        DataConI{} -> deriveShowDataFamInst numToDrop tyConName
        FamilyI (FamilyD DataFam _ _ _) _ ->
            error $ ns ++ "Cannot use a data family name. Use a data family instance constructor instead."
        FamilyI (FamilyD TypeFam _ _ _) _ ->
            error $ ns ++ "Cannot use a type family name."
        _ -> error $ ns ++ "The name must be of a plain type constructor or data family instance constructor."
#else
        _ -> error $ ns ++ "The name must be of a plain type constructor."
#endif
  where
    ns :: String
    ns = "Text.Show.Text.TH.deriveShow: "

-- | Generates a 'T.Show' instance declaration for a plain type constructor.
deriveShowTyCon :: ShowClass
                -> Name
                -> Q [Dec]
deriveShowTyCon numToDrop tyConName =
    withTyCon tyConName fromCons
  where
    className :: Name
    className = showClassTable numToDrop

    fromCons :: Cxt -> [TyVarBndr] -> [Con] -> Q [Dec]
    fromCons ctxt tvbs cons = (:[]) <$>
        instanceD (return instanceCxt)
                  (return $ AppT (ConT className) instanceType)
                  (showbPrecDecs droppedNbs cons)
      where
        (instanceCxt, instanceType, droppedNbs) =
            cxtAndTypeTyCon numToDrop tyConName ctxt tvbs

#if MIN_VERSION_template_haskell(2,7,0)
-- | Generates a 'T.Show' instance declaration for a data family instance constructor.
deriveShowDataFamInst :: ShowClass
                      -> Name
                      -> Q [Dec]
deriveShowDataFamInst numToDrop dataFamInstName =
    withDataFamInstCon dataFamInstName fromDec
  where
    className :: Name
    className = showClassTable numToDrop

    fromDec :: [TyVarBndr] -> [TyVarBndr] -> Cxt -> Name -> [Type] -> [Con] -> Q [Dec]
    fromDec famTvbs instTvbs ctxt parentName tys cons = (:[]) <$>
        instanceD (return instanceCxt)
                  (return $ AppT (ConT className) instanceType)
                  (showbPrecDecs droppedNbs cons)
      where
        (instanceCxt, instanceType, droppedNbs) =
            cxtAndTypeDataFamInstCon numToDrop parentName ctxt famTvbs instTvbs tys
#endif

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
mkShowbPrec = mkShowbPrecNumber Show

-- | Generates a lambda expression which behaves like 'T.showbPrecWith' (without
-- requiring a 'Show1' instance).
--
-- /Since: 1/
mkShowbPrecWith :: Name -> Q Exp
mkShowbPrecWith = mkShowbPrecNumber Show1

-- | Generates a lambda expression which behaves like 'T.showbPrec1' (without
-- requiring a 'Show1' instance).
--
-- /Since: 1/
mkShowbPrec1 :: Name -> Q Exp
mkShowbPrec1 name = [| $(mkShowbPrecWith name) $(mkShowbPrec name) |]

-- | Generates a lambda expression which behaves like 'T.showbPrecWith2' (without
-- requiring a 'Show2' instance).
--
-- /Since: 1/
mkShowbPrecWith2 :: Name -> Q Exp
mkShowbPrecWith2 = mkShowbPrecNumber Show2

-- | Generates a lambda expression which behaves like 'T.showbPrecWith2' (without
-- requiring a 'Show2' instance).
--
-- /Since: 1/
mkShowbPrec2 :: Name -> Q Exp
mkShowbPrec2 name = [| $(mkShowbPrecWith2 name) showbPrec showbPrec |]

mkShowbPrecNumber :: ShowClass -> Name -> Q Exp
mkShowbPrecNumber numToDrop tyConName = do
    info <- reify tyConName
    case info of
        TyConI{} -> withTyCon tyConName $ \ctxt tvbs decs ->
            let (_, _, nbs) = cxtAndTypeTyCon numToDrop tyConName ctxt tvbs
             in consToShow nbs decs
#if MIN_VERSION_template_haskell(2,7,0)
        DataConI{} -> withDataFamInstCon tyConName $ \famTvbs instTvbs ctxt parentName tys cons ->
            let (_, _, nbs) = cxtAndTypeDataFamInstCon numToDrop parentName ctxt famTvbs instTvbs tys
             in consToShow nbs cons
        FamilyI (FamilyD DataFam _ _ _) _ ->
            error $ ns ++ "Cannot use a data family name. Use a data family instance constructor instead."
        FamilyI (FamilyD TypeFam _ _ _) _ ->
            error $ ns ++ "Cannot use a type family name."
        _ -> error $ ns ++ "The name must be of a plain type constructor or data family instance constructor."
#else
        _ -> error $ ns ++ "The name must be of a plain type constructor."
#endif
  where
    ns :: String
    ns = "Text.Show.Text.TH.mk: "

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

-- | Generates code to generate the 'T.Show' encoding of a number of constructors.
-- All constructors must be from the same type.
consToShow :: [NameBase] -> [Con] -> Q Exp
consToShow _   []   = error "Text.Show.Text.TH.consToShow: Not a single constructor given!"
consToShow nbs cons = do
    p     <- newName "p"
    value <- newName "value"
    sps   <- mapM newName ["sp" ++ S.show n | (_, n) <- zip nbs [(1 :: Int) ..]]
    let tvis   = zip nbs sps
        sClass = toEnum $ length nbs
    lamE (map varP $ sps ++ [p, value])
        . appsE
        $ [ varE $ constTable sClass
          , caseE (varE value) $ map (encodeArgs p sClass tvis) cons
          ] ++ map varE sps
            ++ [varE p, varE value]

-- | Generates code to generate the 'T.Show' encoding of a single constructor.
encodeArgs :: Name -> ShowClass -> [TyVarInfo] -> Con -> Q Match
encodeArgs _ _ _ (NormalC conName [])
    = match (conP conName [])
            (normalB [| fromString $(stringE (parenInfixConName conName "")) |])
            []
encodeArgs p sClass tvis (NormalC conName [(_, argTy)]) = do
    arg <- newName "arg"

    let showArg  = showbPrecTy appPrec1 sClass (nameBase conName) tvis argTy arg
        namedArg = [| fromString $(stringE (parenInfixConName conName " ")) <> $(showArg) |]

    match (conP conName [varP arg])
          (normalB [| showbParen ($(varE p) > $(lift appPrec)) $(namedArg) |])
          []
encodeArgs p sClass tvis (NormalC conName ts) = do
    args <- mapM newName ["arg" ++ S.show n | (_, n) <- zip ts [(1 :: Int) ..]]

    if isNonUnitTuple conName
       then do
           let showArgs       = map (\(arg, (_, argTy)) -> showbPrecTy 0 sClass (nameBase conName) tvis argTy arg)
                                    (zip args ts)
               parenCommaArgs = [| singleton '(' |] : intersperse [| singleton ',' |] showArgs
               mappendArgs    = foldr (`infixApp` [| (<>) |])
                                      [| singleton ')' |]
                                      parenCommaArgs

           match (conP conName $ map varP args)
                 (normalB mappendArgs)
                 []
       else do
           let showArgs = map (\(arg, (_, argTy)) -> showbPrecTy appPrec1 sClass (nameBase conName) tvis argTy arg)
                              (zip args ts)
               mappendArgs = foldr1 (\v q -> [| $(v) <> showbSpace <> $(q) |]) showArgs
               namedArgs   = [| fromString $(stringE (parenInfixConName conName " ")) <> $(mappendArgs) |]

           match (conP conName $ map varP args)
                 (normalB [| showbParen ($(varE p) > $(lift appPrec)) $(namedArgs) |])
                 []
encodeArgs p sClass tvis (RecC conName []) = encodeArgs p sClass tvis $ NormalC conName []
encodeArgs _p sClass tvis (RecC conName ts) = do
    args <- mapM newName ["arg" ++ S.show n | (_, n) <- zip ts [1 :: Int ..]]

    let showArgs       = concatMap (\(arg, (argName, _, argTy))
                                      -> [ [| fromString $(stringE (nameBase argName ++ " = ")) |]
                                         , showbPrecTy 0 sClass (nameBase conName) tvis argTy arg
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
encodeArgs p sClass tvis (InfixC (_, alTy) conName (_, arTy)) = do
    al   <- newName "argL"
    ar   <- newName "argR"
    info <- reify conName

    let conPrec  = case info of
                        DataConI _ _ _ (Fixity prec _) -> prec
                        other -> error $ "Text.Show.Text.TH.encodeArgs: Unsupported type: " ++ S.show other
        opName   = nameBase conName
        infixOpE = if isInfixTypeCon opName
                      then [| fromString $(stringE $ " "  ++ opName ++ " " ) |]
                      else [| fromString $(stringE $ " `" ++ opName ++ "` ") |]

    match (infixP (varP al) conName (varP ar))
          (normalB $ appE [| showbParen ($(varE p) > conPrec) |]
                          [| $(showbPrecTy (conPrec + 1) sClass opName tvis alTy al)
                          <> $(infixOpE)
                          <> $(showbPrecTy (conPrec + 1) sClass opName tvis arTy ar)
                          |]
          )
          []
encodeArgs p sClass tvis (ForallC tvbs _ con) = encodeArgs p sClass (removeForalled tvbs tvis) con

-- | Checks if an type variable has an unlifted type that can be shown. If so,
-- wrap it in its corresponding constructor and show it. Otherwise, only show
-- the type variable.
showbPrecTy :: Int
            -> ShowClass
            -> String
            -> [TyVarInfo]
            -> Type
            -> Name
            -> Q Exp
showbPrecTy p sClass conName tvis ty tyExpName = do
    ty' <- expandSyn ty
    showbPrecTy' p sClass conName tvis ty' tyExpName

showbPrecTy' :: Int
             -> ShowClass
             -> String
             -> [TyVarInfo]
             -> Type
             -> Name
             -> Q Exp
showbPrecTy' p _ _ _ (ConT tyName) tyExpName =
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
showbPrecTy' p sClass conName tvis ty tyExpName =
    [| $(makeShowExp sClass conName tvis ty) p $(varE tyExpName) |]

makeShowExp :: ShowClass
            -> String
            -> [TyVarInfo]
            -> Type
            -> Q Exp
makeShowExp _ _ tvis (VarT tyName) =
    case lookup (NameBase tyName) tvis of
         Just spExp -> varE spExp
         Nothing    -> [| showbPrec |]
makeShowExp sClass conName tvis (SigT ty _)         = makeShowExp sClass conName tvis ty
makeShowExp sClass conName tvis (ForallT tvbs _ ty) = makeShowExp sClass conName (removeForalled tvbs tvis) ty
makeShowExp sClass conName tvis ty =
    let tyArgs :: [Type]
        _ :| tyArgs = unapplyTy ty

        numLastArgs :: Int
        numLastArgs = min (fromEnum sClass) (length tyArgs)

        lhsArgs, rhsArgs :: [Type]
        (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

        tyVarNameBases :: [NameBase]
        tyVarNameBases = map fst tvis

    in if any (`mentionsNameBase` tyVarNameBases) lhsArgs
          then outOfPlaceTyVarError conName tyVarNameBases numLastArgs
          else appsE $ [ varE . showFuncTable $ toEnum numLastArgs]
                    ++ map (makeShowExp sClass conName tvis) rhsArgs

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

data ShowClass = Show | Show1 | Show2
  deriving (Enum, Eq, Ord)

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

-- | A type-restricted version of 'const'. This is useful when generating the lambda
-- expression in 'mkShowbPrec' for a data type with only nullary constructors (since
-- the expression wouldn't depend on the precedence). For example, if you had @data
-- Nullary = Nullary@ and attempted to run @$(mkShowbPrec ''Nullary) Nullary@, simply
-- ignoring the precedence argument would cause the type signature of @$(mkShowbPrec
-- ''Nullary)@ to be @a -> Nullary -> Builder@, not @Int -> Nullary -> Builder@.
--
-- To avoid this problem, after computing the 'Builder' @b@, we call @intConst b p@,
-- where @p@ is the precedence argument. This forces @p :: Int@.
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

constTable :: ShowClass -> Name
constTable Show  = 'showbPrecConst
constTable Show1 = 'showbPrecWithConst
constTable Show2 = 'showbPrecWith2Const

-- | Extracts a plain type constructor's information.
withTyCon :: Name -- ^ Name of the plain type constructor
            -> (Cxt -> [TyVarBndr] -> [Con] -> Q a)
            -- ^ Function that generates the actual code. Will be applied
            -- to the type variable binders and constructors extracted
            -- from the given 'Name'.
            -> Q a
            -- ^ Resulting value in the 'Q'uasi monad.
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
withDataFam :: Name
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
withDataFamInstCon :: Name
                   -> ([TyVarBndr] -> [TyVarBndr] -> Cxt
                           -> Name -> [Type] -> [Con] -> Q a)
                   -> Q a
withDataFamInstCon dficName f = do
    dficInfo <- reify dficName
    case dficInfo of
        DataConI _ ty parentName _ -> do
            parentInfo <- reify parentName
            case parentInfo of
                FamilyI (FamilyD DataFam _ _ _) _ -> withDataFam parentName $ \famTvbs decs ->
                    let sameDefDec = flip find decs $ \dec ->
                          case dec of
                              DataInstD    _ _ _ cons' _ -> any ((dficName ==) . constructorName) cons'
                              NewtypeInstD _ _ _ con   _ -> dficName == constructorName con
                              _ -> error $ ns ++ "Must be a data or newtype instance."

                        (ctxt, etaReducedTys, cons) = case sameDefDec of
                              Just (DataInstD    ctxt' _ erTys cons' _) -> (ctxt', erTys, cons')
                              Just (NewtypeInstD ctxt' _ erTys con   _) -> (ctxt', erTys, [con])
                              _ -> error $ ns ++ "Could not find data or newtype instance constructor."

                        instTvbs = case ty of
                              ForallT tvbs _ _ -> tvbs
                              _                -> []
                    in f famTvbs instTvbs ctxt parentName etaReducedTys cons
                _ -> error $ ns ++ "Data constructor " ++ S.show dficName ++ " is not from a data family instance."
        other -> error $ ns ++ "Unsupported type " ++ S.show other ++ ". Must be a data family instance constructor."
  where
    ns :: String
    ns = "Text.Show.Text.TH.withDataFamInstCon: "

-- | Extracts the name of a constructor.
constructorName :: Con -> Name
constructorName (NormalC name      _  ) = name
constructorName (RecC    name      _  ) = name
constructorName (InfixC  _    name _  ) = name
constructorName (ForallC _    _    con) = constructorName con
#endif

showClassTable :: ShowClass -> Name
showClassTable Show  = ''T.Show
showClassTable Show1 = ''Show1
showClassTable Show2 = ''Show2

showFuncTable :: ShowClass -> Name
showFuncTable Show  = 'showbPrec
showFuncTable Show1 = 'showbPrecWith
showFuncTable Show2 = 'showbPrecWith2

-- | Extracts the name from a type variable binder.
tvbName :: TyVarBndr -> Name
tvbName (PlainTV  name)   = name
tvbName (KindedTV name _) = name

tvbKind :: TyVarBndr -> Kind
tvbKind (PlainTV  _)   = starK
tvbKind (KindedTV _ k) = k

-- | Generates a declaration defining the 'showbPrec' function, followed by any custom
-- pragma declarations specified by the 'PragmaOptions' argument.
--
-- The Template Haskell API for generating pragmas (as well as GHC's treatment of
-- pragmas themselves) has changed considerably over the years, so there's a lot of
-- CPP magic required to get this to work uniformly across different versions of GHC.
showbPrecDecs :: [NameBase] -> [Con] -> [Q Dec]
showbPrecDecs nbs cons =
    [ funD classFuncName
           [ clause []
                    (normalB $ consToShow nbs cons)
                    []
           ]
    ]
  where
    classFuncName :: Name
    classFuncName  = showFuncTable . toEnum $ length nbs

-- | Applies a typeclass constraint to a type.
applyClass :: Name -> Name -> Pred
#if MIN_VERSION_template_haskell(2,10,0)
applyClass con t = AppT (ConT con) (VarT t)
#else
applyClass con t = ClassP con [VarT t]
#endif

canEtaReduce :: [Type] -> [Type] -> Bool
canEtaReduce remaining dropped =
       all isTyVar dropped
    && allDistinct nbs -- Make sure not to pass something of type [Type], since Type
                       -- didn't have an Ord instance until template-haskell-2.10.0.0
    && not (any (`mentionsNameBase` nbs) remaining)
  where
    nbs :: [NameBase]
    nbs = map varTToNameBase dropped

varTToName :: Type -> Name
varTToName (VarT n)   = n
varTToName (SigT t _) = varTToName t
varTToName _          = error "Not a type variable!"

varTToNameBase :: Type -> NameBase
varTToNameBase = NameBase . varTToName

isTyVar :: Type -> Bool
isTyVar (VarT _)   = True
isTyVar (SigT t _) = isTyVar t
isTyVar _          = False

allDistinct :: Ord a => [a] -> Bool
allDistinct = allDistinct' Set.empty
  where
    allDistinct' :: Ord a => Set a -> [a] -> Bool
    allDistinct' uniqs (x:xs)
        | x `Set.member` uniqs = False
        | otherwise            = allDistinct' (Set.insert x uniqs) xs
    allDistinct' _ _           = True

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

predMentionsNameBase :: Pred -> [NameBase] -> Bool
#if MIN_VERSION_template_haskell(2,10,0)
predMentionsNameBase = mentionsNameBase
#else
predMentionsNameBase (ClassP _ tys) nbs = any (`mentionsNameBase` nbs) tys
predMentionsNameBase (EqualP t1 t2) nbs = mentionsNameBase t1 nbs || mentionsNameBase t2 nbs
#endif

newtype NameBase = NameBase { getName :: Name }

getNameBase :: NameBase -> String
getNameBase = nameBase . getName

instance Eq NameBase where
    (==) = (==) `on` getNameBase

instance Ord NameBase where
    compare = compare `on` getNameBase

instance S.Show NameBase where
    showsPrec p = showsPrec p . nameBase . getName

-- data TyVarInfo = TyVarInfo {
--     tyVarNameBase   :: NameBase
--   , _tyVarShowsPrec :: Name
-- }
type TyVarInfo = (NameBase, Name)

-- numTyArrows :: Type -> Int
-- numTyArrows t = length (uncurryTy t) - 1
-- numTyArrows = go 0
--   where
--     go !n (AppT (AppT ArrowT _) ty) = go (n + 1) ty
--     go !n _                         = n

numKindArrows :: Kind -> Int
numKindArrows k = length (uncurryKind k) - 1
-- #if MIN_VERSION_template_haskell(2,8,0)
-- numKindArrows = numTyArrows
-- #else
-- numKindArrows = go 0
--   where
--     go !n (ArrowK _ k) = go (n + 1) k
--     go !n _            = n
-- #endif

applyTy :: Type -> [Type] -> Type
applyTy = foldl' AppT

-- Fully applies a type constructor to its type variables.
applyTyCon :: Name -> [Type] -> Type
applyTyCon = applyTy . ConT

unapplyTy :: Type -> NonEmpty Type
unapplyTy = NE.reverse . go
  where
    go :: Type -> NonEmpty Type
    go (AppT t1 t2) = t2 <| go t1
    go (SigT t _)   = go t
    go t            = t :| []

uncurryTy :: Type -> NonEmpty Type
uncurryTy = NE.reverse . go
  where
    go :: Type -> NonEmpty Type
    go (AppT (AppT ArrowT t1) t2) = t2 <| go t1
    go (SigT t _)                 = go t
    go t                          = t :| []

uncurryKind :: Kind -> NonEmpty Kind
#if MIN_VERSION_template_haskell(2,8,0)
uncurryKind = uncurryTy
#else
uncurryKind (ArrowK k1 k2) = k1 <| uncurryKind k2
uncurryKind k              = k :| []
#endif

applyConstraint :: TyVarBndr -> Pred
applyConstraint (PlainTV  name)      = applyClass ''T.Show  name
applyConstraint (KindedTV name kind) = applyClass showClass name
  where
    showClass :: Name
    showClass = showClassTable . toEnum $ numKindArrows kind

needsConstraint :: ShowClass -> TyVarBndr -> Bool
needsConstraint showClass (PlainTV _) = showClass >= Show
needsConstraint showClass (KindedTV _ kind) =
       showClass >= toEnum (numKindArrows kind)
    && canRealizeKindStarChain kind

wellKinded :: [Kind] -> Bool
wellKinded = all canRealizeKindStar

canRealizeKindStar :: Kind -> Bool
-- canRealizeKindStar k = numKindArrows k == 0
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

replaceTyVarName :: TyVarBndr -> Type -> TyVarBndr
replaceTyVarName tvb            (SigT t _) = replaceTyVarName tvb t
replaceTyVarName (PlainTV  _)   (VarT n)   = PlainTV  n
replaceTyVarName (KindedTV _ k) (VarT n)   = KindedTV n k
replaceTyVarName tvb            _          = tvb

-- | Of form k1 -> k2 -> ... -> kn, where k is either a single kind variable or *.
canRealizeKindStarChain :: Kind -> Bool
canRealizeKindStarChain = all canRealizeKindStar . uncurryKind

-- | Deduces the 'Show' instance context of a simple type constructor, as well
-- as the type constructor fully applied to its type variables.
cxtAndTypeTyCon :: ShowClass
                -> Name
                -> Cxt
                -> [TyVarBndr]
                -> (Cxt, Type, [NameBase])
cxtAndTypeTyCon numToDrop tyConName dataCxt tvbs =
    if remainingLength < 0 || not (wellKinded droppedKinds) -- If we have enough well-kinded type variables
       then derivingKindError numToDrop tyConName
    else if any (`predMentionsNameBase` droppedNbs) dataCxt -- If the last type variable(s) are mentioned in a datatype context
       then datatypeContextError numToDrop instanceType
    else (instanceCxt, instanceType, droppedNbs)
  where
    instanceCxt :: Cxt
    instanceCxt = map applyConstraint
                $ filter (needsConstraint numToDrop) remaining

    instanceType :: Type
    instanceType = applyTyCon tyConName $ map (VarT . tvbName) remaining

    remainingLength :: Int
    remainingLength = length tvbs - fromEnum numToDrop

    remaining, dropped :: [TyVarBndr]
    (remaining, dropped) = splitAt remainingLength tvbs

    droppedKinds :: [Kind]
    droppedKinds = map tvbKind dropped

    droppedNbs :: [NameBase]
    droppedNbs = map (NameBase . tvbName) dropped

#if MIN_VERSION_template_haskell(2,7,0)
-- | Deduces the 'Show' instance context of a data family instance constructor,
-- as well as the type constructor fully applied to its type variables.
cxtAndTypeDataFamInstCon :: ShowClass
                         -> Name
                         -> Cxt
                         -> [TyVarBndr]
                         -> [TyVarBndr]
                         -> [Type]
                         -> (Cxt, Type, [NameBase])
cxtAndTypeDataFamInstCon numToDrop parentName dataCxt famTvbs _instTvbs etaReducedTys =
    if remainingLength < 0 || not (wellKinded droppedKinds) -- If we have enough well-kinded type variables
       then derivingKindError numToDrop parentName
    else if any (`predMentionsNameBase` droppedNbs) dataCxt -- If the last type variable(s) are mentioned in a datatype context
       then datatypeContextError numToDrop instanceType
    else if canEtaReduce remaining dropped -- If it is safe to drop the type variables
       then (instanceCxt, instanceType, droppedNbs)
    else etaReductionError instanceType
  where
    instanceCxt :: Cxt
    instanceCxt = map applyConstraint
                $ filter (needsConstraint numToDrop) lhsTvbs

    instanceType :: Type
    instanceType = applyTyCon parentName remaining

    remainingLength :: Int
    remainingLength = length famTvbs - fromEnum numToDrop

    remaining, dropped :: [Type]
    (remaining, dropped) = splitAt remainingLength rhsTypes

    droppedKinds :: [Kind]
    droppedKinds = map tvbKind . snd $ splitAt remainingLength famTvbs

    droppedNbs :: [NameBase]
    droppedNbs = map varTToNameBase dropped

    instTypes :: [Type]
    instTypes =
# if MIN_VERSION_template_haskell(2,10,0)
        etaReducedTys
# else
        -- If PolyKinds is enabled, the first entries in this list will be
        -- kind signatures on early versions of GHC, so drop them
        if length etaReducedTys > length famTvbs
              then drop (length famTvbs) etaReducedTys
              else etaReducedTys
# endif

    lhsTvbs :: [TyVarBndr]
    lhsTvbs = map (uncurry replaceTyVarName)
        . filter (isTyVar . snd)
        . take remainingLength
        $ zip famTvbs rhsTypes

    -- In @template-haskell-2.9.0.0@, only the @Type@s up to the rightmost
    -- non-eta-reduced variable in @instTypes@ are provided, so we have to do some
    -- careful type manipulation to get the LHS of the instance context just right.
    -- For more info on this bug, see https://ghc.haskell.org/trac/ghc/ticket/9692
    --
    -- Other versions of @template-haskell@ seem a bit more sensible.
    rhsTypes :: [Type]
# if !(MIN_VERSION_template_haskell(2,9,0)) || MIN_VERSION_template_haskell(2,10,0)
    rhsTypes = instTypes
# else
    rhsTypes = instTypes ++ map (VarT . tvbName)
                                (drop (length _instTvbs
                                        - length famTvbs
                                        + length instTypes
                                      )
                                      _instTvbs)
# endif
#endif

derivingKindError :: ShowClass -> Name -> a
derivingKindError numToDrop tyConName = error
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
    . showString (pprint . createKind $ fromEnum numToDrop)
    $ ""
  where
    className :: String
    className = nameBase $ showClassTable numToDrop

    createKind :: Int -> Kind
    createKind = go starK
      where
        go :: Kind -> Int -> Kind
        go k !0 = k
#if MIN_VERSION_template_haskell(2,8,0)
        go k !n = go (AppT (AppT ArrowT StarT) k) (n - 1)
#else
        go k !n = go (ArrowK StarK k) (n - 1)
#endif

etaReductionError :: Type -> a
etaReductionError instanceType = error $
    "Cannot eta-reduce to an instance of form \n\tinstance (...) => "
    ++ pprint instanceType

datatypeContextError :: ShowClass -> Type -> a
datatypeContextError numToDrop instanceType = error
    . showString "Can't make a derived instance of ‘"
    . showString (pprint instanceType)
    . showString "‘:\n\tData type ‘"
    . showString className
    . showString "‘ must not have a class context involving the last type argument(s)"
    $ ""
  where
    className :: String
    className = nameBase $ showClassTable numToDrop

outOfPlaceTyVarError :: String -> [NameBase] -> Int -> a
outOfPlaceTyVarError conName tyVarName numLastArgs = error
    . showString "Constructor ‘"
    . showString conName
    . showString "‘ must use the type variable"
    . plural id (showChar 's')
    . showString " "
    . showsPrec 0 tyVarName
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

-------------------------------------------------------------------------------
-- Expanding type synonyms
-------------------------------------------------------------------------------

-- Expands all type synonyms in a type. Written by Dan Rosén in the
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
