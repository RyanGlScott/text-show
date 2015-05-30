{-# LANGUAGE CPP             #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module:      Text.Show.Text.TH.Internal
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Functions to mechanically derive 'T.Show' instances or splice
@show@-related expressions into Haskell source code. You need to enable
the @TemplateHaskell@ language extension in order to use this module.

This implementation is based off of the @Data.Aeson.TH@ module from the
@aeson@ library.
-}
module Text.Show.Text.TH.Internal (
      -- * @deriveShow@
      -- $deriveShow
      deriveShow
    , deriveShow1
    , deriveShow2
    , deriveShowPragmas
    , deriveShow1Pragmas
    , deriveShow2Pragmas
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
--     , mkShowbPrec2
      -- * Advanced pragma options
    , PragmaOptions(..)
    , defaultPragmaOptions
    , defaultInlineShowbPrec
    , defaultInlineShowb
    , defaultInlineShowbList
    ) where

import           Data.Function (on)
import           Data.List (find, foldl', intersperse)
#if MIN_VERSION_template_haskell(2,7,0)
import           Data.Maybe (fromJust)
#endif
import           Data.Monoid.Compat ((<>))
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Text    as TS ()
import qualified Data.Text.IO as TS (putStrLn, hPutStrLn)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (fromString, toLazyText)
import qualified Data.Text.Lazy    as TL ()
import qualified Data.Text.Lazy.IO as TL (putStrLn, hPutStrLn)

import           GHC.Exts (Char(..), Double(..), Float(..), Int(..), Word(..))
import           GHC.Prim (Char#, Double#, Float#, Int#, Word#)
import           GHC.Show (appPrec, appPrec1)

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (lift)

import           Prelude ()
import           Prelude.Compat hiding (Show)

import qualified Text.Show as S (Show(show))
import qualified Text.Show.Text.Classes as T
import           Text.Show.Text.Classes (Show1(..), Show2(..),
                                         showb, showbPrec, showbList,
                                         showbListWith, showbParen, showbSpace)
import           Text.Show.Text.Utils (isInfixTypeCon, isTupleString, s)

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

Note that at the moment, there are some limitations to this approach:

* The 'Name' argument to 'deriveShow' must not be a type synonym.

* 'deriveShow' makes the assumption that all type variables in a data type require a
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
deriveShow = deriveShowPragmas defaultPragmaOptions

deriveShow1 :: Name
            -> Q [Dec]
deriveShow1 = deriveShow1Pragmas defaultPragmaOptions

deriveShow2 :: Name
            -> Q [Dec]
deriveShow2 = deriveShow2Pragmas defaultPragmaOptions

-- | Generates a 'T.Show' instance declaration for the given data type or data family
-- instance. You shouldn't need to use this function unless you know what you are doing.
-- 
-- Unlike 'deriveShow', this function allows configuration of whether to inline
-- certain functions. It also allows for specializing instances for certain types.
-- For example:
-- 
-- @
-- &#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
-- import Text.Show.Text.TH
-- 
-- data ADT a = ADT a
-- $(deriveShowPragmas 'defaultInlineShowbPrec' {
--                         specializeTypes = [ [t| ADT Int |] ]
--                      }
--                      ''ADT)
-- @
-- 
-- This declararation would produce code like this:
-- 
-- @
-- instance Show a => Show (ADT a) where
--     &#123;-&#35; INLINE showbPrec &#35;-&#125;
--     &#123;-&#35; SPECIALIZE instance Show (ADT Int) &#35;-&#125;
--     showbPrec = ...
-- @
-- 
-- Beware: 'deriveShow' can generate extremely long code splices, so it may be unwise
-- to inline in some cases. Use with caution.
-- 
-- /Since: 0.5/
deriveShowPragmas :: PragmaOptions -- ^ Specifies what pragmas to generate with this instance
                  -> Name          -- ^ Name of the data type to make an instance of 'T.Show'
                  -> Q [Dec]
deriveShowPragmas = deriveShowNumber 0 ''T.Show 'showbPrec

deriveShow1Pragmas :: PragmaOptions
                   -> Name
                   -> Q [Dec]
deriveShow1Pragmas = deriveShowNumber 1 ''Show1 'showbPrecWith

deriveShow2Pragmas :: PragmaOptions
                   -> Name
                   -> Q [Dec]
deriveShow2Pragmas = deriveShowNumber 2 ''Show2 'showbPrecWith2

deriveShowNumber :: Int
                 -> Name
                 -> Name
                 -> PragmaOptions
                 -> Name
                 -> Q [Dec]
deriveShowNumber numToDrop className funcName opts tyConName = do
    info <- reify tyConName
    case info of
        TyConI{} -> deriveShowTyCon numToDrop className funcName opts tyConName
#if MIN_VERSION_template_haskell(2,7,0)
        DataConI{} -> deriveShowDataFamInst numToDrop className funcName opts tyConName
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
deriveShowTyCon :: Int
                -> Name
                -> Name
                -> PragmaOptions
                -> Name
                -> Q [Dec]
deriveShowTyCon numToDrop className funcName opts tyConName =
    withTyCon tyConName fromCons
  where
    fromCons :: [Name] -> [Con] -> Q [Dec]
    fromCons tyVarNames cons = (:[]) <$>
        instanceD (return instanceCxt)
                  (return $ AppT (ConT className) instanceType)
                  (showbPrecDecs opts className funcName droppedNbs cons)
      where
        (instanceCxt, instanceType, droppedNbs) =
            cxtAndTypeTyCon numToDrop tyConName tyVarNames

#if MIN_VERSION_template_haskell(2,7,0)
-- | Generates a 'T.Show' instance declaration for a data family instance constructor.
deriveShowDataFamInst :: Int
                      -> Name
                      -> Name
                      -> PragmaOptions
                      -> Name
                      -> Q [Dec]
deriveShowDataFamInst numToDrop className funcName opts dataFamInstName =
    withDataFamInstCon dataFamInstName fromDec
  where
    fromDec :: Name -> [Name] -> Dec -> Q [Dec]
    fromDec parentName tyVarNames dec = (:[]) <$>
        instanceD (return instanceCxt)
                  (return $ AppT (ConT className) instanceType)
                  (showbPrecDecs opts className funcName droppedNbs $ decCons [dec])
      where
        (instanceCxt, instanceType, droppedNbs) =
            cxtAndTypeDataFamInstCon numToDrop parentName tyVarNames dec
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
mkShowbPrec = mkShowbPrecNumber 0

mkShowbPrecWith :: Name -> Q Exp
mkShowbPrecWith = mkShowbPrecNumber 1

mkShowbPrec1 :: Name -> Q Exp
mkShowbPrec1 name = [| $(mkShowbPrecWith name) $(mkShowbPrec name) |]

mkShowbPrecWith2 :: Name -> Q Exp
mkShowbPrecWith2 = mkShowbPrecNumber 2

-- mkShowbPrec2 :: Name -> Q Exp
-- mkShowbPrec2 name = [| $(mkShowbPrecWith2 name) $(mkShowbPrec name) |]

mkShowbPrecNumber :: Int -> Name -> Q Exp
mkShowbPrecNumber numToDrop tyConName = do
    info <- reify tyConName
    case info of
        TyConI{} -> withTyCon tyConName $ \tyVarNames decs ->
            let (_, _, nbs) = cxtAndTypeTyCon numToDrop tyConName tyVarNames
             in consToShow nbs decs
#if MIN_VERSION_template_haskell(2,7,0)
        DataConI{} -> withDataFamInstCon tyConName $ \parentName tyVarNames dec ->
            let (_, _, nbs) = cxtAndTypeDataFamInstCon numToDrop parentName tyVarNames dec
             in consToShow nbs $ decCons [dec]
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

-- | Options that specify what @INLINE@ or @SPECIALIZE@ pragmas to generate with
-- a 'T.Show' instance.
-- 
-- /Since: 0.5/
data PragmaOptions = PragmaOptions {
    inlineFunctions :: [Name]   -- ^ Inline these functions
  , specializeTypes :: [Q Type] -- ^ Create specialized instance declarations for these types
}

-- | Do not generate any pragmas with a 'T.Show' instance.
-- 
-- /Since: 0.5/
defaultPragmaOptions :: PragmaOptions
defaultPragmaOptions = PragmaOptions [] []

-- | Inline the 'showbPrec' function in a 'T.Show' instance.
-- 
-- /Since: 0.5/
defaultInlineShowbPrec :: PragmaOptions
defaultInlineShowbPrec = defaultPragmaOptions { inlineFunctions = ['showbPrec] }

-- | Inline the 'showb' function in a 'T.Show' instance.
-- 
-- /Since: 0.5/
defaultInlineShowb :: PragmaOptions
defaultInlineShowb = defaultPragmaOptions { inlineFunctions = ['showb] }

-- | Inline the 'showbList' function in a 'T.Show' instance.
-- 
-- /Since: 0.5/
defaultInlineShowbList :: PragmaOptions
defaultInlineShowbList = defaultPragmaOptions { inlineFunctions = ['showbList] }

-- | Generates code to generate the 'T.Show' encoding of a number of constructors.
-- All constructors must be from the same type.
consToShow :: [NameBase] -> [Con] -> Q Exp
consToShow _   []   = error "Text.Show.Text.TH.consToShow: Not a single constructor given!"
consToShow nbs cons = do
    p     <- newName "p"
    value <- newName "value"
    sps   <- mapM newName ["sp" ++ S.show n | (_, n) <- zip nbs [(1 :: Int) ..]]
    let tvbs = zipWith TyVarInfo nbs sps
    lamE (map varP $ sps ++ [p, value])
        . caseE (varE value)
        $ map (encodeArgs p tvbs) cons

-- | Generates code to generate the 'T.Show' encoding of a single constructor.
encodeArgs :: Name -> [TyVarInfo] -> Con -> Q Match
encodeArgs p _ (NormalC conName [])
    = match (conP conName [])
            (normalB [| intConst (fromString $(stringE (parenInfixConName conName ""))) $(varE p) |])
            []
encodeArgs p tvis (NormalC conName [(_, argTy)]) = do
    arg <- newName "arg"
    
    let showArg  = showbPrecPrim appPrec1 tvis argTy arg
        namedArg = [| fromString $(stringE (parenInfixConName conName " ")) <> $(showArg) |] 
    
    match (conP conName [varP arg])
          (normalB [| showbParen ($(varE p) > $(lift appPrec)) $(namedArg) |])
          []
encodeArgs p tvis (NormalC conName ts) = do
    args <- mapM newName ["arg" ++ S.show n | (_, n) <- zip ts [(1 :: Int) ..]]
    
    if isNonUnitTuple conName
       then do
           let showArgs       = map (\(arg, (_, argTy)) -> showbPrecPrim 0 tvis argTy arg)
                                    (zip args ts)
               parenCommaArgs = [| s '(' |] : intersperse [| s ',' |] showArgs
               mappendArgs    = foldr (`infixApp` [| (<>) |])
                                      [| s ')' |]
                                      parenCommaArgs
           
           match (conP conName $ map varP args)
                 (normalB [| intConst $(mappendArgs) $(varE p) |])
                 []
       else do
           let showArgs = map (\(arg, (_, argTy)) -> showbPrecPrim appPrec1 tvis argTy arg)
                              (zip args ts)
               mappendArgs = foldr1 (\v q -> [| $(v) <> showbSpace <> $(q) |]) showArgs
               namedArgs   = [| fromString $(stringE (parenInfixConName conName " ")) <> $(mappendArgs) |]
           
           match (conP conName $ map varP args)
                 (normalB [| showbParen ($(varE p) > $(lift appPrec)) $(namedArgs) |])
                 []
encodeArgs p tvis (RecC conName []) = encodeArgs p tvis $ NormalC conName []
encodeArgs p tvis (RecC conName ts) = do
    args <- mapM newName ["arg" ++ S.show n | (_, n) <- zip ts [1 :: Int ..]]
    
    let showArgs       = concatMap (\(arg, (argName, _, argTy))
                                      -> [ [| fromString $(stringE (nameBase argName ++ " = ")) |]
                                         , showbPrecPrim 0 tvis argTy arg
                                         , [| fromString ", "                                   |]
                                         ]
                                   )
                                   (zip args ts)
        braceCommaArgs = [| s '{' |] : take (length showArgs - 1) showArgs
        mappendArgs    = foldr (`infixApp` [| (<>) |])
                           [| s '}' |]
                           braceCommaArgs
        namedArgs      = [| fromString $(stringE (parenInfixConName conName " ")) <> $(mappendArgs) |]
    
    match (conP conName $ map varP args)
          (normalB
#if __GLASGOW_HASKELL__ >= 711
                    [| intConst $(namedArgs) $(varE p) |]
#else
                    [| showbParen ($(varE p) > $(lift appPrec)) $(namedArgs) |]
#endif
          )
          []
encodeArgs p tvis (InfixC (_, alTy) conName (_, arTy)) = do
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
                          [| $(showbPrecPrim (conPrec + 1) tvis alTy al)
                          <> $(infixOpE)
                          <> $(showbPrecPrim (conPrec + 1) tvis arTy ar)
                          |]
          )
          []
encodeArgs p tvis (ForallC tvbs _ con) = encodeArgs p (removeForalled tvbs tvis) con

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

-- | Parenthesize an infix constructor name if it is being applied as a prefix
-- function (e.g., data Amp a = (:&) a a)
parenInfixConName :: Name -> ShowS
parenInfixConName conName =
    let conNameBase = nameBase conName
     in showParen (isInfixTypeCon conNameBase) $ showString conNameBase

-- | Checks if an type variable has an unlifted type that can be shown. If so,
-- wrap it in its corresponding constructor and show it. Otherwise, only show
-- the type variable.
showbPrecPrim :: Int -> [TyVarInfo] -> Type -> Name -> Q Exp
#if __GLASGOW_HASKELL__ >= 711
-- Starting with GHC 7.10, data types containing unlifted types with derived @Show@
-- instances show hashed literals with actual hash signs, and negative hashed
-- literals are not surrounded with parentheses.
showbPrecPrim p _ (ConT tyName) tyExpName = showE
  where
    tyVarE :: Q Exp
    tyVarE = varE tyExpName
    
    showE :: Q Exp
    showE | tyName == ''Char#   = [| showbPrec 0 (C# $(tyVarE)) <> s '#'           |]
          | tyName == ''Double# = [| showbPrec 0 (D# $(tyVarE)) <> fromString "##" |]
          | tyName == ''Float#  = [| showbPrec 0 (F# $(tyVarE)) <> s '#'           |]
          | tyName == ''Int#    = [| showbPrec 0 (I# $(tyVarE)) <> s '#'           |]
          | tyName == ''Word#   = [| showbPrec 0 (W# $(tyVarE)) <> fromString "##" |]
          | otherwise = [| showbPrec p $(tyVarE) |]
#else
showbPrecPrim p _ (ConT tyName) tyExpName = [| showbPrec p $(expr) |]
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
showbPrecPrim p tvis (VarT tyName) tyExpName =
    case find ((== NameBase tyName) . tyVarNameBase) tvis of
         Just (TyVarInfo _ spExp) -> [| $(varE spExp) p $(tyVarE) |]
         Nothing                  -> [| showbPrec p $(tyVarE) |]
  where
    tyVarE :: Q Exp
    tyVarE = varE tyExpName
showbPrecPrim p tvis (SigT ty _)         tyExpName = showbPrecPrim p tvis ty tyExpName
showbPrecPrim p tvis (ForallT tvbs _ ty) tyExpName = showbPrecPrim p (removeForalled tvbs tvis) ty tyExpName
showbPrecPrim p _     _                  tyExpName = [| showbPrec p $(varE tyExpName) |]

removeForalled :: [TyVarBndr] -> [TyVarInfo] -> [TyVarInfo]
removeForalled tvbs = filter (not . foralled tvbs)
  where
    foralled :: [TyVarBndr] -> TyVarInfo -> Bool
    foralled tvbs' tvi = tyVarNameBase tvi `elem` map (NameBase . tvbName) tvbs'

-- | Checks if a 'Name' represents a tuple type constructor (other than '()')
isNonUnitTuple :: Name -> Bool
isNonUnitTuple = isTupleString . nameBase

-- | A type-restricted version of 'const'. This is useful when generating the lambda
-- expression in 'mkShowbPrec' for a data type with only nullary constructors (since
-- the expression wouldn't depend on the precedence). For example, if you had @data
-- Nullary = Nullary@ and attempted to run @$(mkShowbPrec ''Nullary) Nullary@, simply
-- ignoring the precedence argument would cause the type signature of @$(mkShowbPrec
-- ''Nullary)@ to be @a -> Nullary -> Builder@, not @Int -> Nullary -> Builder@.
-- 
-- To avoid this problem, after computing the 'Builder' @b@, we call @intConst b p@,
-- where @p@ is the precedence argument. This forces @p :: Int@.
intConst :: a -> Int -> a
intConst = const
{-# INLINE intConst #-}

-- | Extracts a plain type constructor's information.
withTyCon :: Name -- ^ Name of the plain type constructor
            -> ([Name] -> [Con] -> Q a)
            -- ^ Function that generates the actual code. Will be applied
            -- to the type variable names and constructors extracted
            -- from the given 'Name'.
            -> Q a
            -- ^ Resulting value in the 'Q'uasi monad.
withTyCon name f = do
    info <- reify name
    case info of
        TyConI dec ->
            case dec of
                DataD    _ _ tvbs cons _ -> f (map tvbName tvbs) cons
                NewtypeD _ _ tvbs con  _ -> f (map tvbName tvbs) [con]
                other -> error $ ns ++ "Unsupported type " ++ S.show other ++ ". Must be a data type or newtype."
        _ -> error $ ns ++ "The name must be of a plain type constructor."
  where
    ns :: String
    ns = "Text.Show.Text.TH.withTyCon: "

#if MIN_VERSION_template_haskell(2,7,0)
-- | Extracts a data family name's information.
withDataFam :: Name
            -> ([Name] -> [Dec] -> Q a)
            -> Q a
withDataFam name f = do
    info <- reify name
    case info of
        FamilyI (FamilyD DataFam _ tvbs _) decs -> f (map tvbName tvbs) decs
        FamilyI (FamilyD TypeFam _ _    _) _    ->
            error $ ns ++ "Cannot use a type family name."
        other -> error $ ns ++ "Unsupported type " ++ S.show other ++ ". Must be a data family name."
  where
    ns :: String
    ns = "Text.Show.Text.TH.withDataFam: "

-- | Extracts a data family instance constructor's information.
withDataFamInstCon :: Name
                   -> (Name -> [Name] -> Dec -> Q a)
                   -> Q a
withDataFamInstCon dficName f = do
    dficInfo <- reify dficName
    case dficInfo of
        DataConI _ _ parentName _ -> do
            parentInfo <- reify parentName
            case parentInfo of
                FamilyI (FamilyD DataFam _ _ _) _ -> withDataFam parentName $ \tyVarNames decs ->
                    let sameDefDec = fromJust . flip find decs $ \dec ->
                          case dec of
                              DataInstD    _ _ _ cons _ -> any ((dficName ==) . constructorName) cons 
                              NewtypeInstD _ _ _ con  _ -> dficName == constructorName con
                              _ -> error $ ns ++ "Must be a data or newtype instance."
                    in f parentName tyVarNames sameDefDec
                _ -> error $ ns ++ "Data constructor " ++ S.show dficName ++ " is not from a data family instance."
        other -> error $ ns ++ "Unsupported type " ++ S.show other ++ ". Must be a data family instance constructor."
  where
    ns :: String
    ns = "Text.Show.Text.TH.withDataFamInstCon: "
#endif

-- | Extracts the information about the constructors from several @data@ or @newtype@
-- declarations.
decCons :: [Dec] -> [Con]
decCons decs = flip concatMap decs $ \dec -> case dec of
    DataInstD    _ _ _ cons _ -> cons
    NewtypeInstD _ _ _ con  _ -> [con]
    _ -> error "Text.Show.Text.TH.decCons: Must be a data or newtype instance."

-- | Extracts the name of a constructor.
constructorName :: Con -> Name
constructorName (NormalC name      _  ) = name
constructorName (RecC    name      _  ) = name
constructorName (InfixC  _    name _  ) = name
constructorName (ForallC _    _    con) = constructorName con

-- | Extracts the name from a type variable binder.
tvbName :: TyVarBndr -> Name
tvbName (PlainTV  name  ) = name
tvbName (KindedTV name _) = name

-- | Generates a declaration defining the 'showbPrec' function, followed by any custom
-- pragma declarations specified by the 'PragmaOptions' argument.
-- 
-- The Template Haskell API for generating pragmas (as well as GHC's treatment of
-- pragmas themselves) has changed considerably over the years, so there's a lot of
-- CPP magic required to get this to work uniformly across different versions of GHC.
showbPrecDecs :: PragmaOptions -> Name -> Name -> [NameBase] -> [Con] -> [Q Dec]
showbPrecDecs _opts _className funcName nbs cons =
    [ funD funcName [ clause []
                             (normalB $ consToShow nbs cons)
                             []
                    ]
    ] ++ inlineDecs
      ++ specializeDecs
  where
    inlineDecs :: [Q Dec]
#if __GLASGOW_HASKELL__ <= 702
    inlineDecs = []
#else
    inlineDecs = map inline $ inlineFunctions _opts

    inline :: Name -> Q Dec
    inline funName =
        pragInlD funName
# if MIN_VERSION_template_haskell(2,8,0)
                 Inline FunLike AllPhases
# else
                 (inlineSpecNoPhase True False)
# endif
#endif
          
    specializeDecs :: [Q Dec]
#if MIN_VERSION_template_haskell(2,8,0)
    specializeDecs = (fmap . fmap) (PragmaD
                                        . SpecialiseInstP
                                        . AppT (ConT _className)
                                   )
                                   (specializeTypes _opts)
#else
    -- There doesn't appear to be an equivalent of SpecialiseInstP in early
    -- versions of Template Haskell.
    specializeDecs = []
#endif

-- Fully applies a type constructor to its type variables.
applyTyCon :: Name -> [Type] -> Type
applyTyCon = foldl' AppT . ConT

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
    go foralls (VarT n)     nbs = nb `elem` nbs && not (nb `Set.member` foralls)
      where
        nb = NameBase n
    go _       _            _   = False

newtype NameBase = NameBase { getName :: Name }

getNameBase :: NameBase -> String
getNameBase = nameBase . getName

instance Eq NameBase where
    (==) = (==) `on` getNameBase

instance Ord NameBase where
    compare = compare `on` getNameBase

data TyVarInfo = TyVarInfo {
    tyVarNameBase   :: NameBase
  , _tyVarShowsPrec :: Name
}

-- | Deduces the 'Show' instance context of a simple type constructor, as well
-- as the type constructor fully applied to its type variables.
cxtAndTypeTyCon :: Int -> Name -> [Name] -> (Cxt, Type, [NameBase])
cxtAndTypeTyCon numToDrop tyConName tyVarNames = 
    if remainingLength >= 0 -- If we have enough type variables
       then (instanceCxt, instanceType, droppedNbs)
       else error "TODOERRORMESSAGE"
  where
    instanceCxt :: Cxt
    instanceCxt = map (applyClass ''T.Show) remaining

    instanceType :: Type
    instanceType = applyTyCon tyConName $ map VarT remaining

    remainingLength :: Int
    remainingLength = length tyVarNames - numToDrop

    (remaining, dropped) = splitAt remainingLength tyVarNames

    droppedNbs :: [NameBase]
    droppedNbs = map NameBase dropped

-- | Deduces the 'Show' instance context of a data family instance constructor,
-- as well as the type constructor fully applied to its type variables.
cxtAndTypeDataFamInstCon :: Int -> Name -> [Name] -> Dec -> (Cxt, Type, [NameBase])
cxtAndTypeDataFamInstCon numToDrop parentName tyVarNames dec =
    if remainingLength >= 0                -- If we have enough type variables
         && canEtaReduce remaining dropped -- If it is safe to drop the type variables
       then (instanceCxt, instanceType, droppedNbs)
       else error "TODOERRORMESSAGE"
  where
    instanceCxt :: Cxt
    instanceCxt = map (applyClass ''T.Show) $ take remainingLength lhsTypeNames

    instanceType :: Type
    instanceType = applyTyCon parentName remaining

    remainingLength :: Int
    remainingLength = length rhsTypes - numToDrop

    (remaining, dropped) = splitAt remainingLength rhsTypes

    droppedNbs :: [NameBase]
    droppedNbs = map varTToNameBase dropped

    -- It seems that Template Haskell's representation of the type variable binders
    -- in a data family instance declaration has changed considerably with each new
    -- version. Yikes.
    --
    -- In @template-haskell-2.8.0.0@, only the TyVarBndrs up to the rightmost non-type
    -- variable are provided, so we have to do some careful Name manipulation to get
    -- the LHS of the instance context just right.
    --
    -- Other versions of @template-haskell@ seem a bit more sensible.
    lhsTypeNames :: [Name]
# if !(MIN_VERSION_template_haskell(2,9,0)) || MIN_VERSION_template_haskell(2,10,0)
    lhsTypeNames = filterTyVars tyVarNames instTypes
# else
    lhsTypeNames = filterTyVars (take (length instTypes) tyVarNames) instTypes
                ++ drop (length instTypes) tyVarNames
# endif

    filterTyVars :: [Name] -> [Type] -> [Name]
    filterTyVars ns     (SigT t _:ts) = filterTyVars ns (t:ts)
    filterTyVars (_:ns) (VarT n  :ts) = n : filterTyVars ns ts
    filterTyVars (_:ns) (_       :ts) = filterTyVars ns ts
    filterTyVars []     _             = []
    filterTyVars _      []            = []

    rhsTypes :: [Type]
    rhsTypes = instTypes ++ drop (length instTypes) (map VarT tyVarNames)

    instTypes :: [Type]
    instTypes = let tys = case dec of
                              DataInstD    _ _ tys' _ _ -> tys'
                              NewtypeInstD _ _ tys' _ _ -> tys'
                              _ -> error "Text.Show.Text.TH.deriveShow: The impossible happened."
# if MIN_VERSION_template_haskell(2,10,0)
                in tys
# else
                -- If PolyKinds is enabled, the first entries in this list will be
                -- kind signatures on early versions of GHC, so drop them
                in if length tys > length tyVarNames
                      then drop (length tyVarNames) tys
                      else tys
# endif
