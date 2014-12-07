{-# LANGUAGE CPP, NoImplicitPrelude, TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.TH.Internal
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Functions to mechanically derive 'Show' instances or splice
-- 'show'-related expressions into Haskell source code. You need to enable
-- the @TemplateHaskell@ language extension in order to use this module.
----------------------------------------------------------------------------
module Text.Show.Text.TH.Internal (
      -- * @deriveShow@
      -- $deriveShow
      deriveShow
      -- * @mk@ functions
      -- $mk
    , mkShow
    , mkShowLazy
    , mkShowPrec
    , mkShowPrecLazy
--     , mkShowList
--     , mkShowListLazy
    , mkShowb
    , mkShowbPrec
--     , mkShowbList
    , mkPrint
    , mkPrintLazy
    , mkHPrint
    , mkHPrintLazy
    ) where

import           Control.Applicative ((<$>))

import           Data.List (foldl')
import qualified Data.Text    as TS ()
import qualified Data.Text.IO as TS (putStrLn, hPutStrLn)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (Builder, fromString, toLazyText)
import qualified Data.Text.Lazy    as TL ()
import qualified Data.Text.Lazy.IO as TL (putStrLn, hPutStrLn)

import           GHC.Show (appPrec, appPrec1)

import           Language.Haskell.TH

import qualified Prelude as P
import           Prelude hiding (Show)

import           Text.Show.Text.Class (Show(showb, showbPrec), showbParen)
import           Text.Show.Text.Utils ((<>), s)

{- $deriveShow

'deriveShow' automatically generates a 'Show' instance declaration for a @data@
type or @newtype@. As an example:

@
&#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
import Text.Show.Text.TH (deriveShow)

data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Double
                  , testTwo   :: Bool
                  , testThree :: D a
                  }
$(deriveShow ''D)
@

@D@ now has a 'Show' instance equivalent to that which would be generated
by a @deriving Show@ clause. 

Note that at the moment, 'deriveShow' does not support data families,
so it is impossible to use 'deriveShow' with @data instance@s or @newtype
instance@s. Also, 'deriveShow' lacks the ability to properly detect data types
with higher-kinded type parameters (e.g., @data HK f a = HK (f a)@), so it cannot
create instances for them either.

-}

-- | Generates a 'Show' instance declaration for the given @data@ type or @newtype@.
deriveShow :: Name -> Q [Dec]
deriveShow name = withType name $ \tvbs cons -> (:[]) <$> fromCons tvbs cons
  where
    fromCons :: [TyVarBndr] -> [Con] -> Q Dec
    fromCons tvbs cons = instanceD cxt'
                                   (appT classType type')
                                   [ funD 'showbPrec [ clause [] (normalB $ consToShow cons) []
                                                     ]
                                   ]
      where
          classType :: Q Type
          classType = conT ''Show
          
          cxt'  :: Q Cxt
          type' :: Q Type
          (cxt', type') = instanceCtxType name tvbs

{- $mk

There may be scenarios in which you want to show an arbitrary @data@ type or @newtype@
without having to make the type an instance of 'Show'. For these cases,
"Text.Show.Text.TH" provide several functions (all prefixed with @mk@) that splice
the appropriate lambda expression into your source code.

As an example, suppose you have @data ADT = ADTCon@, which is not an instance of 'Show'.
With @mkShow@, you can still convert it to 'Text':

@
&#123;-&#35; LANGUAGE OverloadedStrings, TemplateHaskell &#35;-&#125;

whichADT :: Bool
whichADT = $(mkShow ''ADT) ADTCon == \"ADT\"
@

Note that due the order in which Template Haskell executes splices, the above code
may fail to compile if @ADT@ is located in the same module and @whichADT@. To get
around this, you can use the following hack:

@
&#123;-&#35; LANGUAGE OverloadedStrings, TemplateHaskell &#35;-&#125;

data ADT = ADTCon
$(return [])

whichADT :: Bool
whichADT = $(mkShow ''ADT) ADTCon == \"ADT\"
@

-}

-- |
-- Generates a lambda expression which converts the given @data@ type or @newtype@
-- to a strict 'TS.Text'.
mkShow :: Name -> Q Exp
mkShow name = [| toStrict . $(mkShowLazy name) |]

-- |
-- Generates a lambda expression which converts the given @data@ type or @newtype@
-- to a lazy 'TL.Text'.
mkShowLazy :: Name -> Q Exp
mkShowLazy name = [| toLazyText . $(mkShowb name) |]

-- |
-- Generates a lambda expression which converts the given @data@ type or @newtype@
-- to a strict 'TS.Text' with the given precedence.
mkShowPrec :: Name -> Q Exp
mkShowPrec name = [| \p -> toStrict . $(mkShowPrecLazy name) p |]

-- |
-- Generates a lambda expression which converts the given @data@ type or @newtype@
-- to a lazy 'TL.Text' with the given precedence.
mkShowPrecLazy :: Name -> Q Exp
mkShowPrecLazy name = [| \p -> toLazyText . $(mkShowbPrec name) p |]

-- |
-- Generates a lambda expression which converts the given @data@ type or @newtype@
-- to a 'Builder'.
mkShowb :: Name -> Q Exp
mkShowb name = mkShowbPrec name `appE` [| 0 :: Int |]

-- |
-- Generates a lambda expression which converts the given @data@ type or @newtype@
-- to a 'Builder' with the given precedence.
mkShowbPrec :: Name -> Q Exp
mkShowbPrec name = withType name $ \tvbs cons -> fromCons tvbs cons
  where
    fromCons :: [TyVarBndr] -> [Con] -> Q Exp
    fromCons tvbs cons = sigE (consToShow cons)
                            $ forallT tvbs
                                      cxt'
                                      [t| Int -> $(type') -> Builder |]
      where
        cxt'  :: Q Cxt
        type' :: Q Type
        (cxt', type') = instanceCtxType name tvbs

-- |
-- Generates a lambda expression which writes the given @data@ type or @newtype@
-- argument's strict 'TS.Text' output to the standard output, followed by a newline.
mkPrint :: Name -> Q Exp
mkPrint name = [| TS.putStrLn . $(mkShow name) |]

-- |
-- Generates a lambda expression which writes the given @data@ type or @newtype@
-- argument's lazy 'TL.Text' output to the standard output, followed by a newline.
mkPrintLazy :: Name -> Q Exp
mkPrintLazy name = [| TL.putStrLn . $(mkShowLazy name) |]

-- |
-- Generates a lambda expression which writes the given @data@ type or @newtype@
-- argument's strict 'TS.Text' output to the given file handle, followed by a newline.
mkHPrint :: Name -> Q Exp
mkHPrint name = [| \h -> TS.hPutStrLn h . $(mkShow name) |]

-- |
-- Generates a lambda expression which writes the given @data@ type or @newtype@
-- argument's lazy 'TL.Text' output to the given file handle, followed by a newline.
mkHPrintLazy :: Name -> Q Exp
mkHPrintLazy name = [| \h -> TL.hPutStrLn h . $(mkShowLazy name) |]

-- | Generates code to generate the 'Show' encoding of a number of constructors.
--   All constructors must be from the same type.
consToShow :: [Con] -> Q Exp
consToShow []   = error $ "Text.Show.Text.TH.consToShow: Not a single constructor given!"
consToShow cons = do
    p     <- newName "p"
    value <- newName "value"
    lam1E (if all isNullary cons then wildP else varP p)
        . lam1E (varP value)
        $ caseE (varE value) [encodeArgs p con | con <- cons]

-- | Generates code to generate the 'Show' encoding of a single constructor.
encodeArgs :: Name -> Con -> Q Match
encodeArgs _ (NormalC conName [])
    = match (conP conName [])
            (normalB [| fromString $(stringE (nameBase conName)) |])
            []
encodeArgs p (NormalC conName ts) = do
    args <- mapM newName ["arg" ++ P.show n | (_, n) <- zip ts [1 :: Int ..]]
    
    let showArgs    = map (appE [| showbPrec appPrec1 |] . varE) args
        mappendArgs = foldr1 (\v q -> [| $(v) <> s ' ' <> $(q) |]) showArgs
        namedArgs   = [| fromString $(stringE (nameBase conName)) <> s ' ' <> $(mappendArgs) |]
    
    match (conP conName $ map varP args)
          (normalB $ appE [| showbParen ($(varE p) > appPrec) |] namedArgs)
          []
encodeArgs p (RecC conName []) = encodeArgs p $ NormalC conName []
encodeArgs p (RecC conName ts) = do
    args <- mapM newName ["arg" ++ P.show n | (_, n) <- zip ts [1 :: Int ..]]
    
    let showArgs    = map (\(arg, (argName, _, _)) -> [| fromString $(stringE (nameBase argName)) <> fromString " = " <> showb $(varE arg) |])
                          $ zip args ts
        mappendArgs = foldr1 (\v q -> [| $(v) <> fromString ", " <> $(q) |]) showArgs
        namedArgs   = [| fromString $(stringE (nameBase conName)) <> s ' ' <> showbBraces $(mappendArgs) |]
    
    match (conP conName $ map varP args)
          (normalB $ appE [| showbParen ($(varE p) > appPrec) |] namedArgs)
          []
encodeArgs p (InfixC _ conName _) = do
    al   <- newName "argL"
    ar   <- newName "argR"
    info <- reify conName
    
    let conPrec = case info of
                       DataConI _ _ _ (Fixity prec _) -> prec
                       other -> error $ "Text.Show.Text.TH.encodeArgs: Unsupported type: " ++ P.show other
    
    match (infixP (varP al) conName (varP ar))
          (normalB $ appE [| showbParen ($(varE p) > conPrec) |]
                          [| showbPrec (conPrec + 1) $(varE al)
                          <> s ' '
                          <> fromString $(stringE (nameBase conName))
                          <> s ' '
                          <> showbPrec (conPrec + 1) $(varE ar)
                          |]
          )
          []
encodeArgs p (ForallC _ _ con) = encodeArgs p con

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

instanceCtxType :: Name -> [TyVarBndr] -> (Q Cxt, Q Type)
instanceCtxType name tvbs
    = let typeNames :: [Name]
          typeNames = map tvbName tvbs
          
          instanceType :: Q Type
          instanceType = foldl' appT (conT name) $ map varT typeNames
      in (applyCon ''Show typeNames name, instanceType)

-- | If constructor is nullary.
isNullary :: Con -> Bool
isNullary (NormalC _ []) = True
isNullary (RecC    _ []) = True
isNullary _              = False

-- | Surrounds a 'Builder' with braces.
showbBraces :: Builder -> Builder
showbBraces b = s '{' <> b <> s '}'

-- | Boilerplate for top level splices.
--
-- The given 'Name' must be from a type constructor. Furthermore, the
-- type constructor must be either a data type or a newtype. Any other
-- value will result in an exception.
withType :: Name
         -> ([TyVarBndr] -> [Con] -> Q a)
         -- ^ Function that generates the actual code. Will be applied
         -- to the type variable binders and constructors extracted
         -- from the given 'Name'.
         -> Q a
         -- ^ Resulting value in the 'Q'uasi monad.
withType name f = do
    info <- reify name
    case info of
      TyConI dec ->
        case dec of
          DataD    _ _ tvbs cons _ -> f tvbs cons
          NewtypeD _ _ tvbs con  _ -> f tvbs [con]
          other -> error $ "Text.Show.Text.TH.withType: Unsupported type: "
                          ++ P.show other
      _ -> error "Text.Show.Text.TH.withType: I need the name of a type."

-- | Extracts the name from a type variable binder.
tvbName :: TyVarBndr -> Name
tvbName (PlainTV  name)   = name
tvbName (KindedTV name _) = name

-- |
-- Applies a typeclass to several type parameters to produce the type predicate of an
-- instance declaration. If a recent version of Template Haskell is used, this function
-- will filter type parameters that have phantom roles (since they have no effect on
-- the instance declaration.
applyCon :: Name -> [Name] -> Name -> Q [Pred]
#if MIN_VERSION_template_haskell(2,9,0)
applyCon con typeNames targetData
    = map apply . nonPhantomNames typeNames <$> reifyRoles targetData
#else
applyCon con typeNames _
    = return $ map apply typeNames
#endif
  where
    apply :: Name -> Pred
    apply t = ClassP con [VarT t]

#if MIN_VERSION_template_haskell(2,9,0)
    -- Filters a list of tycon names based on their type roles.
    -- If a tycon has a phantom type role, remove it from the list.
    nonPhantomNames :: [Name] -> [Role] -> [Name]
    nonPhantomNames (_:ns) (PhantomR:rs) = nonPhantomNames ns rs
    nonPhantomNames (n:ns) (_:rs)        = n:(nonPhantomNames ns rs)
    nonPhantomNames []     _             = []
    nonPhantomNames _      []            = []
#endif