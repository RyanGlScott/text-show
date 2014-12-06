{-# LANGUAGE CPP, NoImplicitPrelude, TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.TH
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Exports 'deriveShow', which automatically derives a 'Show' instance for a
-- @data@ type or @newtype@. You need to enable the @TemplateHaskell@
-- language extension in order to use 'deriveShow'.
-- 
-- As an example:
-- 
-- @
-- &#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
-- import Text.Show.Text.TH (deriveShow)
-- 
-- data D a = Nullary
--          | Unary Int
--          | Product String Char a
--          | Record { testOne   :: Double
--                   , testTwo   :: Bool
--                   , testThree :: D a
--                   }
-- $(deriveShow ''D)
-- @
-- 
-- @D@ now has a 'Show' instance equivalent to that which would be generated
-- by a @deriving Show@ clause. 
-- 
-- Note that at the moment, 'deriveShow' does not support data families,
-- so it is impossible to use 'deriveShow' with @data instance@s or @newtype
-- instance@s.
----------------------------------------------------------------------------
module Text.Show.Text.TH (deriveShow) where

import           Control.Applicative ((<$>))

import           Data.List (foldl')
import           Data.Text.Lazy.Builder (Builder, fromString)

import           GHC.Show (appPrec, appPrec1)

import           Language.Haskell.TH

import qualified Prelude as P
import           Prelude hiding (Show)

import           Text.Show.Text.Class (Show(showb, showbPrec), showbParen)
import           Text.Show.Text.Instances ()
import           Text.Show.Text.Utils ((<>), s)

-- | Generates a 'Show' instance declaration for the given @data@ type or @newtype@.
deriveShow :: Name -> Q [Dec]
deriveShow name = withType name $ \tvbs cons -> (:[]) <$> fromCons tvbs cons
  where
    fromCons :: [TyVarBndr] -> [Con] -> Q Dec
    fromCons tvbs cons =
        instanceD (applyCon ''Show typeNames name)
                  (appT classType instanceType)
                  [ funD 'showbPrec [ clause [] (normalB $ consToShow cons) []
                                    ]
                  ]
      where
        classType :: Q Type
        classType = conT ''Show
        
        typeNames :: [Name]
        typeNames = map tvbName tvbs
        
        instanceType :: Q Type
        instanceType = foldl' appT (conT name) $ map varT typeNames

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