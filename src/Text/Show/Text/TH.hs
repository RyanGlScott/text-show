{-# LANGUAGE CPP, NoImplicitPrelude, TemplateHaskell #-}
module Text.Show.Text.TH where

import           Control.Applicative

import           Data.List

import           Language.Haskell.TH

import qualified Prelude as P
import           Prelude hiding (Show(..))

import           Text.Show.Text

deriveShow :: Name -> Q [Dec]
deriveShow name = withType name $ \tvbs cons -> fmap (:[]) $ fromCons tvbs cons
  where
    fromCons :: [TyVarBndr] -> [Con] -> Q Dec
    fromCons tvbs _ {- cons -} =
        instanceD (applyCon ''Show typeNames $ roles name)
                  (classType `appT` instanceType)
                  [ funD 'showbPrec
                         [ 
--                            clause []
--                                   (normalB $ consToJSON opts cons)
--                                   []
                           clause []
                                  (normalB $ varE 'undefined)
                                  []
                         ]
                  ]
      where
        classType :: Q Type
        classType = conT ''Show
        
        typeNames :: [Name]
        typeNames = map tvbName tvbs
        
        instanceType :: Q Type
        instanceType = foldl' appT (conT name) $ map varT typeNames
        
#if MIN_VERSION_template_haskell(2,9,0)
        roles :: Name -> Q [Role]
        roles = reifyRoles
#else
        roles :: Name -> Q [a]
        roles _ = return []
#endif

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

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

#if MIN_VERSION_template_haskell(2,9,0)
applyCon :: Name -> [Name] -> Q [Role] -> Q [Pred]
#else
applyCon :: Name -> [Name] -> Q [a]    -> Q [Pred]
#endif
applyCon con typeNames roles = map apply . nonPhantomNames typeNames <$> roles
  where apply t =
#if MIN_VERSION_template_haskell(2,10,0)
          AppT (ConT con) (VarT t)
#else
          ClassP con [VarT t]
#endif

-- | Filters a list of tycon names based on their type roles.
-- 
-- If a tycon has a phantom type role, remove it from the list.
#if MIN_VERSION_template_haskell(2,9,0)
nonPhantomNames :: [Name] -> [Role] -> [Name]
nonPhantomNames (_:ns) (PhantomR:rs) = nonPhantomNames ns rs
nonPhantomNames (n:ns) (_:rs)        = n:(nonPhantomNames ns rs)
nonPhantomNames []     _             = []
nonPhantomNames _      []            = []
#else
nonPhantomNames :: [Name] -> [a]    -> [Name]
nonPhantomNames = const
#endif