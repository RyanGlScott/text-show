{-# LANGUAGE CPP, TemplateHaskell #-}
{-|
Module:      Text.Show.Text.TH.Internal
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Functions to mechanically derive 'T.Show' instances or splice
@show@-related expressions into Haskell source code. You need to enable
the @TemplateHaskell@ language extension in order to use this module.
-}
module Text.Show.Text.TH.Internal (
      -- * @deriveShow@
      -- $deriveShow
      deriveShow
    , deriveShowPragmas
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
      -- * Advanced pragma options
    , PragmaOptions(..)
    , defaultPragmaOptions
    , defaultInlineShowbPrec
    , defaultInlineShowb
    , defaultInlineShowbList
    ) where

import           Control.Applicative

import           Data.List (foldl', intersperse, isPrefixOf)
import qualified Data.Text    as TS ()
import qualified Data.Text.IO as TS (putStrLn, hPutStrLn)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (fromString, toLazyText)
import qualified Data.Text.Lazy    as TL ()
import qualified Data.Text.Lazy.IO as TL (putStrLn, hPutStrLn)

import           GHC.Show (appPrec, appPrec1)

import           Language.Haskell.TH

import           Prelude hiding (Show)

import qualified Text.Show as S (Show(show))
import qualified Text.Show.Text.Classes as T (Show)
import           Text.Show.Text.Classes (showb, showbPrec, showbList,
                                         showbListDefault, showbParen, showbSpace)
import           Text.Show.Text.Utils ((<>), s)

{- $deriveShow

'deriveShow' automatically generates a 'T.Show' instance declaration for a @data@
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
$('deriveShow' 'defaultOptions' ''D)
@

@D@ now has a 'T.Show' instance equivalent to that which would be generated
by a @deriving Show@ clause. 

Note that at the moment, there are a number of limitations to this approach:

* 'deriveShow' does not support data families, so it is impossible to use
  'deriveShow' with @data instance@s or @newtype instance@s.
  
* 'deriveShow' lacks the ability to properly detect data types with higher-kinded
   type parameters (e.g., @data HK f a = HK (f a)@), so it cannot create 'T.Show'
   for those data types.

* Some data constructors have arguments whose 'T.Show' instance depends on a
  typeclass besides 'T.Show'. For example, consider @newtype MyRatio a = MyRatio
  (Ratio a)@. @'Ratio' a@ is a 'T.Show' instance only if @a@ is an instance of both
  'Integral' and 'T.Show'. Unfortunately, 'deriveShow' cannot infer that 'a' must
  be an instance of 'Integral', so it cannot create a 'T.Show' instance for @MyRatio@.

-}

-- | Generates a 'T.Show' instance declaration for the given @data@ type or @newtype@.
deriveShow :: Name -- ^ Name of the data type to make an instance of 'T.Show'
           -> Q [Dec]
deriveShow = deriveShowPragmas defaultPragmaOptions

-- | Generates a 'T.Show' instance declaration for the given @data@ type or @newtype@.
-- You shouldn't need to use this function unless you know what you are doing.
-- 
-- Unlike 'deriveShow', this function allows configuration of whether to inline
-- 'showbPrec', 'showb', or 'showbList'. It also allows for specializing instances
-- certain types. For example:
-- 
-- @
-- data ADT a = ADT a
-- $(deriveShowPragmas 'defaultInlineShowbPrec' {
--                         specializeTypes = [t| Int |]
--                      }
--                      ''ADT)
-- @
-- 
-- This declararation would produce code like this:
-- 
-- @
-- instance Show a => Show (ADT a) where
--     {-# INLINE showbPrec #-}
--     {-# SPECIALIZE instance Show (ADT Int) #-}
--     showbPrec = ...
-- @
deriveShowPragmas :: PragmaOptions -- ^ Specifies what pragmas to generate with this instance
                  -> Name          -- ^ Name of the data type to make an instance of 'T.Show'
                  -> Q [Dec]
deriveShowPragmas opts dataName =
    withType dataName $ \tvbs cons -> (:[]) <$> fromCons tvbs cons
  where
    fromCons :: [TyVarBndr] -> [Con] -> Q Dec
    fromCons tvbs cons =
        instanceD (applyCon ''T.Show typeNames dataName)
                  (appT classType instanceType)
                  ([ funD 'showbPrec [ clause []
                                              (normalB $ consToShow cons)
                                              []
                                     ]
                   ] ++ inlineShowbPrecDec
                     ++ inlineShowbDec
                     ++ inlineShowbListDec
                     ++ specializeDecs
                  )
        
      where
          typeNames :: [Name]
          typeNames = map tvbName tvbs
          
          instanceType :: Q Type
          instanceType = foldl' appT (conT dataName) $ map varT typeNames
          
          classType :: Q Type
          classType = conT ''T.Show
          
          inline :: (PragmaOptions -> Bool) -> Name -> [Q Dec]
          inline isInlining funName
              | isInlining opts = [ pragInlD funName
#if MIN_VERSION_template_haskell(2,8,0)
                                             Inline FunLike AllPhases
#else
                                             (inlineSpecNoPhase True False)
#endif
                                  ]
              | otherwise       = []
          
          inlineShowbPrecDec :: [Q Dec]
          inlineShowbPrecDec = inline inlineShowbPrec 'showbPrec
          
          inlineShowbDec :: [Q Dec]
          inlineShowbDec = inline inlineShowb 'showb

          inlineShowbListDec :: [Q Dec]
          inlineShowbListDec = inline inlineShowbList 'showbList
          
          specializeDecs :: [Q Dec]
#if MIN_VERSION_template_haskell(2,8,0)
          specializeDecs = (fmap . fmap) (PragmaD
                                             . SpecialiseInstP
                                             . AppT (ConT ''T.Show)
                                         )
                                         (specializeTypes opts)
#else
          -- There doesn't appear to be an equivalent of SpecialiseInstP in early
          -- versions Template Haskell.
          specializeDecs = []
#endif

{- $mk

There may be scenarios in which you want to show an arbitrary @data@ type or @newtype@
without having to make the type an instance of 'T.Show'. For these cases,
"Text.Show.Text.TH" provide several functions (all prefixed with @mk@) that splice
the appropriate lambda expression into your source code.

As an example, suppose you have @data ADT = ADTCon@, which is not an instance of 'T.Show'.
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

-- | Generates a lambda expression which converts the given @data@ type or @newtype@
-- to a strict 'TS.Text'.
mkShow :: Name -> Q Exp
mkShow name = [| toStrict . $(mkShowLazy name) |]

-- | Generates a lambda expression which converts the given @data@ type or @newtype@
-- to a lazy 'TL.Text'.
mkShowLazy :: Name -> Q Exp
mkShowLazy name = [| toLazyText . $(mkShowb name) |]

-- | Generates a lambda expression which converts the given @data@ type or @newtype@
-- to a strict 'TS.Text' with the given precedence.
mkShowPrec :: Name -> Q Exp
mkShowPrec name = [| \p -> toStrict . $(mkShowPrecLazy name) p |]

-- | Generates a lambda expression which converts the given @data@ type or @newtype@
-- to a lazy 'TL.Text' with the given precedence.
mkShowPrecLazy :: Name -> Q Exp
mkShowPrecLazy name = [| \p -> toLazyText . $(mkShowbPrec name) p |]

-- | Generates a lambda expression which converts the given list of @data@ types or
-- @newtype@s to a strict 'TS.Text' in which the values are surrounded by square
-- brackets and each value is separated by a comma.
mkShowList :: Name -> Q Exp
mkShowList name = [| toStrict . $(mkShowListLazy name) |]

-- | Generates a lambda expression which converts the given list of @data@ types or
-- @newtype@s to a lazy 'TL.Text' in which the values are surrounded by square
-- brackets and each value is separated by a comma.
mkShowListLazy :: Name -> Q Exp
mkShowListLazy name = [| toLazyText . $(mkShowbList name) |]

-- | Generates a lambda expression which converts the given @data@ type or @newtype@
-- to a 'Builder'.
mkShowb :: Name -> Q Exp
mkShowb name = mkShowbPrec name `appE` [| 0 :: Int |]

-- | Generates a lambda expression which converts the given @data@ type or @newtype@
-- to a 'Builder' with the given precedence.
mkShowbPrec :: Name -> Q Exp
mkShowbPrec name = withType name $ const consToShow

-- | Generates a lambda expression which converts the given list of @data@ types or
-- @newtype@s to a 'Builder' in which the values are surrounded by square brackets
-- and each value is separated by a comma.
mkShowbList :: Name -> Q Exp
mkShowbList name = [| showbListDefault $(mkShowb name) |]

-- | Generates a lambda expression which writes the given @data@ type or @newtype@
-- argument's strict 'TS.Text' output to the standard output, followed by a newline.
mkPrint :: Name -> Q Exp
mkPrint name = [| TS.putStrLn . $(mkShow name) |]

-- | Generates a lambda expression which writes the given @data@ type or @newtype@
-- argument's lazy 'TL.Text' output to the standard output, followed by a newline.
mkPrintLazy :: Name -> Q Exp
mkPrintLazy name = [| TL.putStrLn . $(mkShowLazy name) |]

-- | Generates a lambda expression which writes the given @data@ type or @newtype@
-- argument's strict 'TS.Text' output to the given file handle, followed by a newline.
mkHPrint :: Name -> Q Exp
mkHPrint name = [| \h -> TS.hPutStrLn h . $(mkShow name) |]

-- | Generates a lambda expression which writes the given @data@ type or @newtype@
-- argument's lazy 'TL.Text' output to the given file handle, followed by a newline.
mkHPrintLazy :: Name -> Q Exp
mkHPrintLazy name = [| \h -> TL.hPutStrLn h . $(mkShowLazy name) |]

-- | Options that specify what @INLINE@ or @SPECIALIZE@ pragmas to generate with
-- a 'T.Show' instance.
data PragmaOptions = PragmaOptions {
    inlineShowbPrec  :: Bool    -- ^ Whether to inline 'showbPrec'
  , inlineShowb      :: Bool    -- ^ Whether to inline 'showb'
  , inlineShowbList  :: Bool    -- ^ Whether to inline 'showbList'
  , specializeTypes :: [Q Type] -- ^ Types for which to create specialized instance declarations
}

-- | Do not generate any pragmas with a 'T.Show' instance.
defaultPragmaOptions :: PragmaOptions
defaultPragmaOptions = PragmaOptions False False False []

-- | Inline the 'showbPrec' function in a 'T.Show' instance.
defaultInlineShowbPrec :: PragmaOptions
defaultInlineShowbPrec = defaultPragmaOptions { inlineShowbPrec = True }

-- | Inline the 'showb' function in a 'T.Show' instance
defaultInlineShowb :: PragmaOptions
defaultInlineShowb = defaultPragmaOptions { inlineShowb = True }

-- | Inline the 'showbList' function in a 'T.Show' instance.
defaultInlineShowbList :: PragmaOptions
defaultInlineShowbList = defaultPragmaOptions { inlineShowbList = True }

-- | Generates code to generate the 'T.Show' encoding of a number of constructors.
-- All constructors must be from the same type.
consToShow :: [Con] -> Q Exp
consToShow []   = error $ "Text.Show.Text.TH.consToShow: Not a single constructor given!"
consToShow cons = do
    p     <- newName "p"
    value <- newName "value"
    lam1E (varP p)
        . lam1E (varP value)
        . caseE (varE value)
        $ map (encodeArgs p) cons

-- | Generates code to generate the 'T.Show' encoding of a single constructor.
encodeArgs :: Name -> Con -> Q Match
encodeArgs p (NormalC conName [])
    = match (conP conName [])
            (normalB [| intConst (fromString $(stringE (nameBase conName))) $(varE p) |])
            []
encodeArgs p (NormalC conName [_]) = do
    arg <- newName "arg"
    
    let showArg  = [| showbPrec appPrec1 $(varE arg) |]
        namedArg = [| fromString $(stringE (nameBase conName)) <> showbSpace <> $(showArg) |] 
    
    match (conP conName [varP arg])
          (normalB [| showbParen ($(varE p) > appPrec) $(namedArg) |])
          []
encodeArgs p (NormalC conName ts) = do
    args <- mapM newName ["arg" ++ S.show n | (_, n) <- zip ts [1 :: Int ..]]
    
    if isNonUnitTuple conName
       then do
           let showArgs       = map (appE [| showb |] . varE) args
               parenCommaArgs = [| s '(' |] : intersperse [| s ',' |] showArgs
               mappendArgs    = foldr (flip infixApp [| (<>) |])
                                      [| s ')' |]
                                      parenCommaArgs
           
           match (conP conName $ map varP args)
                 (normalB [| intConst $(mappendArgs) $(varE p) |])
                 []
       else do
           let showArgs = map (appE [| showbPrec appPrec1 |] . varE) args
               mappendArgs = foldr1 (\v q -> [| $(v) <> showbSpace <> $(q) |]) showArgs
               namedArgs   = [| fromString $(stringE (nameBase conName)) <> showbSpace <> $(mappendArgs) |]
           
           match (conP conName $ map varP args)
                 (normalB [| showbParen ($(varE p) > appPrec) $(namedArgs) |])
                 []
encodeArgs p (RecC conName []) = encodeArgs p $ NormalC conName []
encodeArgs p (RecC conName ts) = do
    args <- mapM newName ["arg" ++ S.show n | (_, n) <- zip ts [1 :: Int ..]]
    
    let showArgs       = concatMap (\(arg, (argName, _, _))
                                      -> [ [| fromString $(stringE (nameBase argName)) |]
                                         , [| fromString " = "                         |]
                                         , [| showb $(varE arg)                        |]
                                         , [| fromString ", "                          |]
                                         ]
                                   )
                            (zip args ts)
        braceCommaArgs = [| s '{' |] : take (length showArgs - 1) showArgs
        mappendArgs    = foldr (flip infixApp [| (<>) |])
                           [| s '}' |]
                           braceCommaArgs
        namedArgs      = [| fromString $(stringE (nameBase conName)) <> showbSpace <> $(mappendArgs) |]
    
    match (conP conName $ map varP args)
          (normalB [| showbParen ($(varE p) > appPrec) $(namedArgs) |])
          []
encodeArgs p (InfixC _ conName _) = do
    al   <- newName "argL"
    ar   <- newName "argR"
    info <- reify conName
    
    let conPrec = case info of
                       DataConI _ _ _ (Fixity prec _) -> prec
                       other -> error $ "Text.Show.Text.TH.encodeArgs: Unsupported type: " ++ S.show other
    
    match (infixP (varP al) conName (varP ar))
          (normalB $ appE [| showbParen ($(varE p) > conPrec) |]
                          [| showbPrec (conPrec + 1) $(varE al)
                          <> showbSpace
                          <> fromString $(stringE (nameBase conName))
                          <> showbSpace
                          <> showbPrec (conPrec + 1) $(varE ar)
                          |]
          )
          []
encodeArgs p (ForallC _ _ con) = encodeArgs p con

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

-- TODO: There's got to be a better way of doing this.
-- | Checks if a 'Name' represents a tuple type constructor (other than '()')/
isNonUnitTuple :: Name -> Bool
isNonUnitTuple = isPrefixOf "(," . nameBase

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
          other -> error $ "Text.Show.Text.TH.withType: Unsupported type "
                          ++ S.show other ++ ". Must be a data type or newtype."
      ClassI{} -> error "Text.Show.Text.TH.withType: Cannot use a typeclass name."
#if MIN_VERSION_template_haskell(2,7,0)
      FamilyI (FamilyD DataFam _ _ _) _ ->
        error "Text.Show.Text.TH.withType: Data families are not supported as of now."
      FamilyI (FamilyD TypeFam _ _ _) _ ->
        error "Text.Show.Text.TH.withType: Cannot use a type family name."
#endif
      _ -> error "Text.Show.Text.TH.withType: I need the name of a plain type constructor."

-- | Extracts the name from a type variable binder.
tvbName :: TyVarBndr -> Name
tvbName (PlainTV  name)   = name
tvbName (KindedTV name _) = name

-- | Applies a typeclass to several type parameters to produce the type predicate of
-- an instance declaration. If a recent version of Template Haskell is used, this
-- function will filter type parameters that have phantom roles (since they have no
-- effect on the instance declaration.
applyCon :: Name -> [Name] -> Name -> Q [Pred]
#if MIN_VERSION_template_haskell(2,9,0)
applyCon con typeNames targetData
    = map apply . nonPhantomNames typeNames <$> reifyRoles targetData
#else
applyCon con typeNames _
    = pure $ map apply typeNames
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