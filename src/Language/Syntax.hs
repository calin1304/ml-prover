module Language.Syntax where

import           Data.List       (intercalate)
import           Test.QuickCheck

import           Language.Lexer

newtype Source = Source [ModDef]
    deriving (Show)

data ModDef = ModDef String [Declaration]
    deriving Show

data Declaration =
    MetaSym
        String   
        -- ^ Meta symbol name
        [SymAttr]
        -- ^ Meta symbol attributes
  | Notation
        String
        -- ^ Notation name
        Signature
        -- ^ Notation arguments
        Expr
        -- ^ Notation expression
        [SymAttr]
        -- ^ Notation attributes
  | Import
        String
        -- ^ Import name
  | Rule
        String
        -- ^ Rule name
        [String]
        -- ^ Argument names
        Expr
        -- ^ Definition
  | Lemma
        String       
        -- ^ Lemma name
        [String]
        -- ^ Argument names
        Expr
        -- ^ Definition
        [Tactic]
        -- ^ Proof
    deriving (Eq, Show)

class HasArgs a where
    getArgs :: a -> [String]
instance HasArgs Declaration where
    getArgs (Rule _ args _) = args
    getArgs _ = undefined

class HasDefinition a where
    getDefinition :: a -> Expr
instance HasDefinition Declaration where
    getDefinition (Rule _ _ (FromDerive _ e)) = e
    getDefinition _ = undefined

data Tactic =
    Intros [String]
  | Specialize Expr String
  | Apply Expr (Maybe String)
  | Exact String
    deriving (Eq, Show)

data Expr =
    Ident String
  | Application String [Expr]
  | FromDerive [Expr] Expr
    deriving (Eq, Show)

data Signature =
    NoSignature
  | Signature [Argument]
    deriving (Eq, Show)

data Argument = Argument String
    deriving (Eq, Show)

data SymAttr =
    Folded
  | Substitution
  | Binder
  | SetBinder
  | NotNegative
  | Arity Int
    deriving (Eq, Show)

mkAttr :: String -> [Int] -> SymAttr
mkAttr name args =
    case name of
        "folded"       -> Folded
        "binder"       -> Binder
        "substitution" -> Substitution
        "arity"        -> Arity (head args)
        "set-binder"   -> SetBinder
        "notNegative"  -> NotNegative
        _              -> error $ "Invalid attribute name: " ++ name
