module Language.Syntax
    ( Source (..)
    , ModDef (..)
    , Declaration (..)
    , Expr (..)
    , Tactic (..)
    , SymAttr (..)
    , Argument (..)
    , Signature (..)
    , getDefinition
    , mkAttr
    , (##)
    , axiom
    ) where

import           Data.String (IsString, fromString)

newtype Source = Source [ModDef]
    deriving (Show)

data ModDef = ModDef String [Declaration]
    deriving (Show)

data Declaration =
    MetaSym
        String    -- ^ Meta symbol name
        [SymAttr] -- ^ Meta symbol attributes
  | Notation
        String    -- ^ Notation name
        Signature -- ^ Notation arguments
        Expr      -- ^ Notation expression
        [SymAttr] -- ^ Notation attributes
  | Import
        String -- ^ Import name
  | Rule
        String   -- ^ Rule name
        [String] -- ^ Argument names
        [Expr]   -- ^ Hypotheses
        Expr     -- ^ Conclusion
  | Lemma
        String   -- ^ Lemma name
        [String] -- ^ Argument names
        [Expr]   -- ^ Hypotheses
        Expr     -- ^ Conclusion
        [Tactic] -- ^ Proof
    deriving (Eq, Show)

axiom :: String -> Expr -> Declaration
axiom name = Rule name [] []

class HasDefinition a where
    getDefinition :: a -> ([Expr], Expr)

instance HasDefinition Declaration where
    getDefinition (Rule _ _ hs e) = (hs, e)
    getDefinition _               = undefined

data Tactic =
    Intros String
  | Specialize Expr String
  | Apply Expr (Maybe String)
  | Exact String
    deriving (Eq, Show)

data Expr =
    Ident String
  | Application Expr Expr
    deriving (Eq, Show)

instance IsString Expr where
    fromString s = Ident s

-- I'm tired of writing long names
infixl 5 ##

(##) :: Expr -> Expr -> Expr
(##) = Application

data Signature =
    NoSignature
  | Signature [Argument]
    deriving (Eq, Show)

newtype Argument = Argument String
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
