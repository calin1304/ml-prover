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
    , (#)
    ) where

import           Data.String (IsString, fromString)

-- | A ML prover source consists of multiple module definitions.
newtype Source = Source [ModDef]
    deriving (Show)

-- | Module definition.
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

class HasDefinition a where
    getDefinition :: a -> ([Expr], Expr)

instance HasDefinition Declaration where
    getDefinition (Rule _ hs e) = (hs, e)
    getDefinition _             = undefined

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
    fromString = Ident

-- I'm tired of writing long names
infixl 5 #

-- | Operator which creates the application of second expression to the first one.
(#) :: Expr -> Expr -> Expr
(#) = Application

-- | Symbol signature.
data Signature =
    NoSignature
  | Signature [Argument]
    deriving (Eq, Show)

-- | Symbol argument.
newtype Argument = Argument String
    deriving (Eq, Show)

-- | Symbol attributes.
data SymAttr =
    Folded
  | Substitution
  | Binder
  | SetBinder
  | NotNegative
  | Arity Int -- ^ Symbol arity. NOTE: How is this related to `Signature` data type?
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
