module Language.Syntax where

import           Data.List       (intercalate)
import           Test.QuickCheck
import Text.Printf (printf)

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
    deriving (Eq)

instance Show Declaration where
    show = \case
        MetaSym name attrs ->
            printf "%s %s" name (show attrs)
        Notation name sig def attrs ->
            printf ("%s %s := %s %s") name (show sig) (show def) (show attrs)
        Import name ->
            name
        Rule name args def ->
            if null args
                then printf "%s := %s" name (show def)
                else
                    printf
                        "%s := forall %s : %s"
                        name (intercalate " " args) (show def)
        Lemma name args def proof ->
            printf "_%s := forall %s : %s" name (intercalate " " args) (show def)

class HasArgs a where
    getArgs :: a -> [String]
instance HasArgs Declaration where
    getArgs (Rule _ args _) = args
    getArgs _               = undefined

class HasDefinition a where
    getDefinition :: a -> Expr
instance HasDefinition Declaration where
    getDefinition (Rule _ _ (FromDerive _ e)) = e
    getDefinition _                           = undefined

data Tactic =
    Intros String
  | Specialize Expr String
  | Apply Expr (Maybe String)
  | Exact String
    deriving (Eq, Show)

data Expr =
    Ident String
  | Application Expr Expr
  | FromDerive [Expr] Expr
    deriving (Eq)

appPrec = 10

instance Show Expr where
    -- showsPrec d (Ident s) = showString s
    -- showsPrec d (Application e1 e2) = showParen (d > appPrec)
    --     $ showsPrec (d+1) e1 . showString " " . showsPrec (d+1) e2
    -- showsPrec d (FromDerive pres e) = showsPrec d pres . showString " => " . showsPrec d e

    show (Ident s) = s
    show (Application e1 e2) = "(" ++ show e1 ++ ")" ++ " " ++ show e2
    show (FromDerive pres e) = show pres ++ " |- " ++ show e

-- I'm tired of writing long names
infixl 5 ##
(##) = Application

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
