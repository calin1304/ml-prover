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
    deriving (Show)

data Tactic =
    Intros [String]
  | Specialize Expr String
  | Apply Expr (Maybe String)
  | Exact String
    deriving (Show)

data Expr =
    EVar String
  | SVar String
  | Application String [Expr]
  | FromDerive [Expr] Expr
    deriving (Eq, Show)

instance Arbitrary Expr where
    arbitrary = oneof [ genEVar, genSVar, genApplication ]
      where
        genEVar = EVar . pure <$> elements ['a'..'z']
        genSVar = SVar . pure <$> elements ['A'..'Z']
        genApplication =
            Application
                <$> arbitrary `suchThat` (\s -> length s > 3 && all (`elem` ['Z'..'Z']) s)
                <*> arbitrary

-- instance Show Expr where
--     show (EVar s) = s
--     show (SVar s) = s
--     show (Application op xs) =
--         let sop =
--                 case op of
--                     "impl" ->  "(->)"
--                     _ -> op
--          in "(" ++ sop ++ " " ++ intercalate " " (map show xs) ++ ")"

data Signature =
    NoSignature
  | Signature [Argument]
    deriving (Show)

data Argument = Argument String VarType
    deriving (Show)

data VarType = SetVar | ElemVar
    deriving (Show)

data SymAttr =
    Folded
  | Substitution
  | Binder
  | SetBinder
  | NotNegative
  | Arity Int
    deriving (Show)

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

mkVarType :: LexemeClass -> VarType
mkVarType c =
    case c of
        LKeyword "SetVar" -> SetVar
        LKeyword "Var"    -> ElemVar
        _                 -> error "Invalid variable type"
