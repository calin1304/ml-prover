module Language.Syntax where

import           Data.List       (intercalate)
import           Test.QuickCheck

import           Language.Lexer

newtype Source = Source [ModDef]
    deriving (Show)

data ModDef = ModDef String [Expr]
    deriving Show

-- TODO: Factor out the name ?
data Expr =
    MetaSym
        String    -- Meta symbol name
        [SymAttr] -- Meta symbol attributes
  | Notation
        String     -- Notation name
        Signature  -- Notation arguments
        SimpleExpr -- Notation expression
        [SymAttr]  -- Notation attributes
  | Import
        String -- Import name
  | Rule
        String       -- Rule name
        [String]     -- Rule arguments name
        [SimpleExpr] -- Rule premises
        SimpleExpr   -- Rule expression
  | Lemma
        String       -- Lemma name
        [String]     -- Lemma argument names
        [SimpleExpr] -- Lemma premises
        SimpleExpr   -- Lemma expression
        [Tactic]     -- Proof
    deriving (Show)

data Tactic =
    Intros [String]
  | Specialize SimpleExpr String
  | Apply SimpleExpr (Maybe String)
  | Exact String
    deriving (Show)

data SimpleExpr =
    EVar String
  | SVar String
  | Application String [SimpleExpr]
    deriving (Eq, Show)

instance Arbitrary SimpleExpr where
    arbitrary = oneof [ genEVar, genSVar, genApplication ]
      where
        genEVar = EVar . pure <$> elements ['a'..'z']
        genSVar = SVar . pure <$> elements ['A'..'Z']
        genApplication =
            Application
                <$> arbitrary `suchThat` (\s -> length s > 3 && all (`elem` ['Z'..'Z']) s)
                <*> arbitrary

-- instance Show SimpleExpr where
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
