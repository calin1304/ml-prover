module Language.Syntax where

import Data.List (intercalate)

import Language.Lexer

newtype Source = Source [ModDef]
    deriving (Show)

data ModDef = ModDef String [Expr] 
    deriving Show

data Expr =
    MetaSym String [SymAttr]
  | Notation String Signature SimpleExpr [SymAttr]
  | Import String
  | Rule String [String] [SimpleExpr] SimpleExpr
  | Lemma String [String] [SimpleExpr] SimpleExpr [Tactic]
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
    deriving (Show)

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
        "folded" -> Folded
        "binder" -> Binder
        "substitution" -> Substitution
        "arity" -> Arity (head args)
        "set-binder" -> SetBinder
        "notNegative" -> NotNegative
        _ -> error $ "Invalid attribute name: " ++ name

mkVarType :: LexemeClass -> VarType
mkVarType c =
    case c of 
        LKeyword "SetVar" -> SetVar
        LKeyword "Var" -> ElemVar
        _ -> error "Invalid variable type"