module Parser.Syntax where

import Parser.Lexer

data ModDef =
    ModDef String [Expr]
    deriving Show

data Expr =
    MetaSym String [SymAttr]
  | Notation String Signature Application [SymAttr]
    deriving (Show)

data Application =
    Application String [String]
    deriving (Show)

data Signature =
    NoSignature
  | Signature [ArgType]
  | SignatureNames [String]
    deriving (Show)

data ArgType = ArgType String VarType deriving (Show)

data VarType = SetVar | ElemVar deriving (Show)

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
        LKeyword "ElemVar" -> ElemVar
        _ -> error "Invalid variable type"