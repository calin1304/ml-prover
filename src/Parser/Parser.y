{
module Parser.Parser (parser) where

import Parser.Lexer
}

%name parser
%tokentype { LexemeClass }
%error { parserError }

%token
    integer   { LInteger $$ }
    conId     { LConId $$ }
    varId     { LVarId $$ }
    module    { LKeyword "module" }
    endmodule { LKeyword "endmodule" }
    metaSym   { LKeyword "meta-symbol" }
    notation  { LKeyword "notation" }
    setVar    { LKeyword "SetVar" }
    elemVar   { LKeyword "ElemVar" }
    '['       { LSpecial '[' }
    ']'       { LSpecial ']' }
    '('       { LSpecial '(' }
    ')'       { LSpecial ')' }
    ','       { LSpecial ',' }
    ':'       { LSpecial ':' }
    ":="      { LReservedOp ":=" }

%%

Mod :: { ModDef }
  : module conId Exprs endmodule { ModDef $2 $3 }

Exprs :: { [Expr] } 
  : {- empty -} { [] }
  | Expr Exprs { $1 : $2 }

Expr :: { Expr }
  : metaSym varId '[' Attrs ']' { MetaSym $2 $4 }
  | notation varId Signature ":=" Application '[' Attrs ']' { Notation $2 $3 $5 $7 }

Signature :: { Signature }
  : {- empty -}  { NoSignature }
  | '(' SignatureList ')' { Signature $2 }
  | SignatureNamesList { SignatureNames $1 }

SignatureNamesList :: { [String] }
  : varId SignatureNamesList1 { $1 : $2 }
  | conId SignatureNamesList1 { $1 : $2 }

SignatureNamesList1 :: { [String] }
  : {- empty -} { [] }
  | varId SignatureNamesList1 { $1 : $2 } 
  | conId SignatureNamesList1 { $1 : $2 }

SignatureList :: { [ArgType] }
  : {- empty -} { [] }
  | ArgType SignatureList1 { $1 : $2 }

SignatureList1 :: { [ArgType] }
  : {- empty -} { [] }
  | ',' ArgType SignatureList { $2 : $3 } 

ArgType :: { ArgType }
  : varId ':' VarType { ArgType $1 $3 }
  | conId ':' VarType { ArgType $1 $3 }

VarType :: { VarType }
  : setVar  { mkVarType $1 }
  | elemVar { mkVarType $1 }

Application :: { Application }
  : varId ApplicationArgs { Application $1 $2 }

ApplicationArgs :: { [String] }
  : {- empty -} { [] }
  | ApplicationArg ApplicationArgs { $1 : $2 }

ApplicationArg :: { String }
  : varId { $1 }
  | conId { $1 }

Attrs :: { [SymAttr] }
  : {- empty -} { [] }
  | varId '(' AttrArgs ')' Attrs1 { mkAttr $1 $3 : $5}
  | varId  Attrs1                 { mkAttr $1 [] : $2}

Attrs1 :: { [SymAttr] }
  : {- empty -} { [] }
  | ',' Attrs   { $2 }

AttrArgs :: { [Int] }
  : {- empty -} { [] }
  | AttrArg AttrArgs1 { $1 : $2 }

AttrArgs1 :: { [Int] }
  : {- empty -} { [] }
  | ',' AttrArg AttrArgs1 { $2 : $3 }

AttrArg :: { Int }
  : integer { $1 }

{
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

parserError (x:xs) = error $ "Error at token: " ++ show x
}