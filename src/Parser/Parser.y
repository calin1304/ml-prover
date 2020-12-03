{
module Parser.Parser (parser) where

import Parser.Lexer
}

%name parser
%tokentype { LexemeClass }
%error { parserError }

%token
    conName   { LConId $$ }
    varName   { LVarId $$ }
    module    { LModule }
    endmodule { LEndModule }
    metaSym   { LMetaSym }
    integer   { LInteger $$ }
    '['       { LLBracket }
    ']'       { LRBracket }
    '('       { LLParan }
    ')'       { LRParan }
    ','       { LComma }

%%

Mod : module conName Exprs endmodule { ModDef $2 $3 }

Exprs :
    {- empty -} { [] }
  | metaSym varName '[' Attrs ']' Exprs { MetaSym $2 $4 : $6 }

Attrs :
    {- empty -} { [] }
  | varName '(' AttrArgs ')' Attrs1 { mkAttr $1 $3 : $5}
  | varName  Attrs1                 { mkAttr $1 [] : $2}

Attrs1 :
    {- empty -} { [] }
  | ',' Attrs   { $2 }

AttrArgs :
    {- empty -} { [] }
  | AttrArg AttrArgs1 { $1 : $2 }

AttrArgs1 :
    {- empty -} { [] }
  | ',' AttrArg AttrArgs1 { $2 : $3 }

AttrArg : integer { $1 }

{
data ModDef =
    ModDef String [Expr]
    deriving Show

data Expr =
    MetaSym String [SymAttr]
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

parserError (x:xs) = error $ "Error at token: " ++ show x
}