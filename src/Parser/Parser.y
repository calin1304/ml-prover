{
module Parser.Parser (parser) where

import Parser.Lexer
import Parser.Syntax
import Text.Printf (printf)
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
    elemVar   { LKeyword "Var" }
    imports   { LKeyword "imports" }
    rule      { LKeyword "rule" }
    from      { LKeyword "from" }
    derive    { LKeyword "derive" }
    lemma     { LKeyword "lemma" }
    proof     { LKeyword "proof" }
    qed       { LKeyword "qed" }
    intros    { LKeyword "intros" }
    specialize { LKeyword "specialize"}
    apply     { LKeyword "apply"}
    as        { LKeyword "as" }
    '['       { LSpecial '[' }
    ']'       { LSpecial ']' }
    '('       { LSpecial '(' }
    ')'       { LSpecial ')' }
    ','       { LSpecial ',' }
    ':'       { LSpecial ':' }
    ":="      { LReservedOp ":=" }

%%

Source :: { Source }
    : ModDefs { Source (reverse $1) }

ModDefs :: { [ModDef] }
    : {- empty -} { [] }
    | ModDefs ModDef { $2 : $1 }

ModDef :: { ModDef }
  : module conId Exprs endmodule { ModDef $2 $3 }

Exprs :: { [Expr] } 
  : {- empty -} { [] }
  | Expr Exprs { $1 : $2 }

Expr :: { Expr }
  : metaSym varId '[' Attrs ']' { MetaSym $2 $4 }
  | notation varId Signature ":=" SimpleExpr '[' Attrs ']' { Notation $2 $3 $5 $7 }
  | imports conId { Import $2 }
  | rule varId RuleBinders ":=" from '[' RulePres ']' derive SimpleExpr  { Rule $2 $3 $7 $10}
  | lemma varId RuleBinders ":=" from '[' RulePres ']' derive SimpleExpr Proof { Lemma $2 $3 $7 $10 $11 }

Proof :: { [Tactic] }
    : proof Tactics qed { $2 }

Tactics :: { [Tactic] }
    : Tactic { [$1] }
    | Tactics Tactic { $2 : $1 }

Tactic :: { Tactic }
    : intros conId { Intros $2 }
    | specialize Application as conId { Specialize $2 $4 }
    | apply Application { Apply $2 }


RuleBinders :: { [String] }
    : conId { [$1]}
    | RuleBinders conId { $2 : $1 }

RulePres :: { [SimpleExpr] }
    : {- empty -} { [] }
    | SimpleExpr { [$1] }
    | RulePres ',' SimpleExpr { $3 : $1 }

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

SimpleExpr :: { SimpleExpr }
  : varId { EVar $1 }
  | conId { SVar $1 }
  | Application { $1 }
  | '(' SimpleExpr ')' { $2 }

Application :: { SimpleExpr }
    : varId ApplicationArgs { Application $1 $2 }
    | conId ApplicationArgs { Application $1 $2 }
    | '(' Application ')' { $2 }

ApplicationArgs :: { [SimpleExpr] }
  : SimpleExpr { [$1] }
  | ApplicationArgs SimpleExpr { reverse $ $2 : $1 }

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
parserError :: [LexemeClass] -> a
parserError (x:xs) = error $ printf "Error at token: %s (%d from end)" (show x) (length xs)
}