{
module Language.Parser (parser) where

import Control.Monad.State (modify)
import Text.Printf (printf)

import Language.Lexer
import Language.Syntax
import Language.ParserM
}

%name parser
%tokentype { LexemeClass }
%error { parserError }
%monad { ParserM }

-- TODO: Add separate evarId and svarId

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
    exact     { LKeyword "exact"}
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
  : module conId Exprs endmodule { ModDef $2 (reverse $3) }

Exprs :: { [Expr] } 
  : {- empty -} { [] }
  | Exprs Expr { $2 : $1 }

Expr :: { Expr }
  : metaSym varId '[' Attrs ']' { MetaSym $2 $4 }
  | notation varId Signature ":=" SimpleExpr '[' Attrs ']' {% let n = Notation $2 $3 $5 $7 in (pure n <* addDefinition n) }
  | imports conId { Import $2 }
  | rule varId ConIds ":=" from '[' RulePres ']' derive SimpleExpr {% let r = Rule $2 $3 $7 $10 in (pure r <* addDefinition r) }
  | lemma varId ConIds ":=" from '[' RulePres ']' derive SimpleExpr Proof { Lemma $2 $3 $7 $10 $11 }

Proof :: { [Tactic] }
    : proof Tactics qed { $2 }

Tactics :: { [Tactic] }
    : Tactic { [$1] }
    | Tactics Tactic { $2 : $1 }

Tactic :: { Tactic }
    : intros ConIds { Intros (reverse $2) }
    | specialize Application as conId { Specialize $2 $4 }
    | apply Application as conId { Apply $2 (Just $4) }
    | exact conId { Exact $2 }

-- Premises
RulePres :: { [SimpleExpr] }
    : {- empty -} { [] }
    | SimpleExpr { [$1] }
    | RulePres ',' SimpleExpr { $3 : $1 }

ConIds :: { [String] }
    : {- empty -} { [] }
    | ConIds conId { $2 : $1 }

Signature :: { Signature }
  : {- empty -}        { NoSignature }
  | SignatureList      { Signature $1 }

SignatureList :: { [Argument] }
  : {- empty -} { [] }
  | SignatureList '(' TypedArg ')' { $3 : $1 }
  | SignatureList UntypedArg { $2 : $1 }

TypedArg :: { Argument }
  : varId ':' VarType { Argument $1 $3 }
  | conId ':' VarType { Argument $1 $3 }

UntypedArg :: { Argument }
  : varId { Argument $1 ElemVar }
  | conId { Argument $1 SetVar }

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
  : {- empty -}           { [] }
  | varId Attrs1          { mkAttr $1 [] : $2}
  | varId AttrArgs Attrs1 { mkAttr $1 $2 : $3}

Attrs1 :: { [SymAttr] }
  : {- empty -} { [] }
  | ',' Attrs   { $2 }

AttrArgs :: { [Int] }
  : {- empty -}       { [] }
  | AttrArgs integer { reverse ($2 : $1) }

{
parserError :: [LexemeClass] -> ParserM a
parserError (x:xs) =
    error $ printf "Error at token: %s (%d from end)" (show x) (length xs)
}