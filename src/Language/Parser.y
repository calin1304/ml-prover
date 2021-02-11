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

%token
    integer   { LInteger $$ }
    ident     { LIdent $$ }
    module    { LKeyword "module" }
    endmodule { LKeyword "endmodule" }
    metaSym   { LKeyword "meta-symbol" }
    notation  { LKeyword "notation" }
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
    ":="      { LReservedOp ":=" }

%%

Source :: { Source }
    : ModDefs { Source (reverse $1) }

ModDefs :: { [ModDef] }
    : {- empty -} { [] }
    | ModDefs ModDef { $2 : $1 }

ModDef :: { ModDef }
  : module ident Declarations endmodule { ModDef $2 (reverse $3) }

Declarations :: { [Declaration] } 
  : {- empty -} { [] }
  | Declarations Declaration { $2 : $1 }

Declaration :: { Declaration }
  : metaSym ident '[' Attrs ']' { MetaSym $2 $4 }
  | notation ident Signature ":=" Expr '[' Attrs ']' { Notation $2 $3 $5 $7 }
  | imports ident { Import $2 }
  | rule ident Idents ":=" Expr {% addDecl_ (Rule $2 (reverse $3) $5) }
  | lemma ident Idents ":=" Expr Proof { Lemma $2 $3 $5 $6 }

Proof :: { [Tactic] }
    : proof Tactics qed { reverse $2 }

Tactics :: { [Tactic] }
    : Tactic { [$1] }
    | Tactics Tactic { $2 : $1 }

Tactic :: { Tactic }
    : intros ident { Intros $2 }
    | specialize Expr as ident { Specialize $2 $4 }
    | apply Expr as ident { Apply $2 (Just $4) }
    | exact ident { Exact $2 }

Idents :: { [String] }
    : {- empty -} { [] }
    | Idents ident { $2 : $1 }

Signature :: { Signature }
  : SignatureList { Signature $1 }

SignatureList :: { [Argument] }
  : {- empty -} { [] }
  | SignatureList UntypedArg { $2 : $1 }

UntypedArg :: { Argument }
  : ident { Argument $1 }

Exprs :: { [Expr] }
    : Expr { [$1] }
    | Exprs ',' Expr { $3 : $1 }

Expr :: { Expr }
  : ident { Ident $1 }
  | Application { $1 }
  | from '[' ']' derive Expr { FromDerive [] $5 }
  | from '[' Exprs ']' derive Expr { FromDerive $3 $6 }
  | '(' Expr ')' { $2 }

Application :: { Expr }
    : ident ApplicationArgs { Application $1 (reverse $2) }

ApplicationArgs :: { [Expr] }
  : Expr { [$1] }
  | ApplicationArgs Expr { $2 : $1 }

Attrs :: { [SymAttr] }
    : {- empty -}           { [] }
    | Attrs1 { $1 }

Attrs1 :: { [SymAttr] }
    : ident             { [mkAttr $1 []] }
    | ident Decimals1   { [mkAttr $1 []] }
    | Attrs1 ',' ident  { mkAttr $3 [] : $2 }

Decimals1 :: { [Int] }
    : integer { [$1] }
    | Decimals1 integer { $2 : $1 }

{
parserError :: [LexemeClass] -> ParserM a
parserError (x:xs) =
    error $ printf "Error at token: %s (%d from end)\nRest: %s" (show x) (length xs) (show xs)
}