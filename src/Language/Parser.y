{
{-# GHC_OPTIONS -w #-}

module Language.Parser
    ( parser
    , parseModule
    , parseDeclaration
    , parseExpression
    ) where

import Control.Monad.State (modify)
import Text.Printf (printf)

import Language.Lexer
import Language.Syntax
import Language.ParserM
}

%expect 0

%name parser
%name parseModule ModDef
%name parseDeclaration Declaration
%name parseExpression Expr

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

%nonassoc ident '(' from
%nonassoc APP

%%

Source :: { Source }
    : ModDefs { Source (reverse $1) }

------------------------
-- Module definitions --
------------------------

ModDefs :: { [ModDef] }
    : {- empty -} { [] }
    | ModDefs ModDef { $2 : $1 }

ModDef :: { ModDef }
  : module ident Declarations endmodule { ModDef $2 (reverse $3) }

----------------------------
-- Top-Level Declarations --
----------------------------

Declarations :: { [Declaration] } 
  : {- empty -} { [] }
  | Declarations Declaration { $2 : $1 }

Declaration :: { Declaration }
  : metaSym ident '[' Attrs ']' { MetaSym $2 $4 }
  | notation ident Signature ":=" Expr '[' Attrs ']' { Notation $2 $3 $5 $7 }
  | imports ident { Import $2 }

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

Exprs1 :: { [Expr] }
    : Expr { [$1] }
    | Exprs1 ',' Expr { $3 : $1 }

Expr :: { Expr }
  : ident               { Ident $1 }
  | Expr Expr %prec APP { Application $1 $2 }
  | '(' Expr ')'        { $2 }

Attrs :: { [SymAttr] }
    : {- empty -}           { [] }
    | Attrs1 { $1 }

Attrs1 :: { [SymAttr] }
    : ident             { [mkAttr $1 []] }
    | ident Decimals1   { [mkAttr $1 []] }
    | Attrs1 ',' ident  { mkAttr $3 [] : $1 }

Decimals1 :: { [Int] }
    : integer { [$1] }
    | Decimals1 integer { $2 : $1 }

{
parserError :: [LexemeClass] -> ParserM a
parserError (x:xs) =
    error $ printf "Error at token: %s (%d from end)\nRest: %s" (show x) (length xs) (show xs)
}