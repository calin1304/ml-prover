{
module Parser.Lexer (Lexeme (..), LexemeClass (..), scanner) where
import Data.Char (chr)
}

%wrapper "monad"

$whitechar = [ \t\n\r\f\v]
$special   = [\;\`\{\}]

$ascdigit  = 0-9
$digit     = [$ascdigit]

$ascsymbol = [\!\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$symbol    = [$ascsymbol] # [$special \_\:\"\']

$large     = [A-Z]
$small     = [a-z]
$alpha     = [$small $large]

$graphic   = [$small $large $symbol $digit $special \:\"\']

$idchar    = [$alpha $digit \'\-]
$symchar   = [$symbol \:]
$nl        = [\n\r]

@keyword = imports|from|derive|rule

@reservedop = ".." | ":" | "::" | "=" | \\ | "|" | "<-" | "->" | "@" | "~" | "=>"

@varid  = [$small \#] $idchar*
@conid  = $large $idchar*
@varsym = $symbol $symchar*
@consym = \: $symchar*

@decimal     = $digit+

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
     | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
     | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
     | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal)
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap

haskell :-

<0> $white+			{ skip }
<0> "--"\-*[^$symbol].*		{ skip }

<0> $special			                      { mkL LSpecial }
<0> module      	                          { mkL LModule }
<0> endmodule	                              { mkL LEndModule }
<0> meta\-symbol	                          { mkL LMetaSym }
<0> \[                                        { mkL LLBracket }
<0> \]                                        { mkL LRBracket }
<0> \(                                        { mkL LLParan }
<0> \)                                        { mkL LRParan }
<0> \,                                        { mkL LComma}
<0> @keyword			                      { mkL LKeyword }
<0> @conid \. @varid		                  { mkL LQVarId }
<0> @conid \. @conid		                  { mkL LQConId }
<0> @varid			                          { mkNamedL LVarId }
<0> @conid			                          { mkNamedL LConId }
<0> @reservedop			                      { mkL LReservedOp }
<0> @conid \. @varsym		                  { mkL LVarSym }
<0> @conid \. @consym		                  { mkL LConSym }
<0> @varsym			                          { mkL LVarSym }
<0> @consym			                          { mkL LConSym }
<0> @decimal                                  { readL LInteger }
<0> \' ($graphic # [\'\\] | " " | @escape) \' { mkL LChar }
<0> \" @string* \"		                      { mkL LString }

{
data Lexeme = L AlexPosn LexemeClass String 
    deriving (Show)

data LexemeClass =
    LInteger Int
  | LFloat
  | LChar
  | LString
  | LSpecial
  | LKeyword
  | LReservedOp
  | LVarId String
  | LQVarId
  | LConId String
  | LQConId
  | LVarSym
  | LQVarSym
  | LConSym
  | LQConSym
  | LModule
  | LEndModule
  | LMetaSym
  | LLBracket
  | LRBracket
  | LLParan
  | LRParan
  | LComma
  | LEOF 
  deriving (Show, Eq)
  
mkL :: LexemeClass -> AlexInput -> Int -> Alex Lexeme
mkL c (p, _, _, str) len = pure . L p c $ take len str

readL :: Read a => (a -> LexemeClass) -> AlexInput -> Int -> Alex Lexeme
readL c (p, _, _, str) len = pure $ L p c' str
  where
    c' = c . read $ take len str

mapL :: (String -> a) -> (a -> LexemeClass) -> AlexInput -> Int -> Alex Lexeme
mapL f c (p, _, _, str) len = pure $ L p c' str
  where
    c' = c . f $ take len str

mkNamedL :: (String -> LexemeClass) -> AlexInput -> Int -> Alex Lexeme
mkNamedL = mapL id

lexError s = do
    (p,c,_,input) <- alexGetInput
    let ps =
            if not (null input)
                then " before " ++ show (head input)
                else " at end of file"
    alexError $ showPosn p ++ ": " ++ s ++ ps
  where
    showPosn (AlexPn _ line col) = show line ++ ':': show col

scanner str =
    runAlex str $
        let loop = do
                tok@(L _t cl _) <- alexMonadScan
                if cl == LEOF then pure [] else (cl :) <$> loop
         in loop

alexEOF :: Alex Lexeme
alexEOF = pure $ L undefined LEOF ""
}
