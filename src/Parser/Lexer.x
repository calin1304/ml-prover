{
module Parser.Lexer (Lexeme (..), LexemeClass (..), scanner) where
import Data.Char (chr)
}

%wrapper "monad"

$whitechar = [ \t\n\r\f\v]
$special   = [\:\(\)\,\;\[\]\`\{\}]

$ascdigit  = 0-9
$digit     = [$ascdigit]

$ascsymbol = [\!\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$symbol    = [$ascsymbol] # [$special \_\:\"\']

$large     = [A-Z]
$small     = [a-z]
$alpha     = [$small $large]

$graphic   = [$small $large $symbol $digit $special \:\"\']

$idchar    = [$alpha $digit \'\-\_]
$symchar   = [$symbol \:]
$nl        = [\n\r]

@keyword = module|endmodule|imports|meta\-symbol|notation|rule|from|derive|lemma|intros|proof|qed|specialize|apply|as|SetVar|Var

@reservedop = ":="

@varid  = [$small \#\_] $idchar*
@conid  = [$large \_] $idchar*
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

<0> $white+			                          { skip }
<0> "--"\-*[^$symbol].*		                  { skip }
<0> $special			                      { mkCharL LSpecial }
<0> @keyword			                      { mkStringL LKeyword }
<0> @varid			                          { mkStringL LVarId }
<0> @conid			                          { mkStringL LConId }
<0> @reservedop			                      { mkStringL LReservedOp }
<0> @varsym			                          { mkL LVarSym }
<0> @decimal                                  { readL LInteger }
<0> \' ($graphic # [\'\\] | " " | @escape) \' { mkCharL LChar }
<0> \" @string* \"		                      { mkStringL LString }

{
data Lexeme = L AlexPosn LexemeClass String 
    deriving (Show)

data LexemeClass =
    LInteger Int
  | LFloat
  | LChar Char
  | LString String
  | LSpecial Char
  | LKeyword String
  | LReservedOp String
  | LVarId String
  | LQVarId
  | LConId String
  | LQConId
  | LVarSym
  | LQVarSym
  | LConSym
  | LQConSym
  | LLBracket
  | LRBracket
  | LLParan
  | LRParan
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

mkStringL :: (String -> LexemeClass) -> AlexInput -> Int -> Alex Lexeme
mkStringL = mapL id

mkCharL :: (Char -> LexemeClass) -> AlexInput -> Int -> Alex Lexeme
mkCharL c (p, _, _, str) _ = pure . L p (c $ head str) $ take 1 str

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
