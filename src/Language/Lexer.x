{
module Language.Lexer (Lexeme (..), LexemeClass (..), scanner) where
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

@tacticsIds = intros|specialize|apply|exact
@keyword = @tacticsIds|module|endmodule|imports|meta\-symbol|notation|rule|from|derive|lemma|proof|qed|as|SetVar|Var
@reservedop = ":="
@ident  = [$large $small \_] $idchar*
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

mlp :-
    $white+			                          { skip }
    "--"\-*[^$symbol].*		                  { skip }
    $special			                      { mkCharL LSpecial }
    @keyword			                      { mkStringL LKeyword }
    @ident			                          { mkStringL LIdent }
    @reservedop			                      { mkStringL LReservedOp }
    @decimal                                  { readL LInteger }
    \' ($graphic # [\'\\] | " " | @escape) \' { mkCharL LChar }
    \" @string* \"		                      { mkStringL LString }

{
data Lexeme = L AlexPosn LexemeClass String 
    deriving (Show)

data LexemeClass =
    LInteger Int
  | LChar Char
  | LString String
  | LSpecial Char
  | LKeyword String
  | LReservedOp String
  | LIdent String
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
