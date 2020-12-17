module Main where

import System.Environment
import Text.Printf

import Parser.Lexer (scanner)
import Parser.Parser (parser)

main :: IO ()
main =
    getArgs >>= readFile . head >>= \s -> do
        let lexemes = scanner s
        printf
            "\nLexer output\n----\n%s\n----\n\nParser output\n----\n%s\n----\n"
            (show lexemes)
            (show $ fmap parser lexemes)
