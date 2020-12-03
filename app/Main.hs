module Main where

import Parser.Lexer (scanner)
import Parser.Parser (parser)

main :: IO ()
main = do
    s <- readFile "doc/examples/simple.ml"
    print $ scanner s
    print . fmap parser $ scanner s
