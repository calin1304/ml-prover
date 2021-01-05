module Main where

import System.Environment (getArgs)
import Text.Printf (printf)
import Control.Monad.State (runState)

import Parser.Syntax
import Parser.Lexer
import Parser.Parser
import Parser.ParserM (emptyEnv)

main :: IO ()
main =
    getArgs >>= readFile . head >>= \s -> do
        let lexemes = scanner s
            parsed = fmap ((`runState` emptyEnv) . parser) lexemes
        showSection "Lexer output" lexemes
        showSection "Parser output" (fst <$> parsed)
        showSection "Parser final state" (snd <$> parsed)

showSection :: Show a => String -> a -> IO ()
showSection title a = printf "\n%s\n---\n%s\n---\n" title (show a)