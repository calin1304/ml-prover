module Main where

import qualified System.Environment as E (getArgs)
import Text.Printf (printf)
import Control.Monad.State (runState)
import Control.Monad.IO.Class (MonadIO)

import Language.Syntax
import Language.Lexer
import Language.Parser
import Language.ParserM (emptyEnv)

main :: IO ()
main = do
    args <- E.getArgs
    parsed <- parseFile $ head args
    pure ()
    -- getArgs >>= readFile . head >>= \s -> do
    --     let lexemes = scanner s
    --         parsed = fmap ((`runState` emptyEnv) . parser) lexemes
    --     showSection "Lexer output" lexemes
    --     showSection "Parser output" (fst <$> parsed)
    --     showSection "Parser final state" (snd <$> parsed)

showSection :: Show a => String -> a -> IO ()
showSection title a = printf "\n%s\n---\n%s\n---\n" title (show a)

parseFile :: FilePath -> IO (Either String Source)
parseFile path = do
    contents <- readFile path
    let elexemes = scanner contents
        parsed = (\ls -> runState (parser ls) emptyEnv) <$> elexemes
    showSection "Lexer output" elexemes
    showSection "Parser output" (fst <$> parsed)
    showSection "Parser final state" (snd <$> parsed)
    pure (fst <$> parsed)
