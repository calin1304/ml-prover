module Main where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.State    (runState)
import qualified System.Environment     as E (getArgs)
import           Text.Printf            (printf)

import           Interp                 (interp, runInterpM)
import           Language.Lexer
import           Language.Parser
import           Language.ParserM       (emptyEnv)
import           Language.Syntax
import           Utils                  (showSection)

main :: IO ()
main = do
    args <- E.getArgs
    parsed <- parseFile $ head args
    case parsed of
        Left e -> error e
        Right (Source mods) ->
            case mods of
                [] -> error "No modules parsed"
                ((ModDef name decls):xs) -> do
                    let (a, st) = runInterpM (traverse interp decls) []
                    showSection "Interp state" st
    -- getArgs >>= readFile . head >>= \s -> do
    --     let lexemes = scanner s
    --         parsed = fmap ((`runState` emptyEnv) . parser) lexemes
    --     showSection "Lexer output" lexemes
    --     showSection "Parser output" (fst <$> parsed)
    --     showSection "Parser final state" (snd <$> parsed)

parseFile :: FilePath -> IO (Either String Source)
parseFile path = do
    contents <- readFile path
    let elexemes = scanner contents
        parsed = (\ls -> runState (parser ls) emptyEnv) <$> elexemes
    showSection "Lexer output" elexemes
    showSection "Parser output" (fst <$> parsed)
    showSection "Parser final state" (snd <$> parsed)
    pure (fst <$> parsed)
