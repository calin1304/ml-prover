module Main where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.State    (runState)
import qualified System.Environment     as E (getArgs)
import qualified Data.Map as M (empty)
import Data.Foldable (traverse_)

import           Interp                 (interp, runInterpM, Env, InterpError)
import           Language.Lexer
import           Language.Parser
import           Language.ParserM       (ParserState(..))
import           Language.Syntax
import           Utils                  (showSection)

main :: IO ()
main = do
    parsed <- parseFile . head =<< E.getArgs 
    case parsed of
        Left e -> error e
        Right (Source mods) ->
            case mods of
                [] -> error "No modules parsed"
                mods -> showSection "result" $ map checkModule mods
                    -- let (a, st) = runInterpM (traverse interp decls) M.empty
                    -- showSection "Interp state" st
    -- getArgs >>= readFile . head >>= \s -> do
    --     let lexemes = scanner s
    --         parsed = fmap ((`runState` emptyEnv) . parser) lexemes
    --     showSection "Lexer output" lexemes
    --     showSection "Parser output" (fst <$> parsed)
    --     showSection "Parser final state" (snd <$> parsed)

checkModule :: ModDef -> (Either InterpError [()], Env)
checkModule (ModDef name decls) = runInterpM (traverse interp decls) M.empty

parseFile :: FilePath -> IO (Either String Source)
parseFile path = do
    contents <- readFile path
    let elexemes = scanner contents
        parsed = (\ls -> runState (parser ls) (ParserState [])) <$> elexemes
    showSection "Lexer output" elexemes
    showSection "Parser output" (fst <$> parsed)
    showSection "Parser final state" (snd <$> parsed)
    pure (fst <$> parsed)
