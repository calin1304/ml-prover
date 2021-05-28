module Main
    ( main
    ) where

import           Control.Monad.State (runState)
import qualified Data.Map            as M (empty)
import qualified System.Environment  as E (getArgs)

import           Interp              (Env, InterpError, interp, runInterpM)
import           Language.Lexer      (scanner)
import           Language.Parser     (parser)
import           Language.ParserM    (ParserState (ParserState))
import           Language.Syntax     (ModDef (ModDef), Source (Source))
import           Utils               (showSection)

main :: IO ()
main = do
    parsed <- parseFile . head =<< E.getArgs
    case parsed of
        Left e -> error e
        Right (Source mods) ->
            case mods of
                [] -> error "No modules parsed"
                ms -> showSection "result" $ map checkModule ms
                    -- let (a, st) = runInterpM (traverse interp decls) M.empty
                    -- showSection "Interp state" st
    -- getArgs >>= readFile . head >>= \s -> do
    --     let lexemes = scanner s
    --         parsed = fmap ((`runState` emptyEnv) . parser) lexemes
    --     showSection "Lexer output" lexemes
    --     showSection "Parser output" (fst <$> parsed)
    --     showSection "Parser final state" (snd <$> parsed)

checkModule :: ModDef -> (Either InterpError [()], Env)
checkModule (ModDef _ decls) = runInterpM (traverse interp decls) M.empty

parseFile :: FilePath -> IO (Either String Source)
parseFile path = do
    contents <- readFile path
    let elexemes = scanner contents
        parsed = (\ls -> runState (parser ls) (ParserState [])) <$> elexemes
    showSection "Lexer output" elexemes
    showSection "Parser output" (fst <$> parsed)
    showSection "Parser final state" (snd <$> parsed)
    pure (fst <$> parsed)
