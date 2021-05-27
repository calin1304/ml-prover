module Language.ParserM where

import           Control.Monad       (unless)
import           Control.Monad.Extra (andM, whenM)
import           Control.Monad.State
import           Data.List           (intercalate)
import           Data.Maybe          (isJust)
import qualified Text.PrettyPrint    as PP

import           Language.Lexer
import           Language.Syntax
import           Utils

type ParserM a = State ParserState a

data ParserState = ParserState
    { names :: [(String, Declaration)]
    }

instance Show ParserState where
    show = PP.render . docParserState

runParserM :: ([LexemeClass] -> ParserM a)-> [LexemeClass] -> a
runParserM p ls = fst $ runState (p ls) emptyState
  where
    emptyState = ParserState []

-- | Add symbol definition to environment
addDeclaration :: Declaration -> ParserM ()
addDeclaration decl =
    case decl of
        -- Notation name _ def _ -> do
        --     -- Check if we have references to unknown symbols in definition
        --     whenM (checkDef def) (error $ printf "%s not defined" name)
        --     modify $ \env -> env { names = (name, expr) : names env }
        Rule name _ _ _ -> modify $ \env -> env { names = (name, decl) : names env }
        _ -> undefined

addDecl_ :: Declaration -> ParserM Declaration
addDecl_ decl = addDeclaration decl *> pure decl

getName :: String -> ParserM (Maybe Declaration)
getName name = gets (lookup name . names)

hasName :: String -> ParserM Bool
hasName = fmap isJust . getName

-- | Check for undefined symbols in expression
checkDef :: Expr -> ParserM Bool
checkDef (Application left right) = pure False -- andM [hasName left, allA checkDef right]
checkDef (Ident name)             = hasName name

---------------------
-- Pretty printing --
---------------------

docParserState :: ParserState -> PP.Doc
docParserState (ParserState names) = PP.braces $ PP.nest 4 docNames
  where
    docNames = PP.vcat $ map (PP.text . show . snd) names
