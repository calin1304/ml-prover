module Language.ParserM where

import           Control.Monad       (unless)
import           Control.Monad.Extra (andM, whenM)
import           Control.Monad.State
import           Data.Maybe          (isJust)
import           Text.Printf         (printf)

import           Language.Syntax
import           Utils

type ParserM a = State Env a

newtype Env = Env
    { names :: [(String, Declaration)]
    }
    deriving (Show)

emptyEnv :: Env
emptyEnv = Env
    { names = []
    }

-- | Add symbol definition to environment
addDeclaration :: Declaration -> ParserM ()
addDeclaration decl =
    case decl of
        -- Notation name _ def _ -> do
        --     -- Check if we have references to unknown symbols in definition
        --     whenM (checkDef def) (error $ printf "%s not defined" name)
        --     modify $ \env -> env { names = (name, expr) : names env }
        Rule name _ _ ->
            modify $ \env -> env { names = (name, decl) : names env }
        _ -> undefined

addDecl_ :: Declaration -> ParserM Declaration
addDecl_ decl = addDeclaration decl *> pure decl

getName :: String -> ParserM (Maybe Declaration)
getName name = gets (lookup name . names)

hasName :: String -> ParserM Bool
hasName = fmap isJust . getName

-- | Check for undefined symbols in expression
checkDef :: Expr -> ParserM Bool
checkDef (Application left right) = andM [hasName left, allA checkDef right]
checkDef (Ident name)             = hasName name
