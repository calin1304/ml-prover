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
addDefinition :: Declaration -> ParserM ()
addDefinition expr = undefined
    -- case expr of
    --     Lemma {} -> undefined
    --     Import _ -> undefined
    --     Notation name _ def _ -> do
    --         -- Check if we have references to unknown symbols in definition
    --         whenM (checkDef def) (error $ printf "%s not defined" name)
    --         modify $ \env -> env { names = (name, expr) : names env }
    --     Rule name _ _ _ ->
    --         modify $ \env -> env { names = (name, expr) : names env }
    --     _ -> undefined

getName :: String -> ParserM (Maybe Declaration)
getName name = gets (lookup name . names)

hasName :: String -> ParserM Bool
hasName = fmap isJust . getName

-- | Check for undefined symbols in expression
checkDef :: Expr -> ParserM Bool
checkDef (Application left right) = andM [hasName left, allA checkDef right]
checkDef (EVar name)              = hasName name
checkDef (SVar name)              = hasName name
