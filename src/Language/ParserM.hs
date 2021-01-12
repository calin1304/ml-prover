module Language.ParserM where

import           Control.Monad       (unless)
import           Control.Monad.State
import           Text.Printf         (printf)

import           Language.Syntax

type ParserM a = State Env a

newtype Env = Env
    { names :: [(String, Expr)]
    }
    deriving (Show)

emptyEnv :: Env
emptyEnv = Env
    { names = []
    }

addNotation :: Expr -> ParserM ()
addNotation expr =
    case expr of
        Lemma {} -> undefined
        Import _ -> undefined
        Notation name _ def _ -> do
            whenM (checkDef def) (error $ printf "%s not defined" name)
            modify $ \env -> env { names = (name, expr) : names env }
        Rule name _ _ _ ->
            modify $ \env -> env { names = (name, expr) : names env }
        _ -> undefined

getName :: String -> ParserM (Maybe Expr)
getName name = gets (lookup name . names)

hasName :: String -> ParserM Bool
hasName = fmap isJust . getName

checkDef :: SimpleExpr -> ParserM Bool
checkDef (Application left right) = hasName left `andM` allA checkDef right
checkDef (EVar name)              = hasName name
checkDef (SVar name)              = hasName name

isJust (Just _) = True
isJust _        = False

whenM mb act = mb >>= \b -> if b then act else pure ()

andM ma mb = ma >>= \b -> if b then mb else pure False

allA :: (Applicative f, Traversable t) => (a -> f Bool) -> t a -> f Bool
allA p = fmap and . traverse p
