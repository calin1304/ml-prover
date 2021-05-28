module Interp
    ( InterpError
    , Env
    , runInterpM
    , interp
    , isTop
    ) where

import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import           Control.Monad.State  (State, modify, runState)
import           Data.Map             (Map)
import qualified Data.Map.Strict      as M (insert)

import           Language.Syntax      (Declaration (Lemma, Rule), Expr (Ident))

type Env = Map String Declaration

type InterpError = String
type InterpM = ExceptT InterpError (State Env)

runInterpM :: InterpM a -> Env -> (Either InterpError a, Env)
runInterpM = runState . runExceptT

interp :: Declaration -> InterpM ()
interp decl =
    case decl of
        Rule name _ _ _ -> addDeclaration name decl
        Lemma _ _ _ _ _ -> undefined -- do
            -- env <- get
            -- let
            --     introArgs = M.fromList $ map (\x -> (x, Rule x [] (Ident x))) args
            --     (result, proofState') =
            --         runProofM
            --             (traverse step tactics)
            --             (ProofState goal premises (M.union introArgs env))
            -- seq (debugSection "Proof state" proofState')
            --     $ unless (isTop $ proofState' ^. _goal) (throwError "Could not prove top")
        _               -> throwError "Invalid declaration"

addDeclaration :: String -> Declaration -> InterpM ()
addDeclaration name decl = modify (M.insert name decl)

isTop :: Expr -> Bool
isTop (Ident "top") = True
isTop _             = False
