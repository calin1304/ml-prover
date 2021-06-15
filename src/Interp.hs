module Interp
    ( InterpError
    , Env
    , runInterpM
    , interp
    , isTop
    ) where

import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import           Control.Monad.State  (State, runState)
import           Data.Map             (Map)

import           Language.Syntax      (Declaration (Lemma, Rule), Expr (Ident))

type Env = Map String Declaration

type InterpError = String
type InterpM = ExceptT InterpError (State Env)

runInterpM :: InterpM a -> Env -> (Either InterpError a, Env)
runInterpM = runState . runExceptT

interp :: Declaration -> InterpM ()
interp decl =
    case decl of
        Rule _ _ _ -> undefined -- addDeclaration name decl
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

isTop :: Expr -> Bool
isTop (Ident "top") = True
isTop _             = False
