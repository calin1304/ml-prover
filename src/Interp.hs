module Interp where

import           Control.Monad.State
import Control.Monad (unless)
import           Debug.Trace
import Data.Map (Map (..))
import Control.Lens ((^.))
import qualified Data.Map.Strict as M (fromList, union, insert)
import Control.Monad.Except

import           Language.Syntax
import           Prover.ProofM       (ProofState (..), runProofM, step, _goal)
import           Utils               (debugSection)

type Env = Map String Declaration

type InterpError = String
type InterpM = ExceptT InterpError (State Env)

runInterpM :: InterpM a -> Env -> (Either InterpError a, Env)
runInterpM = runState . runExceptT

interp :: Declaration -> InterpM ()
interp decl =
    case decl of
        Rule name _ _ _ -> addDeclaration name decl
        Lemma name args hs c tactics -> undefined -- do
            -- env <- get
            -- let
            --     introArgs = M.fromList $ map (\x -> (x, Rule x [] (Ident x))) args
            --     (result, proofState') =
            --         runProofM
            --             (traverse step tactics)
            --             (ProofState goal premises (M.union introArgs env))
            -- seq (debugSection "Proof state" proofState')
            --     $ unless (isTop $ proofState' ^. _goal) (throwError "Could not prove top")
        _ -> throwError "Invalid declaration"

addDeclaration :: String -> Declaration -> InterpM ()
addDeclaration name decl = modify (M.insert name decl)

isTop :: Expr -> Bool
isTop (Ident "top") = True
isTop _ = False