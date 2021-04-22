module Interp where

import           Control.Monad.State
import           Debug.Trace
import Data.Map (Map (..))
import qualified Data.Map.Strict as M (fromList, union, insert)

import           Language.Syntax
import           Prover.ProofM       (ProofState (..), runProofM, step)
import           Utils               (debugSection)

type Env = Map String Declaration

type InterpM = State Env

runInterpM :: InterpM a -> Env -> (a, Env)
runInterpM = runState

interp :: Declaration -> InterpM ()
interp decl =
    case decl of
        Rule name _ _ -> modify (M.insert name decl)
        Lemma name args (FromDerive premises goal) tactics -> do
            env <- get
            let
                introArgs = M.fromList $ map (\x -> (x, Rule x [] (Ident x))) args
                (result, proofState') =
                    runProofM
                        (traverse step tactics)
                        (ProofState goal premises (M.union introArgs env))
            seq (debugSection "Proof state" proofState') (pure ())
