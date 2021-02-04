module Interp where

import           Control.Monad.State
import           Debug.Trace

import           Language.Syntax
import           Prover.ProofM       (ProofState (..), runProofM, step)
import           Utils               (debugSection)

type Env = [(String, Declaration)]

type InterpM = State Env

runInterpM :: InterpM a -> Env -> (a, Env)
runInterpM = runState

interp :: Declaration -> InterpM ()
interp decl =
    case decl of
        Rule name _ _ -> modify ((name, decl):)
        Lemma name args (FromDerive premises goal) tactics -> do
            env <- get
            let (result, proofState') =
                    runProofM
                        (traverse step tactics)
                        (ProofState goal premises env)
            seq (debugSection "Proof state" proofState') (pure ())
