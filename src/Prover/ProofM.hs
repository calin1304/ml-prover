module Prover.ProofM where

import Control.Monad.State

import Prover.State
import Prover.Types

type ProofM a = State ProofState a

runProof :: Tactics -> Premises -> Goal -> Bool
runProof tactics premises goal =
    evalState
        (prove tactics)
        (ProofState goal premises emptyProofEnv)

prove :: Tactics -> ProofM Bool
prove = undefined