module Prover.Tactics where

import           Language.Syntax

import           Control.Monad.State

import           Prover.ProofM
import           Prover.Types
import           Utils

intros :: Name -> ProofM ()
intros asName = do
    (pre, rest) <- gets (unsafeSplitHead . premises) --TODO: Use total head
    modify $ \st ->
        let env' = (asName, pre) : env st
         in st { env = env', premises = rest }

-- g = (EVar "P" -># EVar "Q") -># (EVar "Q" -># EVar "R")
-- pre = [EVar "P" -># (EVar "Q" -># EVar "R")]

-- test = runState (intros "H") (ProofState g pre emptyProofEnv)

-- (#) :: String -> [SimpleExpr] -> SimpleExpr
-- x # xs = Application x xs

-- (->#) :: SimpleExpr -> SimpleExpr -> SimpleExpr
-- x -># y = Application "impl" [x, y]
