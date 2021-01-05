module Prover.Tactics where

import Parser.Syntax

import Prover.ProofM
import Prover.State

intros = undefined

-- intros :: Name -> ProofM ()
-- intros asName = do
--     (pre, rest) <- gets (splitAt 1 . premises) --TODO: Use total head
--     modify $ \st ->
--         let env' = (asName, head pre) : env st
--          in st { env = env', premises = rest }

-- g = (EVar "P" -># EVar "Q") -># (EVar "Q" -># EVar "R")
-- pre = [EVar "P" -># (EVar "Q" -># EVar "R")]

-- test = runState (intros "H") (ProofState g pre emptyProofEnv)

-- (#) :: String -> [SimpleExpr] -> SimpleExpr
-- x # xs = Application x xs

-- (->#) :: SimpleExpr -> SimpleExpr -> SimpleExpr
-- x -># y = Application "impl" [x, y]