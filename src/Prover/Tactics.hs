module Prover.Tactics where

import           Control.Lens                 (use, uses)
import           Control.Monad.State
import           Data.Generics.Product.Fields (field)

import           Language.Syntax
import           Prover.ProofM
import           Prover.Types
import           Utils

intros :: Name -> ProofM ()
intros asName = do
    (pre, rest) <- gets (unsafeSplitHead . premises) --TODO: Use total head
    modify $ \st ->
        let env' = (asName, pre) : env st
         in st { env = env', premises = rest }

exact :: Name -> ProofM Bool
exact name = (==) <$> goal <*> form
  where
    goal :: ProofM SimpleExpr
    goal = use (field @"goal")

    form :: ProofM SimpleExpr
    form = uses (field @"env") (unsafeLookup name)

-- g = (EVar "P" -># EVar "Q") -># (EVar "Q" -># EVar "R")
-- pre = [EVar "P" -># (EVar "Q" -># EVar "R")]

-- test = runState (intros "H") (ProofState g pre emptyProofEnv)

-- (#) :: String -> [SimpleExpr] -> SimpleExpr
-- x # xs = Application x xs

-- (->#) :: SimpleExpr -> SimpleExpr -> SimpleExpr
-- x -># y = Application "impl" [x, y]
