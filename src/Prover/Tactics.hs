module Prover.Tactics where

import           Control.Lens                 (use, uses)
import           Control.Monad.State
import           Data.Generics.Product.Fields (field)

import           Language.Syntax
import           Prover.ProofM
import           Prover.Substitution          (applySubst, mkSubst)
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
    goal :: ProofM Expr
    goal = use (field @"goal")

    form :: ProofM Expr
    form = uses (field @"env") (unsafeLookup name)

apply = undefined

specialize :: Tactic -> ProofM ()
specialize (Specialize e asName) =
    case e of
        Application sym args -> do
            symDef <- undefined -- get actual symbol definition
            defArgs <- getSymDefArgs symDef -- get sym arg names
            let s = mkSubst defArgs args -- make substitution for arguments
            addToEnv (asName, (applySubst s e))
          where
            getSymDefArgs = undefined
        _ -> undefined

-- g = (EVar "P" -># EVar "Q") -># (EVar "Q" -># EVar "R")
-- pre = [EVar "P" -># (EVar "Q" -># EVar "R")]

-- test = runState (intros "H") (ProofState g pre emptyProofEnv)

-- (#) :: String -> [Expr] -> Expr
-- x # xs = Application x xs

-- (->#) :: Expr -> Expr -> Expr
-- x -># y = Application "impl" [x, y]
