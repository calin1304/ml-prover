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
intros asName = undefined
    -- do
    -- (pre, rest) <- gets (unsafeSplitHead . premises) --TODO: Use total head
    -- modify $ \st ->
    --     let env' = (asName, pre) : env st
    --      in st { env = env', premises = rest }

-- exact :: Name -> ProofM Bool
-- exact name = (==) <$> goal <*> form
--   where
--     goal :: ProofM Expr
--     goal = use (field @"goal")

--     form :: ProofM Expr
--     form = uses (field @"env") (unsafeLookup name)

-- apply = undefined

specialize :: Tactic -> ProofM ()
specialize (Specialize e asName) =
    case e of
        Application symName args -> do
            sym <- unsafeLookupSymbol symName
            let symDef = getDefinition sym
            let symArgs = getArgs sym
            let s = mkSubst symArgs args
            addToEnv (asName, Rule asName [] (applySubst s symDef))
          where
            getSymDefArgs = undefined

            unsafeLookupSymbol :: String -> ProofM Declaration
            unsafeLookupSymbol name = do
                e <- gets env
                pure $ unsafeLookup name e
        _ -> undefined

-- g = (EVar "P" -># EVar "Q") -># (EVar "Q" -># EVar "R")
-- pre = [EVar "P" -># (EVar "Q" -># EVar "R")]

-- test = runState (intros "H") (ProofState g pre emptyProofEnv)

-- (#) :: String -> [Expr] -> Expr
-- x # xs = Application x xs

-- (->#) :: Expr -> Expr -> Expr
-- x -># y = Application "impl" [x, y]
