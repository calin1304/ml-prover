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

exact :: Name -> ProofM Bool
exact name = undefined -- (==) <$> goal <*> form
--   where
--     goal :: ProofM Expr
--     goal = use (field @"goal")

--     form :: ProofM Expr
--     form = uses (field @"env") (getDefinition . unsafeLookup name)
