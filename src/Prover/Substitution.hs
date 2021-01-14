module Prover.Substitution where

import Data.Maybe (fromJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Language.Syntax

newtype Subst = Subst (Map String SimpleExpr)

applySubst :: Subst -> SimpleExpr -> SimpleExpr
applySubst (Subst s) = \case
    EVar name -> fromJust $ M.lookup name s
    SVar name -> fromJust $ M.lookup name s
    Application sym args -> Application sym (map (applySubst (Subst s)) args)
