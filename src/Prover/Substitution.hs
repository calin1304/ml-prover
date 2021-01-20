module Prover.Substitution where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromJust)

import           Language.Syntax

newtype Subst = Subst (Map String Expr)

mkSubst :: [String] -> [Expr] -> Subst
mkSubst ns es = Subst . M.fromList $ zip ns es

applySubst :: Subst -> Expr -> Expr
applySubst (Subst s) = \case
    EVar name -> fromJust $ M.lookup name s
    SVar name -> fromJust $ M.lookup name s
    Application sym args -> Application sym (map (applySubst (Subst s)) args)
