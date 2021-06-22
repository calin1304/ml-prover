module Prover.Tactics where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bool
import           Data.Foldable
import           Text.Printf

import           Language.Syntax
import           Prover.ProofM
import           Prover.Substitution
import           Prover.Types
import           Utils

-- FIXME: Doesn't work with implication
intros :: Name -> ProofM ()
intros _ = undefined
    -- modify $ \st -> do
    --     let (pre, rest) = uncons (premises st) & fromMaybe (throwError "Trying to intros with no premises")
    --     st
    --         & _context %~ M.insert asName (Rule asName [] pre)
    --         & _premises .~ rest

{-
    Instantiate arguments of a rule.

    TODO: First argument is an expression but, ideally, it should just be
        an application of arguments to a name.
-}
specialize :: Expr -> Name -> ProofM ()
specialize expr asName = specialize' expr >>= (`addRule` asName)
    -- case expr of
    --     Application e1 e2 -> undefined
            -- (sdef, sargs) <- (getDefinition &&& getArgs) <$> lookupSymbol sym
            -- let s = mkSubst sargs args
            -- addToEnv (applySubst s sdef) asName
        -- _ -> throwError "Invalid specialize argument"

-- TODO: Make it work with nested applications, for example "mp P Q"
specialize' :: Expr -> ProofM Declaration
specialize' expr =
    case expr of
        Application (Ident i) (Ident j) -> do
            r <- lookupSymbol i
            e2 <- snd . getDefinition <$> lookupSymbol j -- TODO: Check that there are no hypotheses
            case r of
                Rule args hs c ->
                    case args of
                        [] -> throwError (TacticError "Not enough arguments to specialize")
                        (x:xs) -> do
                            let subst = mkSubst [(x, e2)]
                            let hs' = map (applySubst subst) hs
                            let c' = applySubst subst c
                            pure $ Rule xs hs' c'
                _ -> throwError (SymbolNotARule i)
        _ -> throwError (TacticError "Invalid specialize argument")

-- | @apply name subproofs@ tries to match @name@ with the current goal, and if that's the
-- case, then checks @subproofs@ which coressponds to proofs for all the hypotheses.
apply :: String -> [ProofM ()] -> ProofM ()
apply sym subproofs = do
    r <- lookupSymbol sym
    g <- use _goal
    case r of
        Rule [] hs c ->
            if c `matches` g
                then checkHypos (zip hs subproofs) >> setGoal "top" -- TODO: check we have as many proofs as hypotheses
                else throwError (TacticError "Goal doesn't match conclusion of applied formula")
        _             -> throwError (TacticError "Can't apply")
  where
    checkHypos :: [(Expr, ProofM ())] -> ProofM ()
    checkHypos = traverse_ (uncurry checkNew)

    checkNew :: Goal -> ProofM () -> ProofM ()
    checkNew newGoal proof = do
        st <- get
        case runProofM proof (st { goal = newGoal }) of
            Right _ -> pure ()
            Left e  -> throwError e

matches :: Expr -> Expr -> Bool
matches = (==)

-- check goal with consequence => proove all hypotheses

-- Tactic lookups the goal in assumptions
assumptions :: Name -> Name -> ProofM ()
assumptions name asName = do
    r <- lookupSymbol name
    allA isInCtx (hypothese r) >>= bool
        (throwError $ TacticError "Could not satisfy all hypos")
        (addRule (withoutHypotheses r) asName)
  where
    withoutHypotheses :: Declaration -> Declaration
    withoutHypotheses (Rule args _ c) = Rule args [] c
    withoutHypotheses _               = undefined

    hypothese :: Declaration -> [Expr]
    hypothese (Rule _ hs _) = hs
    hypothese _             = undefined

    isInCtx :: Expr -> ProofM Bool
    isInCtx e = uses _context ((e `elem`) . fmap (snd . getDefinition))
     -- ^ TODO: What to do with hypotheses from definition? Add test for that.

-- This could be written as apply on a formula without hypotheses
exact :: Name -> ProofM ()
exact name =
    exact' name >>= \case
        True -> _goal .= Ident "top"
        False ->
            throwError
                $ TacticError $ printf "exact: Could not match formula %s with goal" name

exact' :: Name -> ProofM Bool
exact' name =
    lookupSymbol name >>= \case
        Rule _ [] c -> uses _goal (== c)
        _             -> pure False
