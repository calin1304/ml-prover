module Prover.ProofM where

import           Control.Arrow                ((&&&))
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bool                    (bool)
import           Data.Foldable                (traverse_)
import           Data.Function                ((&))
import           Data.Functor                 ((<&>))
import           Data.Generics.Product.Fields (field)
import           Data.List                    (intercalate)
import           Data.Map                     (Map (..))
import qualified Data.Map                     as M (insert, lookup, toList)
import           Data.Maybe                   (fromMaybe)
import           GHC.Generics                 (Generic)
import           Test.QuickCheck              (Arbitrary, arbitrary)
import           Text.PrettyPrint             (($+$))
import qualified Text.PrettyPrint             as PP
import           Text.Printf                  (printf)

import           Language.Syntax
import           Pretty
import           Prover.Substitution
import           Prover.Types
import           Utils

type ProverError = String
type ProofM = StateT ProofState (Except ProverError)

type ProofEnv = Map String Declaration

data ProofState = ProofState
    { goal     :: Goal
    , premises :: Premises
    , env      :: ProofEnv
    }
    deriving (Eq, Generic, Show)

instance Pretty ProofState where
    pretty (ProofState goal premises env) =
        PP.text "Env" $+$ PP.nest 4 docEnv $+$ PP.text "Goal" $+$ PP.nest 4 docGoal
      where
        docEnv = PP.vcat . map (PP.text . show . snd) $ M.toList env
        docGoal = pretty goal

------------
-- Lenses --
------------

_goal :: Lens' ProofState Goal
_goal = field @"goal"

_premises :: Lens' ProofState Premises
_premises = field @"premises"

_env :: Lens' ProofState ProofEnv
_env = field @"env"

---

runProofM :: ProofM a -> ProofState -> Either ProverError (a, ProofState)
runProofM m st = runExcept $ runStateT m st

step :: Tactic -> ProofM ()
step = \case
    Intros asName -> intros asName
    Specialize expr asName -> specialize expr asName
    -- Apply expr asName -> apply expr asName
    Exact name -> exact name

-------------
-- Tactics --
-------------

-- FIXME: Doesn't work with implication
intros :: Name -> ProofM ()
intros asName = undefined
    -- modify $ \st -> do
    --     let (pre, rest) = uncons (premises st) & fromMaybe (throwError "Trying to intros with no premises")
    --     st
    --         & _env %~ M.insert asName (Rule asName [] pre)
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
                Rule name args hs c ->
                    case args of
                        [] -> throwError "Not enough arguments to specialize"
                        (x:xs) -> do
                            let subst = mkSubst [(x, e2)]
                            let hs' = map (applySubst subst) hs
                            let c' = applySubst subst c
                            pure $ Rule name xs hs' c'
                _ -> throwError "Symbol not a rule"
        _ -> throwError "Invalid specialize argument"

-- | @apply name subproofs@ tries to match @name@ with the current goal, and if that's the
-- case, then checks @subproofs@ which coressponds to proofs for all the hypotheses.
apply :: String -> [ProofM ()] -> ProofM ()
apply sym subproofs = do
    r <- lookupSymbol sym
    goal <- use _goal
    case r of
        Rule _ [] hs c ->
            if c `matches` goal
                then checkHypos (zip hs subproofs) >> setGoal "top" -- TODO: check we have as many proofs as hypotheses
                else throwError "Goal doesn't match conclusion of applied formula"
        _             -> throwError "Can't apply"
  where
    checkHypos :: [(Expr, ProofM ())] -> ProofM ()
    checkHypos = traverse_ (uncurry checkNew)

    checkNew :: Goal -> ProofM () -> ProofM ()
    checkNew newGoal proof = do
        st <- get
        case runProofM proof (st { goal = newGoal }) of
            Right _ -> pure ()
            Left e  -> throwError e

matches = (==)

-- check goal with consequence => proove all hypotheses

-- Tactic lookups the goal in assumptions
assumptions :: Name -> Name -> ProofM ()
assumptions name asName = do
    r <- lookupSymbol name
    allA isInCtx (hypothese r) >>= bool
        (throwError "Could not satisfy all hypos")
        (addRule (withoutHypotheses r) asName)
  where
    withoutHypotheses :: Declaration -> Declaration
    withoutHypotheses (Rule name args hs c) = Rule name args [] c

    hypothese :: Declaration -> [Expr]
    hypothese (Rule _ _ hs _) = hs

    isInCtx :: Expr -> ProofM Bool
    isInCtx e = uses _env ((e `elem`) . fmap (snd . getDefinition))
     -- ^ TODO: What to do with hypotheses from definition? Add test for that.

-- This could be written as apply on a formula without hypotheses
exact :: Name -> ProofM ()
exact name =
    exact' name >>= \case
        True -> _goal .= Ident "top"
        False -> do
            get
                >>= throwError
                        . printf "exact: Could not match formula %s with goal\nProof state:\n%s" name
                        . show

exact' :: Name -> ProofM Bool
exact' name =
    lookupSymbol name >>= \case
        Rule _ _ [] c -> uses _goal (== c)
        _             -> pure False

-------------
-- Helpers --
-------------

addRule :: Declaration -> Name -> ProofM ()
addRule (Rule name args hs c) asName = _env %= M.insert asName (Rule asName args hs c)

addToEnv :: [Expr] -> Expr -> String -> ProofM ()
addToEnv hs c asName = addRule (Rule asName [] hs c) asName

lookupSymbol :: Name -> ProofM Declaration
lookupSymbol name = do
    use (_env . at name)
        >>= maybe (throwError $ printf "Symbol %s not found\n." name) pure

setGoal :: Goal -> ProofM ()
setGoal = assign _goal
