module Prover.ProofM where

import Data.List (intercalate)
import           Control.Arrow                ((&&&))
import           Control.Lens
import           Control.Monad.State
import           Data.Function                ((&))
import Data.Functor ((<&>))
import           Data.Generics.Product.Fields (field)
import           Data.Maybe                   (fromMaybe)
import           GHC.Generics                 (Generic)
import           Test.QuickCheck              (Arbitrary, arbitrary)
import           Text.Printf                  (printf)
import Data.Map (Map (..))
import qualified Data.Map as M (insert, lookup, toList)
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint (($+$))
import Data.Bool (bool)

import           Language.Syntax
import           Prover.Substitution
import           Prover.Types
import           Utils

-- TODO: Add ExceptT for failure signaling
type ProofM a = State ProofState a

type ProofEnv = Map String Declaration

data ProofState = ProofState
    { goal     :: Goal
    , premises :: Premises
    , env      :: ProofEnv
    }
    deriving (Eq, Generic)

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

instance Show ProofState where
    show = PP.render . docProofState

runProofM :: ProofM a -> ProofState -> (a, ProofState)
runProofM = runState

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
    --     let (pre, rest) = uncons (premises st) & fromMaybe (error "Trying to intros with no premises")
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
        -- _ -> error "Invalid specialize argument"

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
                        [] -> error "Not enough arguments to specialize"
                        (x:xs) -> do
                            let subst = mkSubst [(x, e2)]
                            let hs' = map (applySubst subst) hs
                            let c' = applySubst subst c
                            pure $ Rule name xs hs' c'
                _ -> error "Symbol not a rule"
        _ -> error "Invalid specialize argument"

-- | @apply name subproofs@ tries to match @name@ with the current goal, and if that's the
-- case, then checks @subproofs@ which coressponds to proofs for all the hypotheses.
apply :: String -> [[ProofM ()]] -> ProofM ()
apply sym subproofs = do
    r <- lookupSymbol sym
    goal <- use _goal
    case r of
        Rule _ [] _ e -> when (consequence e `matches` goal) proveHypos
        _ -> error "Can't apply"
  where
    consequence = undefined -- \case
        -- FromDerive hs c -> c
        -- e -> error $ printf "Invalid expression %s" (show e)
    proveHypos = undefined

matches = (==)

-- check goal with consequence => proove all hypotheses

-- Tactic lookups the goal in assumptions
assumptions :: Name -> Name -> ProofM ()
assumptions name asName = do
    r <- lookupSymbol name
    allA isInCtx (hypothese r) >>= bool
        (error "Could not satisfy all hypos")
        (addRule (withoutHypotheses r) asName)
  where
    withoutHypotheses :: Declaration -> Declaration
    withoutHypotheses (Rule name args hs c) = Rule name args [] c

    hypothese :: Declaration -> [Expr]
    hypothese (Rule _ _ hs _) = hs

    isInCtx :: Expr -> ProofM Bool
    isInCtx e = uses _env ((e `elem`) . fmap (snd . getDefinition))
     -- ^ TODO: What to do with hypotheses from definition? Add test for that.

exact :: Name -> ProofM ()
exact name =
    exact' name >>= \case
        True -> _goal .=  (Ident "top")
        False -> do
            get
                >>= error 
                        . printf "exact: Could not match formula %s with goal\nProof state:\n%s" name
                        . show

exact' :: Name -> ProofM Bool
exact' name = do
    goal <- use _goal
    r <- lookupSymbol name
    case r of
        Rule _ _ [] c -> pure $ goal == c
        _ -> pure False

-------------
-- Helpers --
-------------

addRule :: Declaration -> Name -> ProofM ()
addRule (Rule name args hs c) asName = _env %= M.insert asName (Rule asName args hs c)

addToEnv :: [Expr] -> Expr -> String -> ProofM ()
addToEnv hs c asName = addRule (Rule asName [] hs c) asName

lookupSymbol :: Name -> ProofM Declaration
lookupSymbol name = 
    use (_env . at name)
        <&> fromMaybe (error $ printf "Symbol %s not found\n." name)

---------------------
-- Pretty printing --
---------------------

docProofState (ProofState goal premises env) =
    PP.text "Env" $+$ PP.nest 4 docEnv $+$ PP.text "Goal" $+$ PP.nest 4 docGoal
  where
    docEnv = PP.vcat . map (PP.text . show . snd) $ M.toList env
    docGoal = docExpr goal