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

import           Language.Syntax
import           Prover.Substitution
import           Prover.Types
import           Utils

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
intros asName =
    modify $ \st -> do
        let (pre, rest) = uncons (premises st) & fromMaybe (error "Trying to intros with no premises")
        st
            & _env %~ M.insert asName (Rule asName [] pre)
            & _premises .~ rest 

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
            e2 <- getDefinition <$> lookupSymbol j
            case r of
                Rule name args def ->
                    case args of
                        [] -> error "Not enough arguments to specialize"
                        (x:xs) -> do
                            let subst = mkSubst [(x, e2)]
                            pure $ Rule name xs (applySubst subst def)
                _ -> error "Symbol not a rule"
        _ -> error "Invalid specialize argument"

apply :: Expr -> String -> ProofM ()
apply = undefined

-- Tactic lookups the goal in assumptions
assumption :: ProofM ()
assumption = undefined

exact :: Name -> ProofM ()
exact name =
    exact' name >>= \case
        True -> _goal .=  (Ident "top")
        False -> do
            get
                >>= error 
                        . printf "exact: Could not match formula with goal\nProof state:\n%s"
                        . show

exact' :: Name -> ProofM Bool
exact' name = (==) <$> gets goal <*> (getDefinition <$> lookupSymbol name)

-------------
-- Helpers --
-------------

addRule :: Declaration -> Name -> ProofM ()
addRule (Rule name args e) asName = _env %= M.insert asName (Rule asName args e)

addToEnv :: Expr -> String -> ProofM ()
addToEnv expr asName = addRule (Rule asName [] expr) asName

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