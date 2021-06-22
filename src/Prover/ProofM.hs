module Prover.ProofM
    ( ProofM
    , ProverError (..)
    , ProofState (..)
    , mkProofState
    , emptyProofState
    , runProofM
    , _goal
    , _context
    , newName
    , lookupSymbol
    , addRule
    , setGoal
    ) where

import           Control.Lens                 (Lens', assign, at, modifying,
                                               use, uses, (%=), (.=))
import           Control.Monad.Except         (Except, runExcept, throwError)
import           Control.Monad.State          (StateT, get, runStateT)
import           Data.Bool                    (bool)
import           Data.Foldable                (traverse_)
import           Data.Generics.Product.Fields (field)
import           Data.Map                     (Map)
import qualified Data.Map                     as M (insert, toList)
import           Data.Maybe                   (fromMaybe)
import           GHC.Generics                 (Generic)
import           Text.PrettyPrint             (($+$))
import qualified Text.PrettyPrint             as PP
import           Text.Printf                  (printf)

import           Language.Syntax              (Declaration (Rule),
                                               Expr (Application, Ident),
                                               Tactic (Apply, Exact, Intros, Specialize),
                                               getDefinition)
import           Pretty                       (Pretty, pretty)
import           Prover.Substitution          (applySubst, mkSubst)
import           Prover.Types                 (Goal, Name)
import           Utils                        (allA)


type ProofM = StateT ProofState (Except ProverError)

type Context = Map String Declaration

data ProverError =
    TacticError String
  | SymbolNotFound String
  | SymbolNotARule String
    deriving (Eq, Show)

data ProofState = ProofState
    { goal    :: Goal
    , context :: Context
    , counter :: Int
    }
    deriving (Eq, Generic, Show)

mkProofState :: Goal -> Context -> ProofState
mkProofState goal context = ProofState
    { goal = goal
    , context = context
    , counter = 0
    }

emptyProofState :: ProofState
emptyProofState = ProofState (Ident "bottom") mempty 0

instance Pretty ProofState where
    pretty (ProofState { goal, context }) =
        PP.text "Env" $+$ PP.nest 4 docEnv $+$ PP.text "Goal" $+$ PP.nest 4 docGoal
      where
        docEnv = PP.vcat . map (PP.text . show . snd) $ M.toList context
        docGoal = pretty goal

------------
-- Lenses --
------------

_goal :: Lens' ProofState Goal
_goal = field @"goal"

_context :: Lens' ProofState Context
_context = field @"context"

_counter :: Lens' ProofState Int
_counter = field @"counter"

---

runProofM :: ProofM a -> ProofState -> Either ProverError (a, ProofState)
runProofM m st = runExcept $ runStateT m st

-------------
-- Helpers --
-------------

-- | Add new rule with given name to context.
addRule :: Declaration -> Name -> ProofM ()
addRule (Rule args hs c) asName = _context %= M.insert asName (Rule args hs c)
addRule _ _                     = undefined

-- | Lookup declaration by name in current context.
lookupSymbol :: Name -> ProofM Declaration
lookupSymbol name = use (_context . at name) >>= maybe (throwError $ SymbolNotFound name) pure

-- | @setGoal expr@ sets goal to given expression.
setGoal :: Goal -> ProofM ()
setGoal = assign _goal

-- | @newName mx@ generates a new name with possible prefix @mx@.
newName :: Maybe Name -> ProofM Name
newName mprefix = (\i -> mconcat ["_", fromMaybe "" mprefix, show i]) <$> getNextCounter

getNextCounter :: ProofM Int
getNextCounter = use _counter <* modifying _counter (+1)
