module Test.Prover.Tactics
    ( tests
    ) where

import           Control.Lens           (at, (^.))
import           Data.Either            (isLeft)
import qualified Data.Map.Strict        as M (fromList, singleton)
import           Data.Maybe             (isJust)
import           Test.Tasty             (TestTree, testGroup)
import           Test.Tasty.HUnit       (assertBool, assertEqual, assertFailure,
                                         testCase, (@?=))
import           Test.Tasty.QuickCheck  ()

import           Interp                 (isTop)
import           Language.Lexer         ()
import           Language.Parser        ()
import           Language.ParserM       ()
import           Language.Syntax        (Declaration (Rule), Expr, Tactic,
                                         ( # ))
import           Prover.ProofM          (Context, ProofM, ProofState,
                                         ProverError, emptyProofState,
                                         mkProofState, newName, runProofM,
                                         _context, _goal)
import           Prover.Tactics
import           Prover.Types           (Goal)
import qualified Test.Prover.Tactics.PL as PL (tests)
import           Test.Utils

tests :: TestTree
tests = testGroup "Tactics"
    [ specializeTacticTests
    , exactTacticTests
    , applyTacticTests
    , newNameTests
    , PL.tests
    ]

specializeTacticTests :: TestTree
specializeTacticTests =
    testGroup "specialize tactic"
        [ testCase "partial specialization" partialSpecialization
        , testCase "total specialization" totalSpecialization
        , testCase "application expression" applicatioSpecialization
        ]
  where
    -- TODO: Test that it fails if rule to specialize is not in the context
    partialSpecialization = do
        let Right (_, st) =
                [ ("mp", Rule ["P", "Q"] ["P", "P" .-> "Q"] "Q")
                , ("X", [] .|- "X")
                ]
                |- "Y" $ specialize ("mp" # "X") "Hs"
        assertBool "specialize result is in environment" $ isJust $ st ^. _context . at "Hs"
        let Just (Rule args hs c) = st ^. _context . at "Hs"
        args @?= ["Q"]
        hs @?= ["X", "X" .-> "Q"]
        c @?= "Q"

        -- TODO: Add test that tries to specialize with something that has hypotheses

    totalSpecialization = do
        let Right (_, st) =
                [ ("mp", Rule ["P", "Q"] ["P", "P" .-> "Q"] "Q")
                , ("X", [] .|- "X")
                , ("Y", [] .|- "Y")
                , ("Hs", Rule ["Q"] ["X", "X" .-> "Q"] "Q")
                ] |- "Y" $ specialize ("Hs" # "Y") "Hss"
        assertBool "specialize result is in environment" $ isJust $ st ^. _context . at "Hss"
        let Just (Rule args hs c) = st ^. _context . at "Hss"
        args @?= []
        hs @?= ["X","X" .-> "Y"]
        c @?= "Y"

    applicatioSpecialization = do
        let Right (_, st) =
                [ ("r", Rule ["P"] ["P" .-> "P"] "P")
                , ("H", [] .|- ("X" .-> "Y"))
                ] |- "Y" $ specialize ("r" # "H") "Hss"
        assertBool "specialize result is in environment" $ isJust $ st ^. _context . at "Hss"
        let Just (Rule args hs c) = st ^. _context . at "Hss"
        args @?= []
        hs @?= [("X" .-> "Y") .-> ("X" .-> "Y")]
        c @?= "X" .->"Y"

exactTacticTests :: TestTree
exactTacticTests =
    testGroup "exact tactic"
        [ testCase "works as expected" exactTest
        ]
  where
    exactTest = do
        let Right (_, st) = [("H", [] .|- "Y")] |- "Y" $ exact "H"
        assertBool "goal is satisfied" $ isTop $ st ^. _goal

applyTacticTests :: TestTree
applyTacticTests =
    testGroup "apply tactic"
        [ testCase "works as exact if there are no hypotheses" noHypotheses
        , testCase "fails if goal doesn't match conclusion" noMatch
        , testCase "application goal is matched" applicationGoal
        ]
  where
    noHypotheses = do
        let result = [("H", [] .|- "P")] |- "P" $ apply "H" []
        case result of
            Left e -> assertFailure $ show e
            Right (_, st) -> assertBool "goal is satisfied" $ isTop $ st ^. _goal
    noMatch = do
        let result = [("H", [] .|- "Q")] |- "P" $ apply "H" []
        assertBool "fail because goal doesn't match conclusion of applied formula"
            $ isLeft result
    applicationGoal = do
        let result =
                [("H", [] .|- ("P" .-> ("P" .-> "Q")))] |- "P" .-> "Q" $ apply "H" []
        case result of
            Left e  -> assertFailure $ show e
            Right _ -> undefined

newNameTests :: TestTree
newNameTests = testGroup "generating new names"
    [ testCase "without prefix" noPrefixTest
    , testCase "with prefix" prefixTest
    ]
  where
    noPrefixTest = run Nothing @?= Right "_1"
    prefixTest = run (Just "H") @?= Right "_H1"

    run mprefix = fst <$> runProofM (newName Nothing >> newName mprefix) emptyProofState
