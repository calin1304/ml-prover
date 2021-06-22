module Test.Prover.Tactics
    ( tests
    ) where

import           Control.Lens          (at, (^.))
import           Data.Either           (isLeft)
import qualified Data.Map.Strict       as M (fromList, singleton)
import           Data.Maybe            (isJust)
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (assertBool, assertEqual, assertFailure,
                                        testCase, (@?=))
import           Test.Tasty.QuickCheck ()

import           Interp                (isTop)
import           Language.Lexer        ()
import           Language.Parser       ()
import           Language.ParserM      ()
import           Language.Syntax       (Declaration (Rule), Expr, Tactic,
                                        ( ## ))
import           Prover.ProofM         (Context, ProofM, ProofState,
                                        ProverError, emptyProofState,
                                        mkProofState, newName, runProofM,
                                        _context, _goal)
import           Prover.Tactics
import           Prover.Types          (Goal)

tests :: TestTree
tests = testGroup "Prover"
    [ specializeTacticTests
    , exactTacticTests
    , applyTacticTests
    , proofTests
    , newNameTests
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
                , ("X", Rule [] [] "X")
                ]
                |- "Y" $ specialize ("mp" ## "X") "Hs"
        assertBool "specialize result is in environment" $ isJust $ st ^. _context . at "Hs"
        let Just (Rule args hs c) = st ^. _context . at "Hs"
        args @?= ["Q"]
        hs @?= ["X", "X" .-> "Q"]
        c @?= "Q"

        -- TODO: Add test that tries to specialize with something that has hypotheses

    totalSpecialization = do
        let Right (_, st) =
                [ ("mp", Rule ["P", "Q"] ["P", "P" .-> "Q"] "Q")
                , ("X", Rule [] [] "X")
                , ("Y", Rule [] [] "Y")
                , ("Hs", Rule ["Q"] ["X", "X" .-> "Q"] "Q")
                ] |- "Y" $ specialize ("Hs" ## "Y") "Hss"
        assertBool "specialize result is in environment" $ isJust $ st ^. _context . at "Hss"
        let Just (Rule args hs c) = st ^. _context . at "Hss"
        args @?= []
        hs @?= ["X","X" .-> "Y"]
        c @?= "Y"

    applicatioSpecialization = do
        let Right (_, st) =
                [ ("r", Rule ["P"] ["P" .-> "P"] "P")
                , ("H", Rule [] [] ("X" .-> "Y"))
                ] |- "Y" $ specialize ("r" ## "H") "Hss"
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
        let Right (_, st) = [("H", Rule [] [] "Y")] |- "Y" $ exact "H"
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
        let result = [("H", Rule [] []  "P")] |- "P" $ apply "H" []
        case result of
            Left e -> assertFailure $ show e
            Right (_, st) -> assertBool "goal is satisfied" $ isTop $ st ^. _goal
    noMatch = do
        let result = [("H", Rule [] [] "Q")] |- "P" $ apply "H" []
        assertBool "fail because goal doesn't match conclusion of applied formula"
            $ isLeft result
    applicationGoal = do
        let result =
                [("H", Rule [] [] ("P" .-> ("P" .-> "Q")))] |- "P" .-> "Q" $ apply "H" []
        case result of
            Left e  -> assertFailure $ show e
            Right _ -> undefined

proofTests :: TestTree
proofTests =
    testGroup "proofs"
        [ testCase "simple proof" proofTest
        , testCase "mp2" proof_mp2
        , testCase "mpd" proof_mpd
        ]
  where
    proofTest = do
        let proof = do
                specialize ("mp" ## "X") "H1"
                specialize ("H1" ## "Y") "H2"
                -- apply "Hss"
                --     [ [ exact "X"]
                --     , [ exact "H"]
                --     ]
                assumptions "H2" "H3"
                exact "H3"
        let Right (_, st) =
                [ ("mp", Rule ["P", "Q"] ["P", "P" .-> "Q"] "Q")
                , ("X", Rule [] [] "X")
                , ("Y", Rule [] [] "Y")
                , ("H", Rule [] [] ("X" .-> "Y"))
                ]
                |- "Y" $ proof
        assertBool "goal is satisfied" $ isTop $ st ^. _goal

    proof_mp2 = do
        let proof = do
                apply "X1"
                    [ apply "X"  []
                    , apply "X0" []
                    ]
        let Right (_, st) =
                [ ("X", Rule [] [] "P")
                , ("X0", Rule [] [] "Q")
                , ("X1", Rule [] ["P", "Q"] "R")
                ]
                |- "R" $ proof
        assertEqual "goal is top" "top" (st ^. _goal)

    proof_mpd = do
        let proof = do
                apply "X0"
                    [ apply "X1" []
                    , apply "X"
                        [ apply "X1" [] -- FIXME: This should fail but it doesn't
                        ]
                    ]
        let Right (_, st) =
                [ ("X", Rule [] ["P"] "Q")
                , ("X0", Rule [] ["P", "Q"] "R")
                , ("X1", Rule [] [] "P")
                ]
                |- "R" $ proof
        assertEqual "goal is top" "top" (st ^. _goal)

newNameTests :: TestTree
newNameTests = testGroup "generating new names"
    [ testCase "without prefix" noPrefixTest
    , testCase "with prefix" prefixTest
    ]
  where
    noPrefixTest = run Nothing @?= Right "_1"
    prefixTest = run (Just "H") @?= Right "_H1"

    run mprefix = fst <$> runProofM (newName Nothing >> newName mprefix) emptyProofState

-----------
-- Utils --
-----------

singleRule :: Goal -> String -> Declaration -> ProofState
singleRule g k v = mkProofState g $ M.singleton k v

infix 5 .->

(.->) :: Expr -> Expr -> Expr
(.->) p q = "impl" ## p ## q

infix 4 |-

(|-) :: [(String, Declaration)] -> Goal -> ProofM () -> Either ProverError ((), ProofState)
(|-) c g p = runProofM p (mkProofState g (M.fromList c))
