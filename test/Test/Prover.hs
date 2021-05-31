module Test.Prover
    ( tests
    ) where

import           Control.Lens          (at, (^.))
import           Data.Either           (isLeft)
import qualified Data.Map.Strict       as M (singleton, fromList)
import           Data.Maybe            (isJust)
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (assertBool, assertEqual, assertFailure,
                                        testCase, (@?=))
import           Test.Tasty.QuickCheck ()

import           Interp                (isTop)
import           Language.Lexer        ()
import           Language.Parser       ()
import           Language.ParserM      ()
import           Language.Syntax       (Declaration (Rule), ( ## ))
import Prover.Types (Goal)
import           Prover.ProofM         (ProofState (ProofState), apply,
                                        assumptions, env, exact, goal,
                                        runProofM, specialize, _env, _goal)

tests :: TestTree
tests = testGroup "Prover"
    [ specializeTacticTests
    , exactTacticTests
    , applyTacticTests
    , proofTests
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
                runProofM
                    (specialize ("mp" ## "X") "Hs")
                    (ProofState
                        { goal = "Y"
                        , env =
                            M.fromList
                                [ ("mp", Rule ["P", "Q"] ["P", "impl" ## "P" ## "Q"] "Q")
                                , ("X", Rule [] [] "X")
                                ]
                        }
                    )
        assertBool "specialize result is in environment" $ isJust $ st ^. _env . at "Hs"
        let Just (Rule args hs c) = st ^. _env . at "Hs"
        args @?= ["Q"]
        hs @?= ["X", "impl" ## "X" ## "Q"]
        c @?= "Q"

        -- TODO: Add test that tries to specialize with something that has hypotheses

    totalSpecialization = do
        let Right (_, st) =
                runProofM
                    (specialize ("Hs" ## "Y") "Hss")
                    (ProofState
                        { goal = "Y"
                        , env =
                            M.fromList
                                [ ("mp", Rule ["P", "Q"] ["P", "impl" ## "P" ## "Q"] "Q")
                                , ("X", Rule [] [] "X")
                                , ("Y", Rule [] [] "Y")
                                , ("Hs", Rule ["Q"] ["X", "impl" ## "X" ## "Q"] "Q")
                                ]
                        }
                    )
        assertBool "specialize result is in environment" $ isJust $ st ^. _env . at "Hss"
        let Just (Rule args hs c) = st ^. _env . at "Hss"
        args @?= []
        hs @?= ["X", "impl" ## "X" ## "Y"]
        c @?= "Y"

    applicatioSpecialization = do
        let Right (_, st) =
                runProofM
                    (specialize ("r" ## "H") "Hss")
                    (ProofState
                        { goal = "Y"
                        , env =
                            M.fromList
                                [ ("r", Rule ["P"] ["impl" ## "P" ## "P"] "P")
                                , ("H", Rule [] [] ("impl" ## "X" ## "Y"))
                                ]
                        }
                    )
        assertBool "specialize result is in environment" $ isJust $ st ^. _env . at "Hss"
        let Just (Rule args hs c) = st ^. _env . at "Hss"
        args @?= []
        hs @?= ["impl" ## ("impl" ## "X" ## "Y") ## ("impl" ## "X" ## "Y")]
        c @?= "impl" ## "X" ## "Y"

exactTacticTests :: TestTree
exactTacticTests =
    testGroup "exact tactic"
        [ testCase "works as expected" exactTest
        ]
  where
    exactTest = do
        let Right (_, st) = runProofM (exact "H") (singleRule "Y" "H" (Rule [] [] "Y"))
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
        let result = runProofM (apply "H" []) (singleRule "P" "H" (Rule [] []  "P"))
        case result of
            Left e -> assertFailure e
            Right (_, st) -> assertBool "goal is satisfied" $ isTop $ st ^. _goal
    noMatch = do
        let result = runProofM (apply "H" []) (singleRule "P" "H" (Rule [] [] "Q"))
        assertBool "fail because goal doesn't match conclusion of applied formula"
            $ isLeft result
    applicationGoal = do
        let result =
                runProofM
                    (apply "H" [])
                    (singleRule ("impl" ## "P" ## "Q") "H" (Rule [] [] ("impl" ## "P" ## ("impl" ## "P" ## "Q"))))
        case result of
            Left e -> assertFailure ("failed with error: " ++ e)
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
                runProofM
                    proof
                    (ProofState
                        { goal = "Y"
                        , env =
                            M.fromList
                                [ ("mp", Rule ["P", "Q"] ["P", "impl" ## "P" ## "Q"] "Q")
                                , ("X", Rule [] [] "X")
                                , ("Y", Rule [] [] "Y")
                                , ("H", Rule [] [] ("impl" ## "X" ## "Y"))
                                ]
                        }
                    )
        assertBool "goal is satisfied" $ isTop $ st ^. _goal

    proof_mp2 = do
        let proof = do
                apply "X1"
                    [ apply "X"  []
                    , apply "X0" []
                    ]
        let Right (_, st) =
                runProofM
                    proof
                    (ProofState
                        { goal = "R"
                        , env =
                            M.fromList
                                [ ("X", Rule [] [] "P")
                                , ("X0", Rule [] [] "Q")
                                , ("X1", Rule [] ["P", "Q"] "R")
                                ]
                        }
                    )
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
                runProofM
                    proof
                    (ProofState
                        { goal = "R"
                        , env =
                            M.fromList
                                [ ("X", Rule [] ["P"] "Q")
                                , ("X0", Rule [] ["P", "Q"] "R")
                                , ("X1", Rule [] [] "P")
                                ]
                        }
                    )
        assertEqual "goal is top" "top" (st ^. _goal)

-----------
-- Utils --
-----------

singleRule :: Goal -> String -> Declaration -> ProofState
singleRule g k v =
    ProofState
        { goal = g
        , env = M.singleton k v
        }