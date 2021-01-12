module Main(main) where

import Test.Tasty

import Test.Prover.Tactics (tests)

main = defaultMain tests
