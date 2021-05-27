module Test.Language.Parser where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Language.Lexer
import           Language.Parser
import           Language.ParserM
import           Language.Syntax

tests :: TestTree
tests =
    testGroup "Parser"
        [ expressionParserTests
        , declarationParserTests
        ]

expressionParserTests :: TestTree
expressionParserTests =
    testGroup "expression parser"
        [ testCase "id" idParserTest
        , testCase "application" applicationParserTest
        ]
  where
    idParserTest = actual @?= expected
      where
        expected = Right "X"
        actual = runParserM parseExpression <$> scanner "X"

    applicationParserTest = actual @?= expected
      where
        expected = Right $ ("f" ## "X" ## "Y" ## "Z") ## ("g" ## "X" ## "Y")
        actual = runParserM parseExpression <$> scanner "(f X Y Z) (g X Y)"

declarationParserTests :: TestTree
declarationParserTests =
    testGroup "declaration parser"
        [ testCase "rule" ruleParserTest
        -- , testCase "metaSymParser" metaSymParserTest
        -- , testCase "notationParser" notationParserTest
        ]
  where
    notationParserTest = actual @?= expected
      where
        expected =
            Right
                $ Notation
                    "nu"
                    (Signature [Argument "X", Argument "E"])
                    nuExpr
                    [Folded, Binder, NotNegative]
        actual =
            runParserM parseDeclaration
                <$> scanner "notation nu (X : SetVar) E := not (mu X (not (#subst E X (not X)))) [folded, set-binder 1 2, notNegative]"

        nuExpr = "not" ## (("mu" ## "X") ## ("not" ## (("#subst" ## "E" ## "X") ## ("not" ## "X"))))

    metaSymParserTest = actual @?= expected
      where
        expected = Right $ MetaSym "exists" [(Arity 2), Binder]
        actual = runParserM parseDeclaration <$> scanner "meta-symbol exists [arity 2, binder 1 2]"

    ruleParserTest = actual @?= expected
      where
        expected = Right $ Rule "mp" ["X", "Y"] ["X", "impl" ## "X" ## Ident "Y"] "Y"
        actual = runParserM parseDeclaration <$> scanner "rule mp X Y := from [X, impl X Y] derive Y"
