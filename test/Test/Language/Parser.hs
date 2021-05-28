module Test.Language.Parser
    ( tests
    ) where

import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

import           Language.Lexer   (scanner)
import           Language.Parser  (parseDeclaration, parseExpression)
import           Language.ParserM (runParserM)
import           Language.Syntax  (Argument (Argument),
                                   Declaration (MetaSym, Notation, Rule),
                                   Expr (Ident), Signature (Signature),
                                   SymAttr (Arity, Binder, Folded, NotNegative),
                                   ( ## ))

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
        , testCase "metaSymParser" metaSymParserTest
        , testCase "notationParser" notationParserTest
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
        expected = Right $ MetaSym "exists" [Arity 2, Binder]
        actual = runParserM parseDeclaration <$> scanner "meta-symbol exists [arity 2, binder 1 2]"

    ruleParserTest = actual @?= expected
      where
        expected = Right $ Rule "mp" ["X", "Y"] ["X", "impl" ## "X" ## Ident "Y"] "Y"
        actual = runParserM parseDeclaration <$> scanner "rule mp X Y := from [X, impl X Y] derive Y"
