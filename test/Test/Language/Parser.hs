module Test.Language.Parser where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Language.Syntax
import Language.Lexer
import Language.Parser
import Language.ParserM

tests :: TestTree
tests =
    testGroup "Parser"
        [ testCase "idParser" idParserTest
        , testCase "applicationParser" applicationParserTest
        -- , testCase "metaSymParser" metaSymParserTest
        -- , testCase "notationParser" notationParserTest
        , testCase "ruleParser" ruleParserTest
        ]

idParserTest = actual @?= expected
  where
    expected = Right "X"
    actual = runParserM parseExpression <$> scanner "X"

applicationParserTest = actual @?= expected
  where
    expected = Right $ ("f" ## "X" ## "Y" ## "Z") ## ("g" ## "X" ## "Y")
    actual = runParserM parseExpression <$> scanner "(f X Y Z) (g X Y)"

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
    expected = Right $ Rule "mp" ["X", "Y"] mp
    actual = runParserM parseDeclaration <$> scanner "rule mp X Y := from [X, impl X Y] derive Y"
    mp = FromDerive [ "X", "impl" ## "X" ## Ident "Y" ] "Y"
