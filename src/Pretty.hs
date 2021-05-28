module Pretty
    ( Pretty
    , pretty
    , toString
    ) where

import           Print
import           Text.PrettyPrint

import           Language.Syntax

class Pretty a where
    pretty :: a -> Doc

    toString :: a -> String
    toString = render . pretty

instance Pretty ModDef where
    pretty (ModDef name decls) =
        docName <+> lbrace $+$ nest 4 docDecls $+$ rbrace
      where
        docName = doubleQuotes (text name)
        docDecls = vcat $ map pretty decls

instance Pretty Declaration where
    pretty = \case
        MetaSym name _ -> text name
        Notation name _ def _ -> docDef name def
        Import name -> text name
        Rule name args hs c ->
            let name' = text name
                args' = text $ unwords args
                hypotheses = pretty hs
                conclusion = pretty c
            in if null args
                    then hsep [name', definition, hypotheses, vdash, conclusion]
                    else hsep [name', definition, forall, args', colon, hypotheses, vdash, conclusion]
        Lemma name args hs c proof ->
            let docProof = vcat $ map pretty proof
                args' = hsep $ map text args
                hypotheses = text "_"
                conclusion = text "_"
            in  hsep
                    [ text ("L_" ++ name)
                    , definition
                    , forall
                    , args'
                    , colon
                    , hypotheses
                    , conclusion
                    ]
                    $+$ nest 4 (docProof $+$ qed)

docDef :: String -> Expr -> Doc
docDef name expr  = text name <+> definition <+> pretty expr

instance Pretty Expr where
    pretty = \case
        Ident s -> if s == "top" then top else text s
        Application e1 e2 ->
            let e2' = (if isApplication e2 then parens else id) (pretty e2)
            in pretty e1 <+> e2'
      where
        isApplication :: Expr -> Bool
        isApplication (Application _ _) = True
        isApplication _                 = False


instance Pretty a => Pretty [a] where
    pretty xs = brackets $ hcat (punctuate comma $ map pretty xs)

instance Pretty Tactic where
    pretty = \case
        Intros asName -> text "intros" <+> text asName
        Specialize e asName -> text "specialize" <+> pretty e <+> text asName
        Apply e maybeAsName -> text "apply" <+> pretty e <+> text (show maybeAsName)
        Exact name -> text "exact" <+> text name
