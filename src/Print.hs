module Print where

import Text.PrettyPrint

top :: Doc
top = char '⊤'

vdash :: Doc
vdash = char '⊢'

lambda :: Doc
lambda = char 'λ'
-- let char_arrow ()  = if !Config.ascii then "->" else "→"
-- let char_darrow () = if !Config.ascii then "=>" else "⇒"
-- let char_prod ()   = if !Config.ascii then "forall" else "Π"
forall :: Doc
forall = char '∀'
-- let char_equal ()  = if !Config.ascii then "==" else "≡"

definition :: Doc
definition = text ":=" --char '≔'

qed :: Doc
qed = char '□'
