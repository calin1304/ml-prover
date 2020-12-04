module ML-SYNTAX
  meta-symbol #subst    [arity(3), substitution]
  meta-symbol bot 	[arity(0)]
  meta-symbol impl 	[arity(2)]
  meta-symbol exists 	[arity(2), binder]
  meta-symbol mu 	[arity(2), set-binder, notNegative]
  meta-symbol not 	[arity(1)]
  notation not (X : SetVar) :=
    impl X bot
    [folded]
  meta-symbol top	[arity(0)]
  notation top :=
    not bot
    [folded]
  meta-symbol and 	[arity(2)]
  notation and X Y :=
    not (impl X (not Y))
    [folded]
  meta-symbol or 	[arity(2)]
  notation or X Y :=
    not (and (not X) (not Y))
    [folded]
  meta-symbol iff 	[arity(2)]
  notation iff X Y :=
    not (and (impl X Y) (impl Y X))
    [folded]
  meta-symbol forall 	[arity(2), binder]
  notation forall (x : Var) E :=
    not (exists x (not E))
    [folded]
  meta-symbol nu 	[arity(2), set-binder, notNegative]
  notation nu (X : SetVar) E :=
    not (mu X (not (#subst E X (not X))))
    [folded]
endmodule
 
module PROPOSITIONAL-LOGIC
  imports ML-SYNTAX
  rule a1 P Q :=
    from []
    derive (impl (P (impl Q P)))
  rule a2 P Q R :=
    from []
    derive (impl (impl P (impl Q R)) (impl (impl P Q) (impl P R)))
  rule a3 P Q :=
    from []
    derive (impl (impl (not P) (impl Q)) (impl Q P))
  rule modus_ponens P Q :=
    from [P, (impl P Q)]
    derive Q
  lemma a2i P Q R :=
    from [impl (P (impl Q R))]
    derive (impl (impl P Q) (impl Q R))
  proof
    intros H1
    specialize (a2 P Q R) as H2
    apply (modus_ponens H1 H2)
  qed
  lemma mpd P Q R :=
    from [(impl P Q), (impl P (impl Q R))]
    derive (impl P R)
  proof
    intros H1 H2
    specialize (a2i _ _ _ H2) as H2'
    apply (modus_ponens _ _ H1 H2') as H3
    exact H3
  qed
  lemma impl_id P :=
    from []
    derive (impl P P)
  proof
    specialize (a1 P P) as H1
    specialize (a1 P (impl P P)) as H2
    apply mpd H1 H2
  qed
endmodule