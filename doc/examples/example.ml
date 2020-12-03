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
​
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
​
  lemma a2i P Q R :=
    from [impl (P (impl Q R))]
    derive (impl (impl P Q) (impl Q R))
  proof
    intros H1
    specialize (a2 P Q R) as H2
    apply (modus_ponens H1 H2)
  qed
​
  lemma mpd P Q R :=
    from [(impl P Q), (impl P (impl Q R))]
    derive (impl P R)
  proof
    intros H1 H2
    specialize (a2i _ _ _ H2) as H2'
    apply (modus_ponens _ _ H1 H2') as H3
    exact H3
  qed
​
  lemma impl_id P :=
    from []
    derive (impl P P)
  proof
    specialize (a1 P P) as H1
    specialize (a1 P (impl P P)) as H2
    apply mpd H1 H2
  qed
endmodule
​
module FOL-REASONING
  imports PROPOSITIONAL-LOGIC
  rule exists_q (x : Var) (y : Var) P :=
    from []
    derive (impl (#subst P x y) (exists x P))
  rule exists_gen (x : Var) P Q :=
    from [impl P Q]
    derive (impl (exists x P) Q)
    [is_free(x, Q)]
endmodule
​
module ML-FRAME-REASONING
  imports ML-SYNTAX
  
  rule bot_propagation (C : AppContext) :=
    from []
    derive (impl (#subst C HOLE bot) bot)
​
  rule or_propagagation (C : AppContext) P Q :=
    from []
    derive (impl (#subst C HOLE (or P Q)) (or (#subst C HOLE P) (#subst C HOLE Q)))
​
  rule exists_propagation (C : AppContext) (x : Var) P :=
    from []
    derive (impl (#subst C HOLE (exists x P)) (exists x (#subst C HOLE P)))
    [is_free(x, C)]
​
  rule framing (C : AppContext) P Q :=
    from [impl P Q]
    derive (impl (#subst C HOLE P) (#subst C HOLE Q))
​
endmodule
​
module ML-FIXPOINT-REASONING
  imports ML-SYNTAX
  
  rule set_variable_substitution (X : SetVar) P Q :=
    from [P]
    derive (#subst P X Q)
  
  rule pre_fixpoint (X : SetVar) P :=
    from [#subst P X (mu X P)]
    derive (mu X P_
  
  rule knaster_tarski (X : SetVar) P Q :=
    from [impl (#subst P X Q) Q]
    derive (impl (mu X P) Q)
endmodule
    
module ML-TECHNICAL-RULES
  imports ML-SYNTAX
  
  rule existence (x : Var) :=
    from []
    derive (exists x x)
  
  rule singleton (C1 : AppContext) (C2 : AppContext) (x : Var) P :=
    from []
    derive (not (and (#subst C1 HOLE (and x P)) (#subst C2 HOLE (and x (not P)))))
endmodule
​
module ML-SYSTEM
  imports FOL-LOGIC
  imports ML-FRAME-REASONING
  imports ML-FIXPOINT-REASONING
  imports ML-TECHNICAL-RULES
endmodule
​
module DEFINEDNESS
  imports ML-SYSTEM
​
  symbol #defined	[arity(1)]
  rule definedness (x : Var) :=
    from []
    derive (#defined x)
   
  meta-symbol #total  	[arity(1)]
  notation #total P :=
    not (#defined (not P))
  
  meta-symbol in	[arity(2)]
  notation in (x : Var) P :=
    #defined (and x P)
​
  meta-symbol eq	[arity(2)]
  notation eq P Q :=
    #total (iff P Q)
    
  meta-symbol incl	[arity(2)]
  notation incl P Q :=
    #total (impl P Q)
endmodule
