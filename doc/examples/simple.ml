module ML-SYNTAX
    meta-symbol subst    [arity(3), substitution]
    meta-symbol bot 	 [arity(0)]
    meta-symbol impl 	 [arity(2)]
    meta-symbol exists 	 [arity(2), binder]
    meta-symbol mu 	     [arity(2), set-binder, notNegative]

    meta-symbol not [arity(1)]
    notation not (X : SetVar) := impl X bot [folded]
  
    meta-symbol top	[arity(0)]
    notation top := not bot [folded]

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