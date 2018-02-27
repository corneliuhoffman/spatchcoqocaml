Variable Things:Type.
Variable this_dish:Things.
Variables Pudding Nice Wholesome:Things->Prop.
Notation "x 'is' 'a' 'pudding'":=(Pudding x)(at level 10).
Notation "x 'is' y":=(y x)(at level 10).
Axiom PN:forall x, (x is a pudding)-> x is Nice.
Axiom TP: this_dish is a pudding.
Axiom NW: forall x, x is Nice-> not (x is Wholesome).
Lemma a: not (this_dish is Wholesome).
Apply result NW.
Apply result PN.
Apply result TP.
Qed.
