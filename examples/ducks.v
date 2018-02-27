Variable Poultry:Type.
Variables Waltz Duck Officer:Poultry->Prop.
Notation "x 'is' 'a' 'duck'":=(Duck x)(at level 10).
Notation "x 'will' 'waltz'":=(Waltz x)(at level 10).
Notation "x 'is' 'an' 'officer'":=(Officer x)(at level 10).
Axiom DW:forall x:Poultry, (x is a duck)-> not (x will waltz).
Axiom OW: forall x, x is an officer -> x will waltz.
Axiom D: forall x, x is a duck.
Lemma a: forall x, not (x is an officer).
Fix an arbitrary element d.
Rewrite goal using the definition of not.
Assume (d is an officer) then prove False.
Claim (d is a duck).
Apply result (D).
Claim  (not (d will waltz)).
Apply result DW.
This follows from assumptions.
Rewrite hypothesis H0  using the definition of not.
Apply result H0 .
Apply result OW.
This follows from assumptions.
Qed.
