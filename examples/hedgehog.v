Variable Creatures:Type.
Variables Read Educated Hedgehog Times:Creatures->Prop.
Notation "x 'is' y":=(y x)(at level 10).
Notation "x 'can' 'read'":=(Read x)(at level 10).
Notation "x 'takes' 'the' 'Times'":=(Times x)(at level 10).
Axiom TE:forall x, (x takes the Times)-> x is Educated.
Axiom HR: forall x, x is Hedgehog-> not (x can read).
Axiom RE: forall x, not (x can read)-> not (x is Educated).
Lemma a: forall x, (x is Hedgehog )-> (not (x takes  the Times)).
Fix an arbitrary element x.
Assume (x is Hedgehog) then prove (not (x takes the Times)).
Rewrite goal using the definition of not.
Assume (x takes the Times) then prove False.
Claim (x is Educated).
Apply result TE.
Apply result Hyp0 .
Claim (not (x can read)).
Apply result HR.
This follows from assumptions.
Claim (not (x is Educated)).
Apply result (RE).
This follows from assumptions.
Apply result H1 .
This follows from assumptions.
Qed.
