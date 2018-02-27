Lemma doubleneg2(P:Prop):(not (not P))→ P.

Qed.
Lemma implies2(P Q:Prop): (P->Q)->(not P \/ Q).


Qed.
Lemma demorgan3 (P Q:Prop): not (P\/Q)->(not P /\ not Q).


Qed.
Lemma demorgan4 (P Q:Prop): (not P /\ not Q) -> not (P\/Q).

Qed.

