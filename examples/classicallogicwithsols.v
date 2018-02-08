Lemma doubleneg2(P:Prop):(not (not P))→ P.
Assume (not (not P)) then prove P.
Prove by contradiction.
Rewrite hypothesis Hyp using the definition of not.
Apply result (Hyp H).
Qed.
Lemma implies2(P Q:Prop): (P->Q)->(not P \/ Q).
Assume (P->Q) then prove (not P \/ Q).
Prove by contradiction.
Claim (not (not P)).
Rewrite goal using the definition of not.
Assume (P->False) then prove False.
Rewrite hypothesis H using the definition of not.
Claim ((P->False) \/ Q).
Prove left hand side.
This follows from assumptions.
Apply result (H H0).
Rewrite hypothesis H using the definition of not.
Claim P.
Apply result doubleneg2.
This follows from assumptions.
Claim Q.
Apply result (Hyp H1).
Claim ((P  →  False)  ∨  Q).
Prove right hand side.
This follows from assumptions.
Apply result (H H3).
Qed.
Lemma demorgan3 (P Q:Prop): not (P\/Q)->(not P /\ not Q).
Assume ( not (P\/Q)) then prove (not P /\ not Q).
Rewrite hypothesis  Hyp using the definition of not.
Prove the conjunction in the goal by first proving (not P) then (not Q).
Prove by contradiction.
Claim P.
Apply result doubleneg2.
This follows from assumptions.
Claim (P\/Q).
Prove left hand side.
This follows from assumptions.
Apply result (Hyp H1).
Prove by contradiction.
Claim Q.
Apply result doubleneg2.
This follows from assumptions.
Claim (P\/Q).
Prove right hand side.
This follows from assumptions.
Apply result (Hyp H1).
Qed.
Lemma demorgan4 (P Q:Prop): (not P /\ not Q) -> not (P\/Q).
Assume (not P /\ not Q) then prove (not (P\/Q)).
Prove by contradiction.
Claim (P\/Q).
Apply result doubleneg2.
This follows from assumptions.
Eliminate the conjuction in hypothesis Hyp.
Consider cases based on disjunction in hypothesis H0.
Apply result (Hyp0 Hyp).
Apply result (Hyp1 Hyp2).
Qed.

