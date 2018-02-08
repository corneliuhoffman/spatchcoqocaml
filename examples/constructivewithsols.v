Lemma assume:forall P:Prop, P->P.
Fix an arbitrary element P.
Assume P then prove P.
This follows from assumptions.
Qed.
Lemma left( P Q:Prop):P->P\/Q.
Assume P then prove (P\/Q).
Prove left hand side.
This follows from assumptions.
Qed.
Lemma right: forall P Q:Prop, Q->P\/Q.
Fix an arbitrary element P.
Fix an arbitrary element Q.
Assume Q then prove (P\/Q).
Prove right hand side.
This follows from assumptions.
Qed.
Lemma comm1(P Q:Prop): P/\Q->Q/\P.
Assume (P/\Q) then prove (Q/\P).
Eliminate the conjuction in hypothesis Hyp.
Prove the conjunction in the goal by first proving Q then P.
This follows from assumptions.
This follows from assumptions.
Qed.
Lemma distr1(P Q R:Prop): P/\(Q\/R)->((P/\Q)\/(P/\R)).
Assume (P  ∧  (Q  ∨  R)) then prove ((P/\Q)\/(P/\R)).
Eliminate the conjuction in hypothesis Hyp.
Consider cases based on disjunction in hypothesis Hyp1.
Prove left hand side.
Prove the conjunction in the goal by first proving P then Q.
This follows from assumptions.
This follows from assumptions.
Prove right hand side.
Prove the conjunction in the goal by first proving P then R.
This follows from assumptions.
This follows from assumptions.
Qed.
Lemma distr2(P Q R:Prop): ((P/\Q)\/(P/\R)) ->P/\(Q\/R).
Assume ((P/\Q)\/(P/\R)) then prove (P/\(Q\/R)).
Consider cases based on disjunction in hypothesis Hyp.
Eliminate the conjuction in hypothesis Hyp0.
Prove the conjunction in the goal by first proving P then (Q\/R).
This follows from assumptions.
Prove left hand side.
This follows from assumptions.
Eliminate the conjuction in hypothesis Hyp1.
Prove the conjunction in the goal by first proving P then (Q\/R).
This follows from assumptions.
Prove right hand side.
This follows from assumptions.
Qed.
Lemma doubleneg(P:Prop): P->(not (not P)).
Assume P then prove (not (not P)).
Rewrite goal using the definition of not.
Assume (P->False) then prove False.
Apply result (Hyp0 Hyp).
Qed.
Lemma counterpos(P Q:Prop):(P->Q)->(not Q -> not P).
Assume (P->Q) then prove (not Q -> not P).
Assume (not Q) then prove (not P).
Rewrite goal using the definition of not.
Assume P then prove False.
Claim Q.
Apply result (Hyp Hyp1).
Apply result (Hyp0 H).
Qed.

