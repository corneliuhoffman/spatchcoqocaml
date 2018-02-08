Lemma assume:forall P:Prop, P->P.

Qed.
Lemma right: forall P Q:Prop, Q->P\/Q.


Qed.
Lemma comm1(P Q:Prop): P/\Q->Q/\P.


Qed.
Lemma distr1(P Q R:Prop): P/\(Q\/R)->((P/\Q)\/(P/\R)).


Qed.
Lemma distr2(P Q R:Prop): ((P/\Q)\/(P/\R)) ->P/\(Q\/R).


Qed.
Lemma doubleneg(P:Prop): P->(not (not P)).


Qed.
Lemma counterpos(P Q:Prop):(P->Q)->(not Q -> not P).

Qed.

