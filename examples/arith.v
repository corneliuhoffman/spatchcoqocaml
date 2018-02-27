Definition div a b:= exists c, b = a* c.
Notation "a | b" := (div a b)(at level 0).
Definition even a := exists c, a = 2* c.
Definition odd a := exists c, a = 2* c+1.

Lemma nt0 (a b c:nat): a | b /\ b | c  ->  a | c.
Assume ((a | b) ∧ (b | c)) then prove (a | c).
Eliminate the conjuction in hypothesis Hyp .
Rewrite goal using the definition of div.
Rewrite hypothesis Hyp0  using the definition of div.
Rewrite hypothesis Hyp1  using the definition of div.
Fix x the existentially quantified variable in Hyp0 .
Fix y the existentially quantified variable in Hyp1 .
Rewrite the goal using Hyp1 .
Rewrite the goal using Hyp0 .
Prove the existential claim is true for (x*y).
True by arithmetic properties.
Qed.

Lemma nt1: even 12.

Qed.
Lemma nt2 (a b c d : nat): (a | c) /\ (b | d)  ->  ((a*b) | (c*d)).

Qed.
Lemma nt3 (a b c:nat):   a | b  /\ a | c   ->  a |( b+c).


Qed.
Lemma nt4 (n m:nat): (odd n) /\ (odd m) -> (even (m+n)).

Qed.
Lemma nt5 (n:nat): odd (n + (n+1)).


Qed.
Lemma nt6 (n:nat): even n \/ odd n.

Qed.
Lemma nt7 (n:nat): even (n*(n+1)).

Qed.
