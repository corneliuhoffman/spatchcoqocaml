Definition div a b:= exists c, b = a* c.
Notation "a | b" := (div a b)(at level 0).
Definition even a := exists c, a = 2* c.
Definition odd a := exists c, a = 2* c+1.
Lemma nt0: even 12.
Rewrite goal using the definition of even.
Prove the existential claim is true for 6.
True by arithmetic properties.
Qed.
Lemma nt1 (a b c:nat): a | b /\ b | c  ->  a | c.
Assume ((a | b) /\  (b | c)) then prove (a | c).
Rewrite hypothesis Hyp  using the definition of div.
Rewrite goal using the definition of div.
Eliminate the conjuction in hypothesis Hyp .
Fix x the existentially quantified variable in Hyp0 .
Fix y the existentially quantified variable in Hyp1 .
Rewrite the goal using Hyp1 .
Rewrite the goal using Hyp0 .
Prove the existential claim is true for (x*y).
True by arithmetic properties.
Qed.
Lemma nt2 (a b c d : nat): (a | c) /\ (b | d)  ->  ((a*b) | (c*d)).
Assume ((a | c) /\  (b | d)) then prove ((a * b) | (c * d)).
Rewrite hypothesis Hyp  using the definition of div.
Eliminate the conjuction in hypothesis Hyp .
Fix x the existentially quantified variable in Hyp0 .
Fix y the existentially quantified variable in Hyp1 .
Rewrite the goal using Hyp0 .
Rewrite the goal using Hyp1 .
Rewrite goal using the definition of div.
Prove the existential claim is true for (x*y).
True by arithmetic properties.
Qed.
Lemma nt3 (a b c:nat):   a | b  /\ a | c   ->  a |( b+c).
Assume ((a | b) /\  (a | c)) then prove (a | (b + c)).
Rewrite hypothesis Hyp  using the definition of div.
Eliminate the conjuction in hypothesis Hyp .
Fix x the existentially quantified variable in Hyp0 .
Fix y the existentially quantified variable in Hyp1 .
Rewrite the goal using Hyp0 .
Rewrite the goal using Hyp1 .
Rewrite goal using the definition of div.
Prove the existential claim is true for (x+y).
True by arithmetic properties.
Qed.
Lemma nt4 (n m:nat): (odd n) /\ (odd m) -> (even (m+n)).
Assume ((odd n) /\ (odd m)) then prove (even (m+n)).
Rewrite goal using the definition of even.
Rewrite hypothesis Hyp  using the definition of odd.
Eliminate the conjuction in hypothesis Hyp .
Fix x the existentially quantified variable in Hyp0 .
Fix y the existentially quantified variable in Hyp1 .
Rewrite the goal using Hyp0 .
Rewrite the goal using Hyp1 .
Prove the existential claim is true for (x+y+1).
True by arithmetic properties.
Qed.
Lemma nt5 (n:nat): odd (n + (n+1)).
Rewrite goal using the definition of odd.
Prove the existential claim is true for n.
True by arithmetic properties.
Qed.
Lemma nt6 (n:nat): even n \/ odd n.
Rewrite goal using the definition of even.
Rewrite goal using the definition of odd.
Apply induction on n.
Prove left hand side.
Prove the existential claim is true for 0.
True by arithmetic properties.
Consider cases based on disjunction in hypothesis IHn .
Prove right hand side.
Fix c the existentially quantified variable in Hyp .
Rewrite the goal using Hyp .
Prove the existential claim is true for c.
True by arithmetic properties.
Prove left hand side.
Fix c the existentially quantified variable in Hyp0 .
Rewrite the goal using Hyp0 .
Prove the existential claim is true for (c+1).
True by arithmetic properties.
Qed.
Lemma nt7 (n:nat): even (n*(n+1)).
Rewrite goal using the definition of even.
Obtain (even n \/ odd n) applying (nt6) to n.
Consider cases based on disjunction in hypothesis H .
Rewrite hypothesis Hyp  using the definition of even.
Fix c the existentially quantified variable in Hyp .
Rewrite the goal using Hyp .
Prove the existential claim is true for (c * ((2 * c) + 1)).
This is trivial.
Rewrite hypothesis Hyp0  using the definition of odd.
Fix c the existentially quantified variable in Hyp0 .
Rewrite the goal using Hyp0 .
Prove the existential claim is true for (((2 * c) + 1) * ( c + 1)).
True by arithmetic properties.
Qed.
