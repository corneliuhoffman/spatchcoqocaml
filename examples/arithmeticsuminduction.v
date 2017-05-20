Fixpoint sum (n:nat) : nat  :=  match n with  |0 =>0 | S p => p +1+ (sum p) end.
Lemma s (n:nat): 2* (sum n) = n* (n+1).
Apply induction on n.
Rewrite goal using the definition of sum.
True by arithmetic properties.
Claim (2* sum (S n)= 2*(n+1+ (sum n))).
This follows from reflexivity.
Rewrite the goal using H.
Claim (2  *  (n  +  1  +  sum  n) = 2* n + 2 + 2* (sum n)).
True by arithmetic properties.
Rewrite the goal using H0.
Rewrite the goal using IHn.
True by arithmetic properties.
Qed.

