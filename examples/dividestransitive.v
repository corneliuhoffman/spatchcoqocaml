Definition divides (a b:nat) := exists x:nat, b = a*x.
Notation " a | b " := (divides a b) (at level 10).
Theorem refldiv:forall a b c, (a | b) /\ (b | c) -> (a |c).
Fix an arbitrary element a.
Fix an arbitrary element b.
Fix an arbitrary element c.
Assume (a | b /\ b | c ) then prove (a |c).
Eliminate the conjuction in hypothesis Hyp.
Rewrite hypothesis Hyp0 using the definition of divides.
Rewrite hypothesis Hyp1 using the definition of divides.
Rewrite goal using the definition of divides.
Fix x the existentially quantified variable in Hyp1.
Rewrite the goal using Hyp1.
Fix y the existentially quantified variable in Hyp0.
Rewrite the goal using Hyp0.
Prove the existential claim is true for (y*x).
True by arithmetic properties.
Qed.

