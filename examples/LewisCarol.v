(* This is my first Coq proof. *)
Lemma m (x y:nat): x+y=y+x.
induction x.
(* The "trivial" tactic proves the base case. *)
trivial.
simpl.
rewrite IHx.
apply plus_n_Sm.
Qed.


(* Lewis Carol proof.  
     All men are mortal.
     Minor premise: Socrates is a man.
     Conclusion: Socrates is a mortal.
*)
Variable Beingings:Set.
Variable Socrates:Beingings.
Variable Men:Beingings->Prop.
Variable Mortal:Beingings->Prop.
(* This following is an axiom because it is an assumption in the problem above. *)
Axiom MenMortal: (forall x, Men x-> Mortal x).
Axiom SocMen: Men Socrates.
Lemma LC: Mortal Socrates.
apply MenMortal.
apply SocMen.
Qed.

Print LC.

Lemma LClong : (forall x, Men x-> Mortal x)-> (Men Socrates)-> Mortal Socrates.
(* Tidy up and get started. *)
intro.
intro.
(* Above two lines can be abreviated to intros. (with an "s").  *)
apply H.
apply H0.
(* Note, we apply the hypotheses "in revese" because we successivly reformulate the goal. *)
Qed.

(* 
(a) All babies are illogical.
(b) Nobody is despised who can manage a crocodile.
(c) Illogical persons are dispised.
*)

Variable Babies:Beingings->Prop.
Variable Illogical:Beingings->Prop.
Variable Despised:Beingings->Prop.
Variable ManageCroc:Beingings->Prop.
Axiom LCa: (forall x, Babies x -> Illogical x).
Axiom LCb: (forall x, ManageCroc x -> not(Despised x)).
Axiom LCc: (forall x, Illogical x -> Despised x).
Lemma BabyCroc: (forall x, Babies x -> not(ManageCroc x)).
intros.
unfold not.
intros.
(* assert is the "only" way coq allows you to "go forward". *)
assert (Illogical x).
apply LCa.
apply H.
assert (Despised x).
apply LCc.
apply H1.
assert (not(Despised x)).
apply LCb.
apply H0.
unfold not in H3.
apply (H3 H2).
Qed.

(* Propositional version of this proof.  *)


Variable Babies2:Prop.
Variable Illogical2:Prop.
Variable Despised2:Prop.
Variable ManageCroc2:Prop.
Axiom LCa2: (Babies2 -> Illogical2).
Axiom LCb2: (ManageCroc2 -> not(Despised2)).
Axiom LCc2: (Illogical2 -> Despised2).
Lemma BabyCroc2: (Babies2 -> not(ManageCroc2)).
intros.
unfold not.
intros.
assert Illogical2.
apply LCa2.
apply H.
assert (Despised2).
apply LCc2.
apply H1.
assert (not(Despised2)).
apply LCb2.
apply H0.
unfold not in H3.
apply (H3 H2).
Qed.