Variable Beings:Set.
Variables Babies Illogical ManageCroc Despised: Beings->Prop.
Axiom LCa: forall x, Babies x -> Illogical x.
Axiom LCb : forall x, ManageCroc x -> not (Despised x).
Axiom LCc: forall x, Illogical x ->Despised x.
Lemma LcBabies: forall x, Babies x -> not (ManageCroc x).
Fix an arbitrary element x.
Assume (Babies x) then prove (not (ManageCroc x)).
Rewrite goal using the definition of not.
Assume (ManageCroc x) then prove False.
obtain (Illogical x) applying (LCa x) to Hyp.
obtain (Despised x) applying (LCc x) to H.
obtain (not (Despised x)) applying (LCb x) to Hyp0.
Apply result (H1 H0).
Qed.

