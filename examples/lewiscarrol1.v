Variable Beings:Set.
Variables Babies Illogical ManageCroc Despised: Beings->Prop.
Notation "x 'is' 'a' 'Baby' ":=(Babies x) (at level 10).
Notation "x 'is' 'illogical' ":=(Illogical x) (at level 10).
Notation "x 'is' 'despised' ":=(Despised x) (at level 10).
Notation "x 'can' 'manage' 'crocodiles'":=(ManageCroc x) (at level 10).
Axiom BI: forall x, x is a Baby -> x is illogical.
Axiom MND : forall x, x can manage crocodiles -> not (x is despised).
Axiom ID: forall x, x is illogical  -> x is despised.
Lemma LcBabies: forall x, x is a Baby -> not (x can manage crocodiles).
Fix an arbitrary element x.
Assume (x is a Baby) then prove (not (x can manage crocodiles)).
Rewrite goal using the definition of not.
Assume (x can manage crocodiles) then prove False.
Claim (x is illogical).
Apply result BI.
This follows from assumptions.
Obtain (x is despised) applying (ID x) to H.
Obtain (not (x is despised)) applying (MND x) to Hyp0.
Apply result H1 .
This follows from assumptions.
