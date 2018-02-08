Variables P Q:Prop.
Notation"'it' 'rains'":=P.
Notation"'I' 'take' 'my' 'umbrella'":=Q.
Axiom rain:  (it rains) -> (I take my umbrella).
Lemma a: not I take my umbrella -> not it rains.
