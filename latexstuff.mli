val removed : 'a list -> 'a list
val conclusion :
  Processresults.goal Treestuff.tree -> Processresults.statement
val gethyps :
  Processresults.goal Treestuff.tree -> Processresults.statement list
val newconclusion :
  Processresults.goal Treestuff.tree -> int -> Processresults.statement
val newhyp :
  Processresults.goal ->
  Processresults.goal Treestuff.tree -> Processresults.statement list
val deadhyp :
  Processresults.goal Treestuff.tree -> Processresults.statement list
val change : string -> string -> string list -> string
val header : string
val latex : Processresults.goal Treestuff.tree -> string