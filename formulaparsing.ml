open Ast




(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Formulaparser.prog Formulalexer.read lexbuf in
  ast

(* Extract a value from an ast node.
   Raises Failure if the argument is a node containing a value. *)


let rec print x = match x with 
  | Var a               -> a

  | Minus(e1,e2) -> 
  let a = match e1 with Var x -> print e1 
      |_ ->"("^(print e1)^")" in
      let b= match e2 with Var x -> print e2 
      |_ ->"("^(print e2)^")" in 
     a^" - "^b
  | Add(e1,e2)          -> 
      let a = match e1 with Var x -> print e1 
      |_ ->"("^(print e1)^")" in
      let b= match e2 with Var x -> print e2 
      |_ ->"("^(print e2)^")" in 
     a^" + "^b
  | Times(e1,e2)          -> 
      let a = match e1 with Var x -> print e1 
      |_ ->"("^(print e1)^")" in
      let b= match e2 with Var x -> print e2 
      |_ ->"("^(print e2)^")" in 
     a^" * "^b
  | Implies(e1,e2)          -> 
      let a = match e1 with Var x -> print e1 
      |_ ->"("^(print e1)^")" in
      let b= match e2 with Var x -> print e2 
      |_ ->"("^(print e2)^")" in 
     a^" → "^b
 | Iff(e1,e2)          -> 
      let a = match e1 with Var x -> print e1 
      |_ ->"("^(print e1)^")" in
      let b= match e2 with Var x -> print e2 
      |_ ->"("^(print e2)^")" in 
     a^" ↔ "^b



   
  
  | Not(Var e1)          -> "not "^(e1)
  | Not(e)               -> "not ("^(print e)^")"

  | Equals( e1, e2) -> (print e1)^" = "^(print e2)

  | And(e1,e2)          -> 
      let a = match e1 with Var x -> print e1 
      |_ ->"("^(print e1)^")" in
      let b= match e2 with Var x -> print e2 
      |_ ->"("^(print e2)^")" in 
     a^" ∧ "^b
  | Or(e1,e2)          -> 
      let a = match e1 with Var x -> print e1 
      |_ ->"("^(print e1)^")" in
      let b= match e2 with Var x -> print e2 
      |_ ->"("^(print e2)^")" in 
     a^" ∨ "^b

  |Exists(e1 , e2) -> "∃ "^(print e1)^", "^(print e2)
  |Forall(e1 , e2) -> "∀ "^(print e1)^", "^(print e2)
  |List(e1::e2) -> (print e1)^" "^(String.concat " " (List.map print e2))
  
(* prints a formula *)
let rec getlist x = 

  let unsorted = match x with 
  

  
  | Not(e)               -> (print x)::(getlist e)
  | Or(e1,e2) | And(e1, e2)  | Implies(e1,e2) | Add(e1,e2) | Iff(e1, e2) | Minus(e1,e2) | Times(e1,e2) | Equals(e1, e2) | Exists(e1 , e2) | Forall(e1 , e2)
         -> (print x)::((getlist e1)@(getlist e2))

  |List(e1::t) -> (print x)::((getlist e1)@(List.concat (List.map getlist t)))
  |_ -> [print x] in
  
 let sorted = List.sort (fun x y-> String.length y - String.length x) unsorted in
 
 sorted
(* produces subformulas *)

let parant ast =
  match ast with
  | Var a -> print ast
  | _ -> "("^(print ast )^")"

let produce_possible_tactics_goal ast name goal =

  match ast with
  | Implies(a, b) -> if goal then ["Assume "^(parant a)^" then prove "^(parant b)^".";"Rewrite goal using the definition of VAR"; "This follows from assumptions."] else ["Apply result "^name^"."]
 | Iff(a,b) -> if goal then ["Prove both directions of "^(parant a)^" iff "^(parant b)^"."; "Rewrite goal using the definition of VAR"; "This follows from assumptions."] else ["Eliminate the conjuction in hypothesis "^name^"."]
  | Or(a,b) -> if goal then  (["Prove left hand side."; "Prove right hand side.";
     "Prove "^(parant a)^" in the disjunction.";
     "Prove "^(parant b)^" in the disjunction."; "Rewrite goal using the definition of VAR"; "This follows from assumptions."]) else ["Consider cases based on disjunction in hypothesis "^name^"."; "Apply result "^name^"."]
| And(a,b) -> if goal then  ["Prove the conjunction in the goal by first proving "^(parant a)^" then "^(parant b)^"."; "Rewrite goal using the definition of VAR"; "This follows from assumptions."]
      else ["Eliminate the conjuction in hypothesis "^name^"."; "Apply result "^name^"."]

   |Forall(a, b) -> if goal then ["Fix an arbitrary element VAR."; "Rewrite goal using the definition of VAR"; "This follows from assumptions."] else ["Obtain VAR using variable VAR in the universally quantified hypothesis "^name^".";"Apply result "^name^"."]
   |Exists(a, b) -> if goal then ["Prove the existential claim is true for VAR."; "Rewrite goal using the definition of VAR"; "This follows from assumptions."] else ["Fix VAR the existentially quantified variable in "^name^"."; "Apply result "^name^"."]
  
|Equals(a, b) -> if goal then ["Rewrite goal using the definition of VAR";"This follows from assumptions."] else ["Rewrite hypothesis "^name^" using the definition of VAR.";"Rewrite the goal using "^name^".";"Claim VAR by rewriting "^name^" using VAR."]
  |_->  if goal then [] else ["Rewrite goal using the definition of VAR";"This follows from assumptions."]





      

