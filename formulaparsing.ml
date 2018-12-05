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
  |Pow(e, f) -> let ee=match e with
  Var a-> print e
  |_->"("^(print e)^")" in
  let ff= match f with
  Var a-> (print f)
  |_->"("^(print f)^")" in
  ee^"^"^ff
(*   | Succ(e) -> "("^(print e)^" + 1)"
 *)  
 |Succ(e) ->(match e with
  Var a-> "S "^(print e)
  |_->"S ("^(print e)^")")
 | EmptySet -> " ∅ "
  | Le(e1,e2) -> 
  let a = match e1 with Var x -> print e1 
      |_ ->"("^(print e1)^")" in
      let b= match e2 with Var x -> print e2 
      |_ ->"("^(print e2)^")" in 
     a^" <= "^b
  | Minus(e1,e2) -> 
  let a = match e1 with Var x -> print e1 
      |_ ->"("^(print e1)^")" in
      let b= match e2 with Var x -> print e2 
      |_ ->"("^(print e2)^")" in 
     a^" - "^b

  | Div(e1,e2)          -> 
      let a = match e1 with Var x -> print e1 
      |_ ->"("^(print e1)^")" in
      let b= match e2 with Var x -> print e2 
      |_ ->"("^(print e2)^")" in 
     a^" | "^b
  | Add(e1,e2)          -> 
      let a = match e1 with Var x -> print e1 
      |_ ->"("^(print e1)^")" in
      let b= match e2 with Var x -> print e2 
      |_ ->"("^(print e2)^")" in 
     a^" + "^b
  | Union(e1,e2)          -> 
      let a = match e1 with Var x -> print e1 
      |_ ->"("^(print e1)^")" in
      let b= match e2 with Var x -> print e2 
      |_ ->"("^(print e2)^")" in 
     a^" ∪ "^b   
  | Intersection(e1,e2)          -> 
      let a = match e1 with Var x -> print e1 
      |_ ->"("^(print e1)^")" in
      let b= match e2 with Var x -> print e2 
      |_ ->"("^(print e2)^")" in 
     a^" ∩ "^b

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
  | Subset(e1,e2)          -> 
      let a = match e1 with Var x -> print e1 
      |_ ->"("^(print e1)^")" in
      let b= match e2 with Var x -> print e2 
      |_ ->"("^(print e2)^")" in 
     a^" ⊆ "^b
| In(e1,e2)          -> 
      let a = match e1 with Var x -> print e1 
      |_ ->"("^(print e1)^")" in
      let b= match e2 with Var x -> print e2 
      | EmptySet -> print e2
      |_ ->"("^(print e2)^")" in 
     a^" ∈ "^b
 | Iff(e1,e2)          -> 
      let a = match e1 with Var x -> print e1 
      |_ ->"("^(print e1)^")" in
      let b= match e2 with Var x -> print e2 
      |_ ->"("^(print e2)^")" in 
     a^" ↔ "^b
| Complement(Var e1)          -> "∁ "^(e1)
  | Complement(e)               -> "∁ ("^(print e)^")"

  |Empty   -> "∅"

   
  
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
  |List(e1::e2) -> (* match e1 with Var x  -> *) (print e1)^" "^(String.concat " " (List.map print e2))
  (* | List( _ )  -> (print e1)^" "^(String.concat " " (List.map print e2))
 
  | _-> "("^(print e1)^") "^(String.concat " " (List.map print e2))
   *)
(* prints a formula *)
let rec getlist x = 

  let unsorted = match x with 
  

  
  | Not(e) | Complement(e) |Succ(e)              -> (print x)::(getlist e)
  | Or(e1,e2) | And(e1, e2) | Div(e1, e2) | Implies(e1,e2) | Add(e1,e2) | Subset(e1,e2) | Union (e1,e2) | Intersection(e1, e2) | In(e1, e2) | Iff(e1, e2) | Minus(e1,e2) | Times(e1,e2) | Equals(e1, e2) | Exists(e1 , e2) | Forall(e1 , e2)
         -> (print x)::((getlist e1)@(getlist e2))

  |List(e1::t) -> (print x)::((getlist e1)@(List.concat (List.map getlist t)))
  |_ -> [print x] in
  
 let sorted = List.sort (fun x y-> String.length y - String.length x) unsorted in
 
 sorted
(* produces subformulas *)
let med string =
  let st1 = Str.global_replace (Str.regexp "<b>") "" string in
  Str.global_replace (Str.regexp "</b>") "" st1
let parant ast =
  match ast with
  | Var a -> print ast
  | _ -> "("^(print ast )^")"

let produce_possible_tactics_goal ast name goal =
Printf.printf "this is the form %s\n\n" (Ast.to_string ast);flush_all ();
print_string (string_of_bool goal);
  match ast with
  | Implies(a, b) -> if  goal then ["Assume "^(parant a)^" then prove "^(parant b)^".";"Rewrite the goal using VAR.";"Apply result VAR."; "Rewrite goal using the definition of VAR."; "This follows from assumptions."; "This is trivial."] else ["Rewrite hypothesis "^(med name)^" using the definition of VAR.";"Apply result "^(med name)^"."; "Rewrite the goal using "^(med name)^"."]
 | Iff(a,b) -> if goal then ["Prove both directions of "^(parant a)^" iff "^(parant b)^"."; "Rewrite the goal using VAR.";"Apply result VAR."; "Rewrite goal using the definition of VAR."; "This follows from assumptions."; "This is trivial."] else ["Eliminate the conjuction in hypothesis "^(med name)^".";"Rewrite hypothesis "^(med name)^" using the definition of VAR.";"Apply result "^(med name)^"."; "Rewrite the goal using "^(med name)^"."]
  | Or(a,b) -> if goal then  (["Prove left hand side."; "Prove right hand side.";
     "Prove "^(parant a)^" in the disjunction.";
     "Prove "^(parant b)^" in the disjunction."; "Rewrite the goal using VAR.";"Apply result VAR."; "Rewrite goal using the definition of VAR.";"This follows from assumptions."; "This is trivial."]) else ["Consider cases based on disjunction in hypothesis "^(med name)^".";"Rewrite hypothesis "^(med name)^" using the definition of VAR."; "Apply result "^(med name)^"."; "Rewrite the goal using "^(med name)^"."]
| And(a,b) -> if goal then  ["Prove the conjunction in the goal by first proving "^(parant a)^" then "^(parant b)^"."; "Rewrite the goal using VAR.";"Apply result VAR."; "Rewrite goal using the definition of VAR."; "This follows from assumptions.";"This is trivial."]
      else ["Eliminate the conjuction in hypothesis "^(med name)^".";"Rewrite hypothesis "^(med name)^" using the definition of VAR.";"Apply result "^(med name)^"."; "Rewrite the goal using "^(med name)^".";"This follows from assumptions." ]

   |Forall(a, b) -> if goal then ["Fix an arbitrary element VAR."; "Apply induction on "^(parant a)^"."; "Rewrite the goal using VAR.";"Apply result VAR."; "Rewrite goal using the definition of VAR."; "This follows from assumptions."; "This is trivial."] else ["Obtain VAR using variable VAR in the universally quantified hypothesis "^(med name)^".";"Rewrite hypothesis "^(med name)^" using the definition of VAR.";"Apply result "^(med name)^"."; "Rewrite the goal using "^(med name)^"."]
   |Exists(a, b) -> if goal then ["Prove the existential claim is true for VAR."; "Rewrite the goal using VAR.";"Apply result VAR."; "Rewrite goal using the definition of VAR.";"This follows from assumptions.";"This is trivial."] else ["Fix VAR the existentially quantified variable in "^(med name)^".";"Rewrite hypothesis "^(med name)^" using the definition of VAR."; "Apply result "^(med name)^"."; "Rewrite the goal using "^(med name)^"."]
  
|Equals(a, b) -> if goal then ["Rewrite the goal using VAR.";"Apply result VAR."; "Rewrite goal using the definition of VAR.";"This follows from assumptions.";"Replace VAR by VAR in the goal"] else ["Replace ("^(print a)^") by ("^(print b)^") in the goal.";"Replace ("^(print b)^") by ("^(print a)^") in the goal."; "Replace ("^(print b)^") by ("^(print a)^") in hypothesis VAR";"Rewrite hypothesis "^(med name)^" using the definition of VAR.";"Apply result "^(med name)^"."; "Rewrite the goal using "^(med name)^".";"Claim VAR by rewriting "^(med name)^" using VAR."]
  |_->  if  goal then ["Rewrite the goal using VAR.";"Apply result VAR."; "Rewrite goal using the definition of VAR.";"Replace VAR by VAR in the goal";"This follows from assumptions.";"This is trivial."] else ["Rewrite hypothesis "^(med name)^" using the definition of VAR.";"Apply result "^(med name)^"."; "Rewrite the goal using "^(med name)^".";"This follows from assumptions."]
;;

print_string (Ast.to_string (parse "∀  n  :  nat,  n  ≠  0"));;
flush_all ()


      

