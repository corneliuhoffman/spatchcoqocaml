(* The type of the abstract syntax tree (AST). *)
type expr =
  | Var of string
  | Succ of expr
  (* | Int of int *)
  | Pow of expr*expr
  | Add of expr*expr
  | Div of expr*expr
  | Minus of expr*expr
  | Times of expr*expr
(*   | Let of string*expr*expr *)
  | Implies of expr*expr
  | Iff of expr*expr
  | And of expr*expr
  | Or of expr*expr
  | Union of expr*expr
  | Intersection of expr*expr
  | In of expr*expr
  | Complement of expr
  | Setminus of expr*expr
  | Subset of expr*expr
  | Empty
  | Equals of expr*expr
  | Not of expr
  | Exists of expr*expr
  | Forall of expr*expr
  | List of expr list
  |Parant of expr
  |EmptySet


let rec to_string expr = match expr with
| EmptySet -> "EmptySet"
| Var string -> "Var("^string^")"
(* | Int a -> "Int("^(string_of_int a)^")" *)
|Succ(a) ->  "Succ("^(to_string a)^")"
| Add( a,b) ->"Add("^(to_string a)^","^(to_string b)^")"
|Pow(a, b) ->"Pow("^(to_string a)^","^(to_string b)^")"
  
  | Implies( a,b) ->"Implies("^(to_string a)^","^(to_string b)^")"
  | Iff( a,b) ->"Iff("^(to_string a)^","^(to_string b)^")"
  | And( a,b) ->"And("^(to_string a)^","^(to_string b)^")"
  | Or( a,b) ->"Or("^(to_string a)^","^(to_string b)^")"
  | Div( a,b) ->"Div("^(to_string a)^","^(to_string b)^")"
(*     | Add( a,b) ->"Add("^(to_string a)^","^(to_string b)^")"
 *)    | Times( a,b) ->"Times("^(to_string a)^","^(to_string b)^")"
    | Equals( a,b) ->"Equals("^(to_string a)^","^(to_string b)^")"
    | Minus( a,b) ->"Minus("^(to_string a)^","^(to_string b)^")"
    | Union( a,b) ->"Union("^(to_string a)^","^(to_string b)^")"
    | Intersection( a,b) ->"Intersection("^(to_string a)^","^(to_string b)^")"
    | In( a,b) ->"In("^(to_string a)^","^(to_string b)^")"
    |Parant(e) -> "Parant("^(to_string e)^")"
| Complement a -> "Complement("^(to_string a)^")"
| Not a -> "Not("^(to_string a)^")"
 | Setminus( a,b) ->"Setminus("^(to_string a)^","^(to_string b)^")"
  | Subset( a,b) ->"Subset("^(to_string a)^","^(to_string b)^")"
  | Empty ->"Empty"
  | List(list)->"List["^(String.concat ";" (List.map to_string list))^"]"
  | Exists(a, b) -> "Exists("^(to_string a)^","^(to_string b)^")"
    | Forall(a, b) -> "Forall("^(to_string a)^","^(to_string b)^")"