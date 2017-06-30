type statement = { name : string; content : string; }
(** A statemenet has aname and content, for example H: a>0 has name H and cotent a>0 while the conclusion a>0 has empty sptring as name and a>0 as content *)


type goal = {
  number : string;
  hyps : statement list;
  conclusion : statement;
  mutable state_id : int;
  mutable leaving_tactic : string;
  mutable values : string array;
}
(** a goal has number (generated by coq) a list of hypotheses (which are all staements) a conclusion (a statement) a state_id (if known othewise it is zero) and a leaving tactic (if known otherwise empty *)

val emptygoal : goal
(** this makes and emptygoal to be filled up later *)

val print_goal : statement -> string
(** makes a statement into a string*)

val print_goals : goal -> string
(** makes a goal into a string *)

val astofstr : string -> bool-> string*Ast.expr

val listofstr : string -> bool -> string list

val xmltostr : string -> string
(** changes the xml to string replacing &nbst;*)

val cleanstr : string -> string

(** changes the xml to replace the various stuff*)
val strToStatement : string -> statement
(** produces statements form strings , if concl the the name is ""*)
val get_texts : 'a Soup.node -> string
val manage : 'a Soup.node list -> goal
val goallist : 'a Soup.node -> Soup.element Soup.node list
val processoutput : 'a Soup.node -> goal list
val printmessages : 'a Soup.node -> string
val get_a_goal : 'a Soup.node list -> 'a Soup.node 