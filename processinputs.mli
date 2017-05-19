val replacelist : string -> (string * string) list -> string
(** replacelist str [(a1,b1); (a2,b2) ....] replaces ai by bi in str *)
val cleanstr : string -> string
(** replaces empty spaces *)
val cleanforsearch : string -> string
(** replaces empty spaces and "" *)
val makevar : string -> string
(**makes the tactic notation into a string with VAR's *)
val makeregexp : string -> Pcre.regexp
(** creates a regular expression from a makevar string*)
val checkinput : string -> string list -> bool
(** check if the commands is a valid tactic*)
val get_tactic : string -> string list list -> string list
(** find the details of the tactic *)
val get_values : string -> string list list -> string array
(**  gets the variables in the c=tcatic*)

val prepareforxml : string -> string
(** replaces > and such*)
val prepareforprint : string -> string
(** changes things to UTF form*)
val separate : string -> string
(** gets the text of theorem*)

val addtext : string -> string -> string
(** makes the add text command for coq*)