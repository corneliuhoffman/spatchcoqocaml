val replacelist : string -> (string * string) list -> string
val cleanstr : string -> string
val cleanforsearch : string -> string
val makevar : string -> string
val makeregexp : string -> Pcre.regexp
val checkinput : string -> string list -> bool
val get_tactic : string -> string list list -> string list
val get_values : string -> string list list -> string array
val prepareforxml : string -> string
val prepareforprint : string -> string
val separate : string -> string
val addtext : string -> string -> string