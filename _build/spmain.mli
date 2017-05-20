val listofcommands : string list list 
val id : string ref
val oldid : string ref
type mainthing = {
  mutable state_id : string;
  mutable goals : Processresults.goal list;
  mutable leaving_tactic : string;
  mutable values : string array;
}
val coqstr : string ref
val checkforcoq : unit -> unit
val oc : in_channel
val ic : out_channel
val ec : in_channel
val locale : string
val remove_book :
  < current_page : int; remove_page : int -> 'a; .. > -> unit -> unit
val get_first_line : string -> string
val get_tail_lines : string -> string
val get_last_line : string -> string
val get_first_lines : string -> string
val main : unit -> unit
