val listofcommands : string list list 
val id : string ref
val oldid : string ref
val listoftrees: Processresults.goal Treestuff.tree list ref
val current_tree:Processresults.goal Treestuff.tree ref
val current_head:Processresults.goal ref
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
