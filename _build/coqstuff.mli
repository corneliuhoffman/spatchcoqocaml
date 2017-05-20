val newparse : string -> Soup.soup Soup.node
(** takes a string and produces a soup node*)
val readFromCoq : in_channel -> unit -> bytes
val stringReadFromCoq : in_channel -> unit -> bytes
(** reads from the channel in *)
val readytoread : out_channel -> in_channel -> bool
val nonblockread : out_channel -> in_channel -> bytes option
(** for windows, reads without blocking*)
val repeatreading : out_channel -> in_channel -> bytes
(** also for windows reads untill it finishes *)
val get_opsys : unit -> string
(**  finds the operating system*)
val isMac : unit -> bool
(** checks if it is a mac *)
val isLinux : unit -> bool
(** checks if it is a  Linux machine*)
val isWin : unit -> bool
(** checks if it is a wubdows machine *)
val getmessages :
  out_channel ->
  in_channel -> Soup.soup Soup.node list -> Soup.soup Soup.node list

(** gets the messages as a soup mode list *)
val mygoal : out_channel -> in_channel -> bytes -> bytes
(** appends  the goal as a bytestring starting with the string, helper for soupgoal *)
val soupgoal : out_channel -> in_channel -> unit -> Soup.soup Soup.node
(** this reads the goals form coq*)
val readnow : out_channel -> in_channel -> string -> string
(** not used *)
val evars : out_channel -> unit -> unit
(** sends the evars command to coq*)
val status : out_channel -> unit -> unit
(** sends the status command to coq*)
val soupstatus : out_channel -> in_channel -> unit -> Soup.soup Soup.node
(** reads the results of the status command *)
val writeToCoq : out_channel -> string -> string -> unit
(** writes the string to coq*)

val printAST : out_channel -> string -> unit
(**sends the printASt command to coq*)

val movebackto : out_channel -> string -> unit
(**moves back to position *)

val findstateid : out_channel -> in_channel -> string -> string
(** helper function*)
val fstid : out_channel -> in_channel -> string -> string
(**finds the state id after the string *)


