val newparse : string -> Soup.soup Soup.node
val readFromCoq : in_channel -> unit -> bytes
val stringReadFromCoq : in_channel -> unit -> bytes
val readytoread : out_channel -> in_channel -> bool
val nonblockread : out_channel -> in_channel -> bytes option
val repeatreading : out_channel -> in_channel -> bytes
val get_opsys : unit -> string
val isMac : unit -> bool
val isLinux : unit -> bool
val isWin : unit -> bool
val getmessages :
  out_channel ->
  in_channel -> Soup.soup Soup.node list -> Soup.soup Soup.node list
val mygoal : out_channel -> in_channel -> bytes -> bytes
val soupgoal : out_channel -> in_channel -> unit -> Soup.soup Soup.node
val readnow : out_channel -> in_channel -> string -> string
val evars : out_channel -> unit -> unit
val status : out_channel -> unit -> unit
val soupstatus : out_channel -> in_channel -> unit -> Soup.soup Soup.node
val writeToCoq : out_channel -> string -> string -> unit
val printAST : out_channel -> string -> unit
val movebackto : out_channel -> string -> unit
val findstateid : out_channel -> in_channel -> string -> string
val fstid : out_channel -> in_channel -> string -> string