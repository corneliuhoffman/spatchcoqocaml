type 'a tree = LEAF of 'a | TREE of 'a * 'a tree list
(** this is the type of a tree, a tree is either a leaf or a tree with a top and children *)

val print_tree : ('a -> unit) -> 'a tree -> unit
(** this is a printout of a tree to a string *)

val get_leaves : 'a tree -> 'a list
(** this takes a tree and gets a list of its leaves *)	

val addtree : 'a tree -> 'a tree -> 'a tree
(**recursive map that adds tree 2 to the leaf of tree 1 that matches its head *)

val add_formated_list : 'a * 'a list -> 'a tree -> 'a tree
(** it takes a tree and a pair (x , list) and appends list as  leaves to the leaf that looks like x  *)


val add_list : 'a list -> 'a tree -> 'a -> 'a tree
(** takes a list and adds it to the tree as follows
a) finds the one leaf that does not appear in list
b) finds the elements of the list that are not leaves of tree 
c) attaches the new elements to the deissapeared tree.
d) if  nobody dissapears nothing is changed
e) if no new elements then add a leaf called "done" *)

val add_list_withlatex :
  Processresults.goal list ->
  Processresults.goal tree ->
  string * string array -> Processresults.goal -> Processresults.goal tree


val tree_from_list : 'a list list -> 'a -> 'a tree
(** makes a tree from a list as above *)

val tree_from_list_of_mains :
  Processresults.goal list list ->
  (string * string array) list ->
  Processresults.goal -> Processresults.goal tree

val maptree : ('a -> 'b) -> 'a tree -> 'b tree
(** this maps a three through teh function f *)
val add_tree :
  GTree.tree_store -> 'a GTree.column -> Gtk.tree_iter -> 'a tree -> unit
(** this adds a tree to a GTree model starting at the iter in the column x *)

val create_tree : string tree -> unit
(** this makes the view of a tree *)
