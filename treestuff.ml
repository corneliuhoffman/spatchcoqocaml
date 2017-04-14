open Gobject.Data;;
open GMain;;
open GdkKeysyms;;


type 'a tree = LEAF of 'a 
| TREE of  'a*('a tree list);; 

let  rec print_tree  tree = match tree with 
	LEAF x -> Printf.printf "%s" x
	|TREE (x, list) -> Printf.printf "%s\n" x ; List.map print_tree list; ();;
let rec get_leaves tree = 
match tree with 
LEAF x -> [x]
|TREE (x, list) ->
 List.concat (List.map get_leaves  list);;



let rec addtree tree2 tree1 = 
	match tree2 with 
	LEAF x -> tree1
	|TREE (x,list2) -> match tree1 with 
	LEAF a -> if a=x then tree2 else tree1 
	| TREE (a, list1) -> 
	if a=x then TREE (a, (list1@list2))  else
	TREE (a, List.map (fun x-> addtree tree2 x) list1);;
(* let rec map_tree f tree =
	match tree with 
	|LEAF x -> LEAF f x
	|TREE (a, list) -> TREE ((f a), List.map f list);;
 *)

(* it takes a tree and a pair (x , list) and appends list as  leaves to the leaf that looks like x  *)
let rec add_formated_list (x, list) tree = 
	match tree with
	LEAF a -> if a != x  then tree else (TREE (a, List.map (fun b-> LEAF b) list))
	|TREE (a, tails) -> 
	TREE (a, List.map (fun a-> add_formated_list (x, list) a) tails );;

let add_list list tree =
	let leaves = (get_leaves tree) in
	let deadleaves = List.filter (fun a -> ((not (List.mem a list)) && (not (a = "done")))) leaves in
	let newleaves = List.filter (fun a-> not (List.mem a leaves)) list in
	if deadleaves =[] then tree else
	let head = List.hd  in
	if newleaves=[] then add_formated_list (List.hd deadleaves, ["done"]) tree else
	add_formated_list (List.hd deadleaves, newleaves) tree;;

let tree_from_list list=
List.fold_left (fun x y -> add_list y x) (LEAF (List.hd (List.hd list))) (List.tl list);; 


let rec add_tree (model:GTree.tree_store) col iter tree =
  match tree with
  LEAF x -> let newiter = model#append ~parent:iter () in 
                  model#set ~row:newiter ~column:col x; ()
  |TREE (x, list) -> let newiter = model#append ~parent:iter () in 
                  model#set ~row:newiter ~column:col x;
                  List.map (fun a -> add_tree model col newiter a ) list; ();;                

let create_tree (t:string tree) =
  let cols = new GTree.column_list in
  let col_text = cols#add string in
  let treestore = GTree.tree_store cols in
  let parent = treestore#append () in
  add_tree treestore col_text parent t ;
  let window = GWindow.window ~title:"Treeview" ~width:150 ~height:150 ~border_width:10 () in
  window#connect#destroy ~callback:(fun () -> window#set_destroy_with_parent true );
  let view = GTree.view  ~packing:window#add () in
  let cell_renderer = GTree.cell_renderer_text [`WEIGHT `BOLD; `BACKGROUND "lightgray"] in
  let col = GTree.view_column ~title:"Goal"
      ~renderer:(cell_renderer, ["text", col_text]) () in
  (* pack tree view column into tree view *)
  view#append_column col;view#set_model (Some (treestore#coerce));
  view#selection#set_mode `NONE;
  window#show ()
  ;;