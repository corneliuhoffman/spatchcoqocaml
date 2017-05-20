open Gobject.Data;;
open GMain;;
open GdkKeysyms;;

type 'a tree = LEAF of 'a 
             | TREE of  'a*('a tree list);; 


let  rec print_tree f tree = match tree with 
    LEAF x -> print_string "LEAF\n :"; f x
  |TREE (x, list) -> print_string "HEAD\n"; f x ; print_string "TAIL:\n"; List.map (print_tree f) list; ();;


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


let rec add_formated_list (x, list) tree = 
  match tree with
    LEAF a -> if a != x  then tree else (TREE (a, List.map (fun b-> LEAF b) list))
  |TREE (a, tails) -> 
    TREE (a, List.map (fun a-> add_formated_list (x, list) a) tails );;



let add_list  list tree d =
  let leaves = (get_leaves tree) in
  let deadleaves = List.filter (fun a -> ((not (List.mem a list)) && (not (a = d)))) leaves in
  let newleaves = List.filter (fun a-> not (List.mem a leaves)) list in
  if deadleaves =[] then tree else
    let head = List.hd deadleaves in
    if newleaves=[] then add_formated_list (head, [d]) tree else
      add_formated_list (head, newleaves) tree;;


let add_list_withlatex  (list: Processresults.goal list) (tree:Processresults.goal tree) latex  d =
  let leaves = (get_leaves tree) in
  let deadleaves = List.filter (fun goal -> ((not (List.mem ((goal:Processresults.goal).number) (List.map (fun aa -> (aa:Processresults.goal).number) list))) && (not (goal = d)))) leaves in
  let newleaves = List.filter (fun a-> not (List.mem (a:Processresults.goal).number  (List.map (fun aa->(aa:Processresults.goal).number) leaves))) list in
  if deadleaves =[] then tree else
    let head = List.hd deadleaves in
    head.leaving_tactic <- fst latex;
    head.values <- snd latex;
    if newleaves=[] then add_formated_list (head, [d]) tree else
      add_formated_list (head, newleaves) tree;;



let tree_from_list list d =
  match list with
  |h::[] -> LEAF (List.hd (List.hd list))
  |_-> List.fold_left (fun x y -> add_list y x d) (LEAF (List.hd (List.hd list))) (List.tl list);; 

let tree_from_list_of_mains (list1:Processresults.goal list  list) (list2:(string * string array) list ) d  =
  match list1 with
  |h::[] -> LEAF {(List.hd (List.hd list1)) with leaving_tactic = (fst (List.hd list2))}
  |_-> List.fold_left2 (fun x y z -> add_list_withlatex y x z d) (LEAF (List.hd (List.hd list1))) (List.tl list1) (List.tl list2);; 

let rec maptree f tree =
  match tree with
    LEAF x -> LEAF (f x)
  |TREE (x, li) -> TREE ((f x), List.map (maptree f) li);;

(* this adds a tree to a GTree model starting at the iter in the column x *)

let rec add_tree (model:GTree.tree_store) col iter tree =
  match tree with
    LEAF x -> let newiter = model#append ~parent:iter () in 
    model#set ~row:newiter ~column:col x; ()
  |TREE (x, list) -> let newiter = model#append ~parent:iter () in 
    model#set ~row:newiter ~column:col x;
    List.map (fun a -> add_tree model col newiter a ) list; 
    ();;                

(* this makes the view of a tree *)

let create_tree (t:string tree) =
  let cols = new GTree.column_list in
  let col_text = cols#add string in
  let treestore = GTree.tree_store cols in
  let parent = treestore#append () in
  match t with 
    LEAF x -> 
    treestore#set ~row:parent ~column:col_text x
  |TREE (x, list)->
    (treestore#set ~row:parent~column:col_text x;
     List.map (fun x -> add_tree treestore col_text parent x) list) ; ();
    let window = GWindow.window ~title:"Treeview" ~width:400 ~height:400 ~border_width:10 () in
    window#connect#destroy ~callback:(fun () -> window#set_destroy_with_parent true );
    let scroll=GBin.scrolled_window ~border_width:10
        ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:window#add () in

    let view = GTree.view  ~packing:scroll#add_with_viewport () in
    let cell_renderer = GTree.cell_renderer_text [`WEIGHT `BOLD; `BACKGROUND "lightgray"] in
    let col = GTree.view_column ~title:"Goal"
        ~renderer:(cell_renderer, ["text", col_text]) () in
    (* pack tree view column into tree view *)
    view#append_column col;view#set_model (Some (treestore#coerce));
    view#selection#set_mode `NONE;
    window#show ()
;;