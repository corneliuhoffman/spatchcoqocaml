open Gobject.Data;;
open GMain;;
open GdkKeysyms;;
let locale = GtkMain.Main.init ();;
type 'a tree = LEAF of 'a 
| TREE of  'a*('a tree list);; 
let tree = TREE ("1", [TREE ("2", [LEAF "4"; LEAF "5"]); LEAF "6"]);;

open Gobject.Data;;
let cols = new GTree.column_list;;
let col_text = cols#add string;;
let treestore = GTree.tree_store cols;;


(* let rec add_tree (model:GTree.tree_store) iter tree =
  match tree with
  LEAF x -> let newiter = model#append ~parent:iter () in 
                  model#set ~row:newiter ~column:col_text x; ()
  |TREE (x, list) -> let newiter = model#append ~parent:iter () in 
                  model#set ~row:newiter ~column:col_text x;
                  List.map (fun a -> add_tree model newiter a ) list; ();; *)
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
  let window = GWindow.window ~title:"Treeview" ~border_width:10 () in
  window#connect#destroy ~callback:(fun () -> window#set_destroy_with_parent true ;GMain.Main.quit());
  let view = GTree.view  ~packing:window#add () in
  let cell_renderer = GTree.cell_renderer_text [`WEIGHT `BOLD] in
  let col = GTree.view_column ~title:"Goal"
      ~renderer:(cell_renderer, ["text", col_text]) () in
  (* pack tree view column into tree view *)
  view#append_column col;view#set_model (Some (treestore#coerce));
  view#selection#set_mode `NONE;
  window#show ()
  ;;


let create_and_fill_model data =
  let treestore = GTree.tree_store cols in
  let parent = treestore#append () in
  let parent1 = treestore#append () in
  treestore#set ~row:parent ~column:col_text "top\n and so \n on";
    
  let append (first_name, row) =
    
    let h= treestore#append ~parent:row () in
    treestore#set ~row:h ~column:col_first_name "fst";
    let toplevel = treestore#append ~parent:h () in
    treestore#set ~row:toplevel ~column:col_first_name first_name;
    let toplevel = treestore#append ~parent:h () in
    treestore#set ~row:toplevel ~column:col_first_name first_name;
    
  in
  List.iter append [
    ("Maria\n hellene\n----\n si", parent);
    ("Jane", parent);
    ("Janinita", parent)
]; 
List.iter append [
    ("Maria", parent1)
    
];treestore;;
let age_cell_data_func renderer (model:GTree.model) iter =
  let year_now = 2004 in
  let year_born = model#get ~row:iter ~column:col_year_born in
  if year_born <= year_now && year_born > 0
  then (
    let age = year_now - year_born in
    renderer#set_properties [
      `TEXT (Printf.sprintf "%u years old" age);
      `FOREGROUND_SET false
    ]
  ) else (
    renderer#set_properties [
      `TEXT "age unknown";
      `FOREGROUND "Red";
      `FOREGROUND_SET true;
] );;
let create_view_and_model ~packing () =
  let view = GTree.view ~packing () in
  (* Column #1 *)
  (* pack cell renderer into tree view column *)
  (* connect ’text’ property of the cell renderer to
   * model column that contains the first name *)
  let col = GTree.view_column ~title:"First Name"
      ~renderer:(GTree.cell_renderer_text [], ["text", col_first_name]) () in
  (* pack tree view column into tree view *)
  view#append_column col;

  let model = create_and_fill_model () in
  view#set_model (Some (model#coerce));
  view#selection#set_mode `NONE;
  view;;
let main () =
  let window = GWindow.window ~title:"Treeview" ~border_width:10 () in
  window#connect#destroy ~callback:GMain.Main.quit;
  let view = create_view_and_model ~packing:window#add () in
  window#show ();
  GMain.Main.main ();;
let _ = Printexc.print main ();;
