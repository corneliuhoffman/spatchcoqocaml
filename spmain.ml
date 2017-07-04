open GMain
open Gobject.Data
open GdkKeysyms
open Str
open String
open Soup
open Processresults
open GToolbox
open Treestuff
let listofcommands = Commands.commands ();;

(* List.map (fun x -> List.map print_string x; flush_all ()) listofcommands;; *)
type status = BUSY | WAITING ;;
let completionindex = ref 0
let completionlist = ref []
let id=ref "1";;
let coqstatus = ref BUSY ;;
let oldid=ref "1";;
type mainthing ={ mutable state_id: string; mutable goals :Processresults.goal list; mutable leaving_tactic: string; mutable values: string array };;
let coqstr = ref "";;

let startingTime = Unix.gettimeofday ();;
let logs:Logs.logs list ref = ref [];;
let gettime () = int_of_float(Unix.gettimeofday ()-. startingTime)
let uniq lst =
  let seen = Hashtbl.create (List.length lst) in
  List.filter (fun x -> let tmp = not (Hashtbl.mem seen x) in
                        Hashtbl.replace seen x ();
                        tmp) lst
let checkforcoq () =
  if Coqstuff.isMac () then

    Unix.chdir "/Applications/spatchcoq.app/Contents/MacOS" else ();

  if Sys.file_exists "hello.txt" then  

    (
      let coqfile= open_in "hello.txt" in 
      let line = input_line coqfile in

      coqstr:= line )
  else   
    (
      let _ = GtkMain.Main.init () in
      let filew = GWindow.file_selection ~title:"File selection" ~border_width:10  
          ~filename:"" () in
      ignore(filew#ok_button#connect#clicked ~callback:( 
        fun () -> 
          let file = filew#filename in 
          coqstr := file;
          let coqfile= open_out (Filename.current_dir_name^"/hello.txt") in 
          Printf.fprintf coqfile "%s\n" file; 
          close_out coqfile;
          filew#destroy (); 
          Main.quit ()));

      ignore(filew#cancel_button#connect#clicked ~callback:filew#destroy);
      filew#show ();
      Main.main ());;
if (Coqstuff.isLinux ()) then (coqstr:="coqtop") else 
  checkforcoq ();;

let oc,ic,ec  = Unix.open_process_full (!coqstr^" -ideslave -main-channel stdfds") (Unix.environment ());;
if (not (Coqstuff.isWin ())) then (
  set_binary_mode_in oc true;
  Unix.set_nonblock (Unix.descr_of_in_channel oc))
else ();;
coqstatus := WAITING;
let repeat = if Coqstuff.isWin () then  30 else 1 in
let imports =["Require Import Classical.";  "Require Import Utf8."; ] in
ignore(List.map (fun x-> 
    Coqstuff.writeToCoq ic   x !id;
    for i = 1 to repeat do
      ignore((List.map Processresults.printmessages  (Coqstuff.getmessages ic oc [])));
     

    done;
    id:=Coqstuff.fstid ic oc  !id; 

    Printf.printf "\n The id is %s \n" !id; flush_all ())

  imports);

let imports =["Require Import  Arith." ] in
ignore(List.map (fun x-> 
    Coqstuff.writeToCoq ic   x !id;
    for i = 1 to 100*repeat do
      ignore(List.map Processresults.printmessages  (Coqstuff.getmessages ic oc []));
     
    done;
    id:=Coqstuff.fstid ic oc  !id;
    Printf.printf "\n The id is %s \n" !id; flush_all ())

  imports);
let imports =["Require Import  ZArith." ] in
ignore(List.map (fun x-> 
    Coqstuff.writeToCoq ic   x !id;
    for i = 1 to 100*repeat do
      ignore(List.map Processresults.printmessages  (Coqstuff.getmessages ic oc []));

    done;
    id:=Coqstuff.fstid ic oc  !id;
    Printf.printf "\n The id is %s \n" !id; flush_all ())

  imports);
(*  let imports =["Local Open Scope Z_scope." ] in
    List.map (fun x-> 
      Coqstuff.writeToCoq ic   x !id;
      for i = 1 to 100*repeat do
       (List.map Processresults.printmessages  (Coqstuff.getmessages ic oc []));

    done;
      id:=Coqstuff.fstid ic oc  !id)

    imports;
*)

(* let imports =["Require Import  Bool."; "Require Import  Arith.";  "Require Import ZArith."; "Require Import Classical.";  "Require Import Utf8." ] in
   List.map (fun x-> 
    Printf.fprintf ic "%s"  (Processinputs.addtext x(Coqstuff.fstid ic oc  !id)) ;flush_all ();
    Printf.printf "%s" (to_string (Coqstuff.soupgoal ic oc  ())); id:=Coqstuff.fstid ic oc  !id) imports;;
*)
let rec sendcommand ic oc  x anid =
        Printf.fprintf ic "%s"  (Processinputs.addtext (List.nth x 1) anid) ;flush_all ();
        if int_of_string anid >= int_of_string (Coqstuff.fstid ic oc  anid) then 
         sendcommand ic oc  x anid
       else id :=  (Coqstuff.fstid ic oc  anid);

       Printf.printf "  %s\n the id is %s" (List.nth x 1) !id; flush_all () in


List.map (fun x-> sendcommand ic oc  x (Coqstuff.fstid ic oc  !id)
(* 
    Printf.fprintf ic "%s"  (Processinputs.addtext (List.nth x 1) (Coqstuff.fstid ic oc  !id)) ;flush_all ();
    id:=Coqstuff.fstid ic oc  !id;
    Printf.printf " the id is %s" !id; flush_all () *)) (listofcommands);;


let locale = GtkMain.Main.init ()

let remove_book notebook () =
  while notebook#current_page >=0 do
    notebook#remove_page notebook#current_page
  done;
  ();;


let get_first_line str =
  print_string str; flush_all ();
  match (Str.split (Str.regexp "\n") str) with
  | h::t -> h^"\n"
  |[] -> ""
let get_tail_lines str =
  match (Str.split (Str.regexp "\n") str) with
  | h::t -> String.concat "\n" t
  |[] -> ""
let get_last_line str =
  let l = (Str.split (Str.regexp "\n") str) in
  if l !=[] then List.hd (List.rev l)
  else "";;
let get_first_lines str =
  let l = (Str.split (Str.regexp "\n") str) in
  if l !=[] then String.concat "\n" (List.rev (List.tl (List.rev l)))
  else "";;
let popup x_root y_root txt =
let window = GWindow.window ~urgency_hint:true ~modal:true ~title:"Spatchcoq" () in
window#move ~x:x_root ~y:y_root;
let vbox = GPack.vbox  ~packing:window#add () in

  ignore(window#connect#destroy);

let win = GText.view ~editable:false 
      ~packing:vbox#add () in
      win#buffer#set_text txt;


window
;;
let addListToNotebook (notebook:GPack.notebook) list (win10:GText.view)= 
  remove_book notebook ();
  List.map (fun x-> 
      let text = x in
      let label = GMisc.label ~text:("Goal " ) () in
      let frame = GBin.frame ~label:"" ~width:100 ~height:75 ~border_width:10
          ~packing:(fun a-> Printf.printf "%s" (string_of_int (notebook#append_page ~tab_label:label#coerce a))) () in
      let scroll = GBin.scrolled_window 
      ~hpolicy:`ALWAYS ~vpolicy:`ALWAYS 
      ~border_width:2 
      ~packing:frame#add () in

      
let cols = new GTree.column_list in
let col_name = cols#add string in 
let col_color = cols#add string in 
let data = List.map Processresults.print_goal x.hyps  in
let conc = Processresults.print_goal x.conclusion in
      let create_model data conc =
  
  let store = GTree.list_store cols in
  let fill (name) =
    let iter = store#append () in
    store#set ~row:iter ~column:col_name name;
    store#set ~row:iter ~column:col_color "lightgray";
   
  in
List.iter fill data;
  store in 

let create_view ~model ~packing () =
  let view = GTree.view ~model ~packing () in

  (* Column #1: col_name is string column *)
  let col = GTree.view_column 
      ~renderer:(GTree.cell_renderer_text [], ["text", col_name; "background", col_color]) () in
  ignore (view#append_column col);
  view#set_headers_visible false;

  (* Column #2: col_age is int column *)
  

  view in


  let store = create_model data conc in
  let view = create_view ~model:store ~packing:scroll#add () in
  view#set_headers_visible false;
      let iter = store#append () in
    store#set ~row:iter ~column:col_name conc;
        store#set ~row:iter ~column:col_color "Orange";


      (* GText.view ~editable:false  ~packing:frame#add () in
      view#buffer#set_text text; *)
      view#misc#modify_base [(`NORMAL, `NAME "lightgray")];


let selection_changed (model:#GTree.model) selection () =
  let pr path =
    let row = model#get_iter path in
    let col= model#get ~row ~column:col_color in
    let name = model#get ~row ~column:col_name in
(*     Printf.printf "the name is %s" name;flush_all ();
 *)    ( let goal = (col = "Orange") in
      let hypname, ast =Processresults.astofstr name goal in
      let pieces = Processresults.listofstr name goal in
(*       Printf.printf "there are %i %s" (List.length pieces) (List.hd pieces);flush_all ();
 *)
    (* let x, y = Gdk.Window.get_pointer_location (Gdk.Window.root_parent ()) in
    let break = Str.split (Str.regexp ":") name in
      let nname, formula = if List.length break =1 then "", name else List.hd break, String.concat ":" (List.tl break) in 
    
 *)
let menupieces = (List.map (fun name -> `I (name, (fun () -> 

let stop = win10#buffer#get_iter `INSERT in
           win10#buffer#insert ~iter:stop ("("^name^")") )) ) pieces) in

let tactics = (List.map (fun n -> `I (n, (fun () -> 

let stop = win10#buffer#get_iter `INSERT in
let ast = (Formulaparsing.parse name) in
           win10#buffer#insert ~iter:stop (n) )) ) 
           (Formulaparsing.produce_possible_tactics_goal ast hypname goal) ) in
    GToolbox.popup_menu ~entries:(menupieces@[`S]@tactics)

               (* [`I (name, (fun () -> 

let stop = win10#buffer#get_iter `INSERT in
           win10#buffer#insert ~iter:stop ("("^name^")") )); `S] *)

            ~button:0 ~time:(Int32.of_int 0); ())

      in
  List.iter pr selection#get_selected_rows
  
in 
(* 
let view_selection_func (model:#GTree.model) path currently_selected =
  let row = model#get_iter path in
  let name = model#get ~row ~column:col_name in
  if currently_selected
  then (
    let pieces = Processresults.listofstr name in
    (* let x, y = Gdk.Window.get_pointer_location (Gdk.Window.root_parent ()) in
    let break = Str.split (Str.regexp ":") name in
      let nname, formula = if List.length break =1 then "", name else List.hd break, String.concat ":" (List.tl break) in 
    
 *)
    GToolbox.popup_menu ~entries:((List.map (fun name -> `I (name, (fun () -> 

let stop = win10#buffer#get_iter `INSERT in
           win10#buffer#insert ~iter:stop ("("^name^")") )) ) pieces)@[`S]@(List.map (fun n -> `I (n, (fun () -> 

let stop = win10#buffer#get_iter `INSERT in
           win10#buffer#insert ~iter:stop (n) )) ) 
           (Formulaparsing.produce_possible_tactics_goal (Formulaparsing.parse name)) ) )

               (* [`I (name, (fun () -> 

let stop = win10#buffer#get_iter `INSERT in
           win10#buffer#insert ~iter:stop ("("^name^")") )); `S] *)

            ~button:0 ~time:(Int32.of_int 0); ())
  else ();

  flush stdout;
  true in *)
view#selection#set_mode `SINGLE;
  view#selection#connect#changed ~callback:(selection_changed store view#selection);



(* view#selection#set_select_function (view_selection_func store);
 *)

    ) list



let runcommand ic oc (win00:GText.view)  (win10:GText.view) win11 mainobj listoftheorems (notebook:GPack.notebook)  = 
  if !coqstatus = WAITING then
    ( coqstatus:=BUSY; 
    
    let start = win10#buffer#start_iter in
    let stop = start#forward_sentence_end  in
   
     let xx = win10#buffer#get_text ~start ~stop () in
     
(*     let xx=(get_first_line (win10#buffer#get_text ())) in 
 *)    

(*     Printf.printf "|%s| \n " xx; flush_all ();
 *)
    if (try (Str.search_forward (Str.regexp "Lemma \| Theorem \| Proposition \| Axiom \| Definition") xx 0 )>=0  with _-> false) then 
    mainobj :=  [{state_id= !id; goals =[{Processresults.emptygoal with  hyps =[{name=""; content =xx}]}]; leaving_tactic=xx; values =[||]}]; 
    if Processinputs.checkinput xx (List.map (fun x -> List.hd (List.tl x)) (listofcommands )) then 
      (begin Coqstuff.writeToCoq ic  (String.trim xx) !id;
      let x =Coqstuff.reallyread ic oc  !id false in
      let ids = List.map (fun a -> try (attribute "val" (a $ "state_id")) with _-> None) x in
      let maxid =  List.fold_left (fun a b -> 
      let z = (match b with
      Some z -> int_of_string z
      |None -> -1) in  max a z ) 0 ids in
      print_int maxid;
     (*  List.map (fun a -> print_string (to_string a)) x; *)
      let messages =String.concat "\n\n" (List.map Processresults.printmessages  x) in

      let error =try Str.search_forward (Str.regexp "rror") messages  0 with Not_found -> -1 in

      if error <0 then 
        (begin

          if (try (Str.search_forward (Str.regexp "Axiom\|Definition") xx 0 )>=0  with _-> false) then
            let latextac = (Processinputs.separate xx) in
            ignore(mainobj :=  [{state_id= !id; goals =[{Processresults.emptygoal with hyps =[{name=""; content =xx}]}]; leaving_tactic=latextac; values =[||]}]; 
(*              ignore(List.map (fun x->ignore(List.map (fun a -> print_string (Processresults.print_goal a)) (List.hd x.goals).hyps)) !mainobj);flush_all ();
 *)             listoftheorems := !mainobj::!listoftheorems;


             mainobj :=  [{state_id= !id; goals =[Processresults.emptygoal]; leaving_tactic=""; values = [||]}];
             win00#buffer#set_text  ((String.trim (win00#buffer#get_text ()))^xx);
             (* insert ~iter:(win00#buffer#get_iter `END) ("\n"^xx); *)
             win10#buffer#set_text (get_tail_lines (win10#buffer#get_text ())); 
             win11#buffer#set_text messages);
          else (if (try (Str.search_forward (Str.regexp "Qed\|Admitted") xx 0 )>=0  with _-> false) then
               (listoftheorems := ({state_id= !id; goals =[Processresults.emptygoal]; leaving_tactic=""; values = [||]}::!mainobj)::!listoftheorems;

                
                mainobj :=  [{state_id= !id; goals =[Processresults.emptygoal]; leaving_tactic=""; values = [||]}];
             win00#buffer#set_text  ((String.trim (win00#buffer#get_text ()))^(String.trim xx));
(*                 set_text  ((String.trim (win00#buffer#get_text ()))^"\n"^xx);
 *)                (* insert ~iter:(win00#buffer#get_iter `END) ("\n"^xx); *)
(*                 win10#buffer#set_text (get_tail_lines (win10#buffer#get_text ()));
 *)                win10#buffer#delete start stop;
                win11#buffer#set_text messages;) 
             else (completionindex:=0;
                   completionlist:=[];

                   win00#buffer#set_text  ((String.trim (win00#buffer#get_text ()))^"\n"^xx);
                   (* insert ~iter:(win00#buffer#get_iter `END) ("\n"^xx); *)
                   win10#buffer#set_text (get_tail_lines (win10#buffer#get_text ()));
                   win11#buffer#set_text messages;
(*                    Printf.printf "here is atry for is %s\n" (Coqstuff.fstid ic oc  !id);
 *)                   let rems = List.filter (fun a -> try (attribute "val" (a $ "state_id") = Some (string_of_int maxid))  with _-> false  ) x in 
                 let strings = List.map (fun a -> Processresults.processoutput a ) rems in
(*                  Printf.printf "%s\n\n" (String.concat "\n\n======\n\n" (List.map (fun a ->String.concat "\n\n\n" (Processresults.print_goal  a)) strings)); flush_all ();
 *)                   let possible_goal = Processresults.get_a_goal x in
                   let y =  match  (Processresults.goallist possible_goal) with
                   [] -> Coqstuff.soupgoal ic oc ()
                   |_ -> possible_goal
                  in
                     
                   id:=string_of_int maxid; 
                   Printf.printf "\n the id is %s\n" !id;
                   let tac =  (Processinputs.get_tactic xx (listofcommands)) in
                   let latextac = if tac !=[] then  List.hd tac else ((Processinputs.separate xx)^"\n\n Proof: @latex{1}") in
                   let gls = Processresults.goallist y in 
                   let goals = List.map (fun a ->Processresults.manage (to_list (children a))) gls in 
                   (* let _ = List.hd !mainobj in
 *)
                   mainobj := {state_id = !id; goals=goals; leaving_tactic =latextac; values = (Processinputs.get_values xx listofcommands) }::!mainobj;

                  

                   (* mainobj.ids <- !id::mainobj.ids;
                      mainobj.goals <- (Processresults.processoutput y)::mainobj.goals; *)
                   ignore(addListToNotebook notebook (Processresults.processoutput y) win10); () ))

          (* win01#buffer#set_text (String.concat "\n_________________\n" (Processresults.processoutput  y)) *) end )
      else (Printf.printf "the after erroe id is %s\n" (Coqstuff.fstid ic oc  !id); 
      win11#buffer#set_text messages; Coqstuff.movebackto  ic !id); Printf.printf "the oldbeforeerroe is %s\n" !id; 
      Printf.printf "the after computed id is %s\n" (Coqstuff.fstid ic oc  !id);  flush_all ();
      Printf.printf "the text\n %s\n"  messages;
      id:=(Coqstuff.fstid ic oc  !id); end)
  else  
    GToolbox.message_box ~title:"error" ~ok:"Ok" (xx^"is not an accepted tactic please check your text.");
coqstatus := WAITING )  
else () ;;
let saveformarking file listoftheorems =

 let out = open_out file in
          let listoftrees=List.map ( fun thm ->
            let totaltree = Treestuff.tree_from_list_of_mains (List.rev (List.map (fun x->x.goals) thm)) (List.rev (List.map (fun x->(x.leaving_tactic, x.values)) thm)) Processresults.emptygoal in
            let rec newtree t = match t with
              LEAF a -> Printf.printf "conc = %s" (print_goal  a.conclusion); LEAF (print_goal  a.conclusion)
              
              |TREE (a, [LEAF b]) -> TREE ((String.concat "\n" (List.map print_goal  a.hyps))^"\n ==== \n "^(print_goal a.conclusion), [LEAF (print_goal  b.conclusion)])
              | TREE (a, l) -> TREE ((String.concat "\n" (List.map print_goal  a.hyps))^"\n ====\n"^(print_goal a.conclusion), List.map newtree l) in

            let tree = newtree totaltree in

            (* Treestuff.maptree (fun x->

             (String.concat "\n" (List.map print_goals  x.hyps)))  totaltree in *)
          tree
          (* Treestuff.print_tree print_string tree;flush_all (); *)
          (* print_string (Sexplib.Sexp.to_string (Treestuff.sexp_of_tree Sexplib.Std.sexp_of_string tree));flush_all (); *)
          (* print_string (Latexstuff.latex totaltree); *)
          (* Treestuff.create_tree tree *)
        ) !listoftheorems in 


          let sexp = Sexplib.Std.sexp_of_list (fun x -> Treestuff.sexp_of_tree Sexplib.Std.sexp_of_string x) listoftrees in 
          let txt = Sexplib.Sexp.to_string sexp in 
          let _=Processresults.emptygoal in
          Printf.fprintf out "%s\n" txt;
          close_out out;; 
let savelogs file logs=
   let out = open_out file in
          let txt = Logs.text_of_list logs in
          
          Printf.fprintf out "%s\n" txt;
          close_out out;; 
let savelatex file mainobj listoftheorems=
   let out = open_out file in
          let d=Processresults.emptygoal in

          let newlist = !mainobj::!listoftheorems in

          let message = List.fold_left (fun   m thm ->

              let totaltree = Treestuff.tree_from_list_of_mains (
                  List.rev (List.map (fun x->x.goals) thm)) (List.rev (List.map (fun x->(x.leaving_tactic, x.values)) thm)) d in
              m^(Latexstuff.latex totaltree))  "" (List.rev newlist)   in
          let message = Latexstuff.header^message^"\\end{document}" in 
          Printf.fprintf out "%s\n" message;
          close_out out;; 

let main () =
  let mainobj =  ref [{state_id= !id; goals =[Processresults.emptygoal]; leaving_tactic=""; values = [||]}] in
  let listoftheorems = ref [] in
  let window = GWindow.window ~width:1200 ~height:600
      ~title:"Spatchcoq" () in
  let vbox0 = GPack.vbox  ~packing:window#add () in

  ignore(window#connect#destroy ~callback:Main.quit);

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox0#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore( factory#add_item "Quit" ~key:_Q ~callback: Main.quit);
 let hbox = GPack.hbox  ~packing:vbox0#add () in
 let vbox = GPack.vbox  ~packing:hbox#add () in
 

  (* first row *)
  let row0 = GPack.hbox  ~height:300 ~homogeneous:true ~packing:vbox#pack() in
  (*left window*)
  let scroll00=GBin.scrolled_window ~border_width:10
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:row0#add () in
  let win00 = GText.view ~editable:false 
      ~packing:scroll00#add_with_viewport () in
  win00#buffer#set_text "";
  win00#misc#modify_base [(`NORMAL, `NAME "lightgreen")];
  (*right window*)
  (* let scroll01=GBin.scrolled_window ~border_width:10
     ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:row0#add () in *)
  let notebook = GPack.notebook ~tab_pos:`TOP
      ~packing:row0#add  () in

  (* let addListToNotebook  list = 
  remove_book notebook ();
  List.map (fun x-> 
      let text = x in
      let label = GMisc.label ~text:("Goal " ) () in
      let frame = GBin.frame ~label:"" ~width:100 ~height:75 ~border_width:10
          ~packing:(fun a-> Printf.printf "%s" (string_of_int (notebook#append_page ~tab_label:label#coerce a))) () in
      let scroll = GBin.scrolled_window 
      ~hpolicy:`ALWAYS ~vpolicy:`ALWAYS 
      ~border_width:2 
      ~packing:frame#add () in

let cols = new GTree.column_list in
let col_name = cols#add string in 
let data = List.map Processresults.print_goal x.hyps  in
      let create_model data =
  
  let store = GTree.list_store cols in
  let fill (name) =
    let iter = store#append () in
    store#set ~row:iter ~column:col_name name;
   
  in
  List.iter fill data;
  store in 

let create_view ~model ~packing () =
  let view = GTree.view ~model ~packing () in

  (* Column #1: col_name is string column *)
  let col = GTree.view_column 
      ~renderer:(GTree.cell_renderer_text [], ["text", col_name]) () in
  ignore (view#append_column col);

  (* Column #2: col_age is int column *)
  

  view in


  let model = create_model data in
  let view = create_view ~model ~packing:scroll#add () in

      (* GText.view ~editable:false  ~packing:frame#add () in
      view#buffer#set_text text; *)
      view#misc#modify_base [(`NORMAL, `NAME "lightgray")]) list in *)

  (* let win01 = GText.view ~editable:false
                              ~packing:scroll01#add () in
     win01#buffer#set_text "";
     win01#misc#modify_base [(`NORMAL, `NAME "lightgrey")]; *)

  (*second row*)
  let row1 = GPack.hbox ~homogeneous:true ~packing:vbox#add () in

  (*left window*)

  let scroll10=GBin.scrolled_window ~border_width:10
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:row1#add () in
  let win10 = GText.view 
      ~packing:scroll10#add () in
  win10#buffer#set_text "SearchPattern (_->_).";

  win10#misc#modify_base [(`NORMAL, `NAME "lightyellow")];
let table =GPack.table 
  ~columns:2 ~width:60 ~packing:hbox#pack () in
  

let images =[("∨",1,1); ("∧",1,2); ("→",2,1); ("∀",2,2); 
    ("∃", 1, 3); ("¬", 2, 3); ("↔", 1,4); ("≠", 2,4);
    ("∈", 1, 5); ("∩", 2, 5); ("∪", 1,6); ("⊆", 2,6);
    (" ∅ ", 1, 7); ("∁", 2, 7); ("∖", 1,8)


] in
List.map 
  (fun (a,i,j) -> 
    let button =GButton.button ~label:a () in
    table#attach ~left:i ~top:j (button)#coerce;
    ignore(button#connect#clicked 
    ~callback:(fun x ->
    let stop = win10#buffer#get_iter `INSERT in
    win10#buffer#insert a))) images;
  
  let file_ok_sel filew () =
    let file=filew#filename in
    try 
      let line =  Commands.load_file file in  (* read line from in_channel and discard \n *)
   
      win10#buffer#set_text line; (* write on the underlying device now *)
      filew#destroy ()                 (* close the input channel *) 

    with e ->                      (* some unexpected exception occurs *)
      (* emergency closing *)
      raise e;
  in


  ignore( factory#add_item "Open" ~key:_O ~callback: ( fun () ->
      let filew = GWindow.file_selection ~title:"File selection" ~border_width:10
          ~filename:"penguin.png" () in

      (* Set a handler for destroy event that immediately exits GTK. *)
      (* filew#connect#destroy ~callback:GMain.Main.quit;
      *)
      (* Connect the ok_button to file_ok_sel function *)
      ignore(filew#ok_button#connect#clicked ~callback:(file_ok_sel filew));

      (* Connect the cancel_button to destroy the widget *)
      ignore(filew#cancel_button#connect#clicked ~callback:filew#destroy);

      filew#show ()


    ));

  ignore( factory#add_item "Save" ~key:_S ~callback: ( fun () ->
      let filew = GWindow.file_selection ~title:"File selection" ~border_width:10
          ~filename:"yourfile.v" () in

      (* Set a handler for destroy event that immediately exits GTK. *)
      (* filew#connect#destroy ~callback:GMain.Main.quit;
      *)
      (* Connect the ok_button to file_ok_sel function *)
      ignore(filew#ok_button#connect#clicked ~callback:(fun () ->
          let file=filew#filename in
          let out = open_out file in
          let txt = win00#buffer#get_text () in 
          let _=Processresults.emptygoal in
          Printf.fprintf out "%s\n" txt;
          close_out out; 
          filew#destroy ()
        ));

      (* Connect the cancel_button to destroy the widget *)
      ignore(filew#cancel_button#connect#clicked ~callback:filew#destroy);

      filew#show ()


    )



    );

ignore( factory#add_item "Save for marking" ~key:_M ~callback: ( fun () ->
      let filew = GWindow.file_selection ~title:"File selection" ~border_width:10
          ~filename:"yourname.mrk" () in

      (* Set a handler for destroy event that immediately exits GTK. *)
      (* filew#connect#destroy ~callback:GMain.Main.quit;
      *)
      (* Connect the ok_button to file_ok_sel function *)
      ignore(filew#ok_button#connect#clicked ~callback:(fun () ->
          let file=filew#filename in
         saveformarking file listoftheorems; 
          filew#destroy ()
        ));

      (* Connect the cancel_button to destroy the widget *)
      ignore(filew#cancel_button#connect#clicked ~callback:filew#destroy);

      filew#show ()


    )



    );


  ignore( factory#add_item "Save latex" ~key:_L ~callback: ( fun () ->
      let filew = GWindow.file_selection ~title:"File selection" ~border_width:10
          ~filename:"yourfile.tex" () in

      (* Set a handler for destroy event that immediately exits GTK. *)
      (* filew#connect#destroy ~callback:GMain.Main.quit;
      *)
      (* Connect the ok_button to file_ok_sel function *)
      ignore(filew#ok_button#connect#clicked ~callback:(fun () ->
          let file=filew#filename in
         savelatex file mainobj listoftheorems;
          filew#destroy ()
        ));

      (* Connect the cancel_button to destroy the widget *)
      ignore(filew#cancel_button#connect#clicked ~callback:filew#destroy);

      filew#show ()


    )



    );


  ignore( factory#add_item "Save all" ~key:_A~callback: ( fun () ->
      let filew = GWindow.file_selection ~title:"File selection" ~border_width:10
          ~filename:"yourfile" () in

      (* Set a handler for destroy event that immediately exits GTK. *)
      (* filew#connect#destroy ~callback:GMain.Main.quit;
      *)
      (* Connect the ok_button to file_ok_sel function *)
      ignore(filew#ok_button#connect#clicked ~callback:(fun () ->
          let f=filew#filename in
          let file =f^string_of_int (int_of_float(Unix.gettimeofday ())) in
          let out = open_out (file^".v") in
          let txt = win00#buffer#get_text () in 
          let _=Processresults.emptygoal in
          Printf.fprintf out "%s\n" txt;
          close_out out; 


          saveformarking (file^".mrk") listoftheorems;
         savelatex (file^".tex") mainobj listoftheorems;
         savelogs (file^".log") (List.rev !logs);
          filew#destroy ()
        ));

      (* Connect the cancel_button to destroy the widget *)
      ignore(filew#cancel_button#connect#clicked ~callback:filew#destroy);

      filew#show ()


    )



    );






  let tacticsfactory = new GMenu.factory menubar in
  let tactics_group = tacticsfactory#accel_group in
  let tactics_menu = tacticsfactory#add_submenu "Tactics" in
  ignore(List.map (fun x -> let y = (Processinputs.makevar (List.hd (List.tl x))) in
             let factory = new GMenu.factory tactics_menu ~accel_group:tactics_group in
             ignore( factory#add_item y  ~callback: (fun () -> win10#buffer#set_text y ;
             logs :=  (DROP (gettime (), y))::!logs;   )))

    (listofcommands));


  (*right window*)
  let scroll11=GBin.scrolled_window ~border_width:10
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:row1#add () in
  let win11 = GText.view 
      ~packing:scroll11#add () in
  win11#buffer#set_text "";



  let t1:GToolbox.key_combination = [`C],'r' in 
  let aa : unit GToolbox.shortcut_specification ={name= "forw"; keys = [t1]; message= ()} in 
  GToolbox.create_shortcuts 
    ~window:window ~shortcuts:[aa] 
    ~callback:(fun () -> 
        ( let buffer = win10#buffer in
          let _= String.trim (get_first_line (buffer#get_text ())) in
          let a = buffer#start_iter#forward_search "VAR" in
          match a with
            Some (a, b) -> buffer#delete a b; 
            buffer#place_cursor ~where:a
          |None ->()
        ));
  let t2:GToolbox.key_combination = [`C],'n' in 
  let a2 : unit GToolbox.shortcut_specification ={name= "runs"; keys = [t2]; message= ()} in 
  GToolbox.create_shortcuts 
    ~window:window ~shortcuts:[a2] 
    ~callback:(fun () -> 
      let before = (win00#buffer#get_text ())^"\n++++++\n"^(win10#buffer#get_text ()) in
       
        runcommand ic oc win00 win10 win11 mainobj listoftheorems notebook;  
      let after = (win00#buffer#get_text ())^"\n++++++\n"^(win10#buffer#get_text ()) in
      let messages = (win11#buffer#get_text ()) in
      logs :=  (CRUN (gettime (), before, after, messages ))::!logs;


      ());



  (*third row*)
  let row2 = GPack.hbox ~height:20 ~homogeneous:true ~packing:vbox#pack () in
  (* run Button *)
  let _ = window#event#connect#key_press ~callback:begin fun ev ->
      let _ = GdkEvent.Key.state ev in
      let k = GdkEvent.Key.keyval ev in
      if ( k = GdkKeysyms._Escape) then 
        (let xx= String.trim (get_first_line (win10#buffer#get_text ())) in

         let justcommands = List.map (fun x ->  
             (Processinputs.makevar (List.hd (List.tl x)))) listofcommands in
          completionlist := List.filter (fun x->  Pcre.pmatch ~rex:(Pcre.regexp xx) x) justcommands; 
          GToolbox.popup_menu ~entries:(List.map 
            (fun a -> `I (a, (fun () -> 
              let stop = win10#buffer#get_iter `INSERT in
              let start = win10#buffer#start_iter in(*  stop#backward_line in *)
              win10#buffer#delete ~start ~stop;
            win10#buffer#insert a) )) !completionlist)
             ~button:0 ~time:(Int32.of_int 0) ); 



         (* if !completionlist = [] then
           completionlist := List.filter (fun x->  Pcre.pmatch ~rex:(Pcre.regexp xx) x) justcommands; 
            GToolbox.popup_menu ~entries:(List.map 
            (fun a -> `I (a, (fun () -> 
              let stop = win10#buffer#get_iter `INSERT in
              let start = stop#backward_sentence_start in
              win10#buffer#delete ~start ~stop;
            win10#buffer#insert a) )) completionlist)
             ~button:0 ~time:(Int32.of_int 0) 
         else (
          let stop = win10#buffer#get_iter `INSERT in
        let start = stop#backward_sentence_start in
        win10#buffer#delete ~start ~stop;
           win10#buffer#insert  (List.nth !completionlist (!completionindex mod (List.length !completionlist))); 
           let before = win10#buffer#get_text () in
           logs :=  (ESC (gettime (), before))::!logs;
           completionindex:=(1 + !completionindex) mod  (List.length !completionlist)))
      else 
        completionlist:=[]; *)

      false
    end in
  window#event#add [`KEY_PRESS];


  let buttonrun = GButton.button ~label:"run!"
      ~packing:row2#add () in
  ignore (buttonrun#connect#clicked ~callback:(fun () -> 
      let before = (win00#buffer#get_text ())^"\n++++++\n"^(win10#buffer#get_text ()) in
       
        runcommand ic oc win00 win10 win11 mainobj listoftheorems notebook;  
      let after = (win00#buffer#get_text ())^"\n++++++\n"^(win10#buffer#get_text ()) in
      let messages = (win11#buffer#get_text ()) in
      logs :=  (RUN (gettime (), before, after, messages ))::!logs;
      ()););
  let buttonundo = GButton.button ~label:"undo!"
      ~packing:row2#add () in
  ignore(buttonundo#connect#clicked ~callback: (fun () ->
    let before = (win00#buffer#get_text ())^"\n++++++\n"^(win10#buffer#get_text ()) in
       
      completionindex:=0; 
      completionlist:=[];

      let stop = win00#buffer#end_iter in
      let start = stop#backward_line in
(*       let last = win00#buffer#get_text ~start ~stop () in
 *)      let last=get_last_line (win00#buffer#get_text ()) in 
 win00#buffer#set_text (get_first_lines (win00#buffer#get_text ()));
(*    win00#buffer#delete ~start ~stop;
(*  *)      print_string ("the string is"^(String.trim last)^"ha"); flush_all ();
 *)      if (String.trim last) ="" then ()
      else
        win10#buffer#set_text ((String.trim last)^"\n"^(win10#buffer#get_text ()));
      let after = (win00#buffer#get_text ())^"\n++++++\n"^(win10#buffer#get_text ()) in
      let messages = (win11#buffer#get_text ()) in
      logs :=  (UNDO (gettime (), before, after, messages ))::!logs;
      id := 
      (if win00#buffer#get_text () = "" then
       "33"
      else    

      (* let first=get_first_lines (win00#buffer#get_text ()) in 
      win00#buffer#set_text first; *)
      
      

      match !mainobj with
     

        h::t::l->
        let newid = Coqstuff.fstid  ic oc !id in
       
        mainobj:=t::l;
        t.state_id
     
      (* 
        let x = Coqstuff.reallyread ic oc  !id true in

        let possible_goal = Processresults.get_a_goal x in
        let y =  match  (Processresults.goallist possible_goal) with
                   [] -> Coqstuff.soupgoal ic oc ()
                   |_ -> possible_goal
                  in
        
        
        mainobj:=t::l;
        ignore(addListToNotebook notebook (Processresults.processoutput  y) win10); () *)
      |t::[] -> 

        (mainobj:=[];
          match !listoftheorems with
          []-> "33"
          |h::t ->(List.hd h).state_id)
      |[] -> 
          (match !listoftheorems with
          []-> "33"
          |h::t->
           (listoftheorems:=t;
           mainobj := List.tl h;
           (List.hd h).state_id))); 
      Coqstuff.movebackto  ic !id;
      print_string !id;
      let y =  Coqstuff.soupgoal ic oc () in 
   (*  let x = Coqstuff.reallyread ic oc  !id false in
      

        let possible_goal = Processresults.get_a_goal x in
        let y =  match  (Processresults.goallist possible_goal) with
                   [] -> Coqstuff.soupgoal ic oc ()
                   |_ -> possible_goal
                  in *)
      ignore(addListToNotebook notebook (Processresults.processoutput  y) win10); ()    

      ));
  (* undo Button *)
  (* tree Button *)


  let buttontree = GButton.button ~label:"draw tree!"
      ~packing:row2#add () in
  ignore (buttontree#connect#clicked ~callback:(fun () -> 
      let d=Processresults.emptygoal in
      let newlist = !mainobj::!listoftheorems in
      ignore (List.map ( fun thm ->
          (*       List.map (fun x->  (List.map (fun a ->print_string (Processresults.print_goals a)) x.goals); print_string x.leaving_tactic) thm;flush_all (); 
          *)      let totaltree = Treestuff.tree_from_list_of_mains (List.rev (List.map (fun x->x.goals) thm)) (List.rev (List.map (fun x->(x.leaving_tactic, x.values)) thm)) d in

          (* let listofstrings = (List.rev (List.map (fun x-> (List.map Processresults.print_goals x.goals)) !mainobj)) in
          *)
          (* let tree = Treestuff.tree_from_list listofstrings "done" in  *)
          let tree = Treestuff.maptree Processresults.print_goals totaltree in
          
          (* Treestuff.print_tree print_string tree;flush_all (); *)
          print_string (Sexplib.Sexp.to_string (Treestuff.to_sexp Sexplib.Std.sexp_of_string tree));flush_all ();
          (* print_string (Latexstuff.latex totaltree); *)
          Treestuff.create_tree tree
        ) newlist ); ()) );

let row3 = GPack.hbox ~height:20 ~homogeneous:true ~packing:vbox#pack () in
let textwin = GEdit.entry ~text:"?" ~packing:row3#pack () in
let buttontree = GButton.button ~label:"SearchPattern"
      ~packing:row3#add () in
  ignore (buttontree#connect#clicked ~callback:(fun () -> 
    let txt = win10#buffer#get_text () in
    win10#buffer#set_text ("SearchPattern ("^textwin#text^").\n"^txt);
runcommand ic oc win00 win10 win11 mainobj listoftheorems notebook;  

    () ));
  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()

let () = main ()