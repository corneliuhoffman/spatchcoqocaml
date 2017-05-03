open GMain
open GdkKeysyms
open Str
open String


open Soup
let listofcommands = Commands.commands ();;

(* List.map (fun x -> List.map print_string x; flush_all ()) listofcommands;; *)

let completionindex = ref 0
let completionlist = ref []
let id=ref "1";;
let oldid=ref "1";;
type mainthing ={ mutable state_id: string; mutable goals :Processresults.goal list; mutable leaving_tactic: string; mutable values: string array };;
let coqstr = ref "";;








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
      let locale = GtkMain.Main.init () in
      let filew = GWindow.file_selection ~title:"File selection" ~border_width:10  
          ~filename:"" () in
      filew#ok_button#connect#clicked ~callback:( 
        fun () -> 
          let file = filew#filename in 
          coqstr := file;
          let coqfile= open_out (Filename.current_dir_name^"/hello.txt") in 
          Printf.fprintf coqfile "%s\n" file; 
          close_out coqfile; 
          Main.quit ());

      filew#cancel_button#connect#clicked ~callback:filew#destroy;
      filew#show ();
      Main.main ());;
if (Coqstuff.isLinux ()) then (coqstr:="coqtop") else 
  checkforcoq ();;

let oc,ic,ec  = Unix.open_process_full (!coqstr^" -ideslave -main-channel stdfds") (Unix.environment ());;
if (not (Coqstuff.isWin ())) then (
  set_binary_mode_in oc;
  Unix.set_nonblock (Unix.descr_of_in_channel oc))
else ();;

let repeat = if Coqstuff.isWin () then  30 else 1 in
let imports =["Require Import Classical.";  "Require Import Utf8."; ] in
List.map (fun x-> 
          Coqstuff.writeToCoq ic   x !id;
          for i = 1 to repeat do
         (List.map Processresults.printmessages  (Coqstuff.getmessages ic oc []));
         print_int i;
         
        done;
          id:=Coqstuff.fstid ic oc  !id; 
    
    Printf.printf "\n The id is %s \n" !id; flush_all ())

     imports;

  let imports =["Require Import  Arith." ] in
List.map (fun x-> 
          Coqstuff.writeToCoq ic   x !id;
          for i = 1 to 100*repeat do
           (List.map Processresults.printmessages  (Coqstuff.getmessages ic oc []));
         
        done;
          id:=Coqstuff.fstid ic oc  !id)

     imports;



(* let imports =["Require Import  Bool."; "Require Import  Arith.";  "Require Import ZArith."; "Require Import Classical.";  "Require Import Utf8." ] in
List.map (fun x-> 
    Printf.fprintf ic "%s"  (Processinputs.addtext x(Coqstuff.fstid ic oc  !id)) ;flush_all ();
    Printf.printf "%s" (to_string (Coqstuff.soupgoal ic oc  ())); id:=Coqstuff.fstid ic oc  !id) imports;;
 *)
List.map (fun x-> 
    
    Printf.fprintf ic "%s"  (Processinputs.addtext (List.hd (List.tl x)) (Coqstuff.fstid ic oc  !id)) ;flush_all ();
    id:=Coqstuff.fstid ic oc  !id;
    Printf.printf " the id is %s" !id; flush_all ()) (listofcommands);;
    

let locale = GtkMain.Main.init ()

let remove_book notebook () =
  while notebook#current_page >=0 do
    notebook#remove_page notebook#current_page
  done;
  ();;


let get_first_line str =
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

  let addListToNotebook (notebook:GPack.notebook) list = 
    remove_book notebook ();
    List.map (fun x-> 
        let text = x in
        let label = GMisc.label ~text:("Goal " ) () in
        let frame = GBin.frame ~label:"" ~width:100 ~height:75 ~border_width:10
            ~packing:(fun x-> Printf.printf "%s" (string_of_int (notebook#append_page ~tab_label:label#coerce x))) () in
        let view = GText.view ~editable:false  ~packing:frame#add () in
        view#buffer#set_text text;
        view#misc#modify_base [(`NORMAL, `NAME "lightgray")]) list;;



let runcommand ic oc (win00:GText.view)  (win10:GText.view) win11 mainobj listoftheorems (notebook:GPack.notebook)  = 
      id:=Coqstuff.fstid ic oc  !id; 
      let xx=(get_first_line (win10#buffer#get_text ())) in 
      print_string ("the id is now "^(!id));

      if (try (Str.search_forward (Str.regexp "Lemma\|Theorem\|Proposition\|Axiom\|Definition") xx 0 )>=0  with _-> false) then 
        mainobj :=  [{state_id= !id; goals =[{Processresults.emptygoal with hyps =[{name=""; content =xx}]}]; leaving_tactic=xx; values =[||]}]; 
      if Processinputs.checkinput xx (List.map (fun x -> List.hd (List.tl x)) (listofcommands )) then 
        (begin Coqstuff.writeToCoq ic   xx !id;
          let x = Coqstuff.getmessages ic oc  [] in
          List.map (fun a -> print_string (to_string a)) x;
          let messages =String.concat "\n\n" (List.map Processresults.printmessages  x) in
          let error =try Str.search_forward (Str.regexp "rror") messages  0 with Not_found -> -1 in
          if error <0 then 
          (begin
            if (try (Str.search_forward (Str.regexp "Axiom\|Definition") xx 0 )>=0  with _-> false) then
        let latextac = (Processinputs.separate xx) in
        (mainobj :=  [{state_id= !id; goals =[{Processresults.emptygoal with hyps =[{name=""; content =xx}]}]; leaving_tactic=latextac; values =[||]}]; 
              List.map (fun x->List.map (fun a -> print_string (Processresults.print_goal a)) (List.hd x.goals).hyps) !mainobj;flush_all ();
                listoftheorems := !mainobj::!listoftheorems;


               
              mainobj :=  [{state_id= !id; goals =[Processresults.emptygoal]; leaving_tactic=""; values = [||]}];
              win00#buffer#set_text  ((String.trim (win00#buffer#get_text ()))^"\n"^xx);
              (* insert ~iter:(win00#buffer#get_iter `END) ("\n"^xx); *)
              win10#buffer#set_text (get_tail_lines (win10#buffer#get_text ()));
              win11#buffer#set_text messages;)
            else (if (try (Str.search_forward (Str.regexp "Qed") xx 0 )>=0  with _-> false) then
              (listoftheorems := ({state_id= !id; goals =[Processresults.emptygoal]; leaving_tactic=""; values = [||]}::!mainobj)::!listoftheorems;
             
              print_string (!id^"\n");
              mainobj :=  [{state_id= !id; goals =[Processresults.emptygoal]; leaving_tactic=""; values = [||]}];
              win00#buffer#set_text  ((String.trim (win00#buffer#get_text ()))^"\n"^xx);
              (* insert ~iter:(win00#buffer#get_iter `END) ("\n"^xx); *)
              win10#buffer#set_text (get_tail_lines (win10#buffer#get_text ()));
              win11#buffer#set_text messages;) 
              else (completionindex:=0;
              completionlist:=[];
              win00#buffer#set_text  ((String.trim (win00#buffer#get_text ()))^"\n"^xx);
              (* insert ~iter:(win00#buffer#get_iter `END) ("\n"^xx); *)
              win10#buffer#set_text (get_tail_lines (win10#buffer#get_text ()));
              win11#buffer#set_text messages;
              let y = Coqstuff.soupgoal ic oc  () in
              id:=Coqstuff.fstid ic oc  !id; 
              let tac =  (Processinputs.get_tactic xx (listofcommands)) in
              let latextac = if tac !=[] then  List.hd tac else ((Processinputs.separate xx)^"\n\n Proof: @latex{1}") in
              let gls = Processresults.goallist y in 
              let goals = List.map (fun a ->Processresults.manage (to_list (children a))) gls in 
              let oldmain = List.hd !mainobj in
              
              

              mainobj := {state_id = !id; goals=goals; leaving_tactic =latextac; values = (Processinputs.get_values xx listofcommands) }::!mainobj;
              
              
              print_string  !id;  flush_all ();

              (* mainobj.ids <- !id::mainobj.ids;
                 mainobj.goals <- (Processresults.processoutput y)::mainobj.goals; *)
              addListToNotebook notebook (Processresults.processoutput  y); () ))

              (* win01#buffer#set_text (String.concat "\n_________________\n" (Processresults.processoutput  y)) *) end )
          else (win11#buffer#set_text messages; Coqstuff.movebackto  ic !id) end)
      else  
        GToolbox.message_box ~title:"error" ~ok:"hi" (xx^"is not an accepted tactic please check your text.")
    ;;



















let main () =
  let mainobj =  ref [{state_id= !id; goals =[Processresults.emptygoal]; leaving_tactic=""; values = [||]}] in
  let listoftheorems = ref [] in
  let window = GWindow.window ~width:1200 ~height:600
      ~title:"Spatchcoq" () in
  let vbox = GPack.vbox  ~packing:window#add () in

  ignore(window#connect#destroy ~callback:Main.quit);

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore( factory#add_item "Quit" ~key:_Q ~callback: Main.quit);

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

  let addListToNotebook  list = 
    remove_book notebook ();
    List.map (fun x-> 
        let text = x in
        let label = GMisc.label ~text:("Goal " ) () in
        let frame = GBin.frame ~label:"" ~width:100 ~height:75 ~border_width:10
            ~packing:(fun x-> Printf.printf "%s" (string_of_int (notebook#append_page ~tab_label:label#coerce x))) () in
        let view = GText.view ~editable:false  ~packing:frame#add () in
        view#buffer#set_text text;
        view#misc#modify_base [(`NORMAL, `NAME "lightgray")]) list in

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
  win10#buffer#set_text "Lemma a:1=1.";

  win10#misc#modify_base [(`NORMAL, `NAME "lightyellow")];
  
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
      filew#ok_button#connect#clicked ~callback:(file_ok_sel filew);

      (* Connect the cancel_button to destroy the widget *)
      filew#cancel_button#connect#clicked ~callback:filew#destroy;

      filew#show ()


    ));

 ignore( factory#add_item "Save" ~key:_O ~callback: ( fun () ->
      let filew = GWindow.file_selection ~title:"File selection" ~border_width:10
          ~filename:"yourfile.v" () in

      (* Set a handler for destroy event that immediately exits GTK. *)
      (* filew#connect#destroy ~callback:GMain.Main.quit;
      *)
      (* Connect the ok_button to file_ok_sel function *)
      filew#ok_button#connect#clicked ~callback:(fun () ->
    let file=filew#filename in
    let out = open_out file in
    let txt = win00#buffer#get_text () in 
    let d=Processresults.emptygoal in
    Printf.fprintf out "%s\n" txt;
    close_out out; 
    filew#destroy ()
    );

      (* Connect the cancel_button to destroy the widget *)
      filew#cancel_button#connect#clicked ~callback:filew#destroy;

      filew#show ()


    )



);

 ignore( factory#add_item "Save latex" ~key:_O ~callback: ( fun () ->
      let filew = GWindow.file_selection ~title:"File selection" ~border_width:10
          ~filename:"yourfile.tex" () in

      (* Set a handler for destroy event that immediately exits GTK. *)
      (* filew#connect#destroy ~callback:GMain.Main.quit;
      *)
      (* Connect the ok_button to file_ok_sel function *)
      filew#ok_button#connect#clicked ~callback:(fun () ->
    let file=filew#filename in
    let out = open_out file in
    let d=Processresults.emptygoal in

    let newlist = !mainobj::!listoftheorems in

    let message = List.fold_left (fun   m thm ->
      
      let totaltree = Treestuff.tree_from_list_of_mains (
        List.rev (List.map (fun x->x.goals) thm)) (List.rev (List.map (fun x->(x.leaving_tactic, x.values)) thm)) d in
      m^(Latexstuff.latex totaltree))  "" (List.rev newlist)   in
    let message = Latexstuff.header^message^"\\end{document}" in 
    Printf.fprintf out "%s\n" message;
    close_out out; 
    filew#destroy ()
    );

      (* Connect the cancel_button to destroy the widget *)
      filew#cancel_button#connect#clicked ~callback:filew#destroy;

      filew#show ()


    )



);





  let tacticsfactory = new GMenu.factory menubar in
  let tactics_group = tacticsfactory#accel_group in
  let tactics_menu = tacticsfactory#add_submenu "Tactics" in
  List.map (fun x -> let y = (Processinputs.makevar (List.hd (List.tl x))) in
             let factory = new GMenu.factory tactics_menu ~accel_group:tactics_group in
             ignore( factory#add_item y  ~callback: (fun () -> win10#buffer#set_text y)))

    (listofcommands);


  (*right window*)
  let win11 = GText.view 
      ~packing:row1#add () in
  win11#buffer#set_text "";



let t1:GToolbox.key_combination = [`C],'r' in 
let aa : unit GToolbox.shortcut_specification ={name= "forw"; keys = [t1]; message= ()} in 
GToolbox.create_shortcuts 
  ~window:window ~shortcuts:[aa] 
  ~callback:(fun () -> 
      ( let buffer = win10#buffer in
        let xx= String.trim (get_first_line (buffer#get_text ())) in
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
runcommand ic oc win00 win10 win11 mainobj listoftheorems notebook; () );


  (*third row*)
  let row2 = GPack.hbox ~height:20 ~homogeneous:true ~packing:vbox#pack () in
  (* run Button *)
let _ = window#event#connect#key_press ~callback:begin fun ev ->
   let m = GdkEvent.Key.state ev in
   let k = GdkEvent.Key.keyval ev in
   if ( k = GdkKeysyms._Escape) then 
   (let xx= String.trim (get_first_line (win10#buffer#get_text ())) in
   let justcommands = List.map (fun x ->  
     (Processinputs.makevar (List.hd (List.tl x)))) listofcommands in
   if !completionlist = [] then
   completionlist := List.filter (fun x->  Pcre.pmatch ~rex:(Pcre.regexp xx) x) justcommands 
   else (
   win10#buffer#set_text (List.nth !completionlist (!completionindex mod (List.length !completionlist))); 
    completionindex:=(1 + !completionindex) mod  (List.length !completionlist)))
   else 
              completionlist:=[];
    
   false
  end in
  window#event#add [`KEY_PRESS];


  let buttonrun = GButton.button ~label:"run!"
      ~packing:row2#add () in
  ignore (buttonrun#connect#clicked ~callback:(fun () -> 

runcommand ic oc win00 win10 win11 mainobj listoftheorems notebook; () 

));
  let buttonundo = GButton.button ~label:"undo!"
      ~packing:row2#add () in
  ignore(buttonundo#connect#clicked ~callback: (fun () ->
      completionindex:=0; 
      completionlist:=[];
      let last=get_last_line (win00#buffer#get_text ()) in 
      let first=get_first_lines (win00#buffer#get_text ()) in 
      win00#buffer#set_text first;
      win10#buffer#set_text (last^"\n"^(win10#buffer#get_text ()));
      match !mainobj with
        h::t::l->
        Coqstuff.movebackto  ic  t.state_id;
        let y = Coqstuff.soupgoal ic oc  () in
        
        id:=t.state_id;
        mainobj:=t::l;
        addListToNotebook  (Processresults.processoutput  y); ();
      |t::[] -> Coqstuff.movebackto  ic  t.state_id;
        match !listoftheorems with
        []->
        let y = Coqstuff.soupgoal ic oc  () in
        
        id:=t.state_id;
        addListToNotebook  (Processresults.processoutput  y); ()
        |h::t ->
        (
          match h with 
        |hd::tl -> 
(*         List.map Processresults.print_goals hd.goals;
 *)        
        Coqstuff.movebackto  ic  hd.state_id;
        let y = Coqstuff.soupgoal ic oc  () in
        
        id:=hd.state_id;
        mainobj:=tl;
        addListToNotebook  (Processresults.processoutput  y); ();
        listoftheorems:= t; ())
      |_-> ()
    ));
  (* undo Button *)
  (* tree Button *)


  let buttontree = GButton.button ~label:"draw tree!"
      ~packing:row2#add () in
  ignore (buttontree#connect#clicked ~callback:(fun () -> 
      let d=Processresults.emptygoal in
      let newlist = !mainobj::!listoftheorems in
      List.map ( fun thm ->
(*       List.map (fun x->  (List.map (fun a ->print_string (Processresults.print_goals a)) x.goals); print_string x.leaving_tactic) thm;flush_all (); 
 *)      let totaltree = Treestuff.tree_from_list_of_mains (List.rev (List.map (fun x->x.goals) thm)) (List.rev (List.map (fun x->(x.leaving_tactic, x.values)) thm)) d in
   
      (* let listofstrings = (List.rev (List.map (fun x-> (List.map Processresults.print_goals x.goals)) !mainobj)) in
 *)
      (* let tree = Treestuff.tree_from_list listofstrings "done" in  *)
      let tree = Treestuff.maptree Processresults.print_goals totaltree in
         Treestuff.print_tree print_string tree;flush_all ();
        print_string (Latexstuff.latex totaltree);
      Treestuff.create_tree tree
    ) newlist ; ()) );


  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()

let () = main ()