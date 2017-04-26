open GMain
open GdkKeysyms
open Str
open String


open Soup

let id=ref "1";;
let oldid=ref "1";;
type mainthing ={ mutable ids : string list; mutable goals :string list list};;
let coqstr = ref "";;

let checkforcoq () =
  if Coqstuff.isMac () then
    (Printf.printf "exec %s \n\n\n" (Unix.getcwd ());
     Unix.chdir ((Unix.getcwd ())^"/Applications/spatchcoq.app/Contents/MacOS")) else ();

  if Sys.file_exists "hello.txt" then  

    (Printf.printf " here is my file %s \n\n\n" Filename.current_dir_name;
     Printf.printf "is inplicit %s\n\n\n"  (Unix.getcwd ());
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
Printf.printf "the file is %s is in full?" !coqstr;;
let oc,ic,ec  = Unix.open_process_full (!coqstr^" -ideslave -main-channel stdfds") (Unix.environment ());;
if (not (Coqstuff.isWin ())) then (
  set_binary_mode_in oc;
  Unix.set_nonblock (Unix.descr_of_in_channel oc))
else ();;

let imports =["Require Import  Bool."; "Require Import  Arith.";  "Require Import ZArith."; "Require Import Classical.";  "Require Import Utf8." ] in
List.map (fun x-> 
    Printf.fprintf ic "%s"  (Processinputs.addtext x(Coqstuff.fstid ic oc  !id)) ;flush_all ();
    Printf.printf "%s" (to_string (Coqstuff.soupgoal ic oc  ())); id:=Coqstuff.fstid ic oc  !id) imports;;

List.map (fun x-> 
    Printf.printf "command %s\n\n " (Processinputs.addtext (snd x) !id);
    Printf.printf "%s" !id;
    Printf.fprintf ic "%s"  (Processinputs.addtext (snd x) (Coqstuff.fstid ic oc  !id)) ;flush_all ();
    Printf.printf "%s" (to_string (Coqstuff.soupgoal ic oc  ())); id:=Coqstuff.fstid ic oc  !id) Commands.commands;;

Printf.printf "the list was %i long and the id is %s" (List.length Commands.commands ) !id;;

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
  List.hd (List.rev l);;
let get_first_lines str =
  let l = (Str.split (Str.regexp "\n") str) in
  String.concat "\n" (List.rev (List.tl (List.rev l)));;
let main () =
  let mainobj = {ids=[!id]; goals =[]} in
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
  win10#buffer#set_text "multi-\nline\ntext";
  win10#misc#modify_base [(`NORMAL, `NAME "lightyellow")];
  let file_ok_sel filew () =
    let file=filew#filename in
    let inp = open_in file in
    try 
      let line = input_line inp in  (* read line from in_channel and discard \n *)
      win10#buffer#set_text line; (* write on the underlying device now *)
      close_in inp;
      filew#destroy ()                 (* close the input channel *) 

    with e ->                      (* some unexpected exception occurs *)
      close_in_noerr inp;           (* emergency closing *)
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
  let tacticsfactory = new GMenu.factory menubar in
  let tactics_group = tacticsfactory#accel_group in
  let tactics_menu = tacticsfactory#add_submenu "Tactics" in
  List.map (fun x -> let y = (Processinputs.makevar (snd x)) in
             let factory = new GMenu.factory tactics_menu ~accel_group:tactics_group in
             ignore( factory#add_item y  ~callback: (fun () -> win10#buffer#set_text y)))

    Commands.commands;


  (*right window*)
  let win11 = GText.view 
      ~packing:row1#add () in
  win11#buffer#set_text "";
  (*third row*)
  let row2 = GPack.hbox ~height:20 ~homogeneous:true ~packing:vbox#pack () in
  (* run Button *)



  let buttonrun = GButton.button ~label:"run!"
      ~packing:row2#add () in
  ignore (buttonrun#connect#clicked ~callback:(fun () -> 
      let xx=(get_first_line (win10#buffer#get_text ())) in 

      if Processinputs.checkinput xx (List.map snd Commands.commands) then 
        (begin Coqstuff.writeToCoq ic   xx !id;
          let x = Coqstuff.getmessages ic oc  [] in
          let messages =String.concat "\n\n" (List.map Processresults.printmessages  x) in
          let error =try Str.search_forward (Str.regexp "rror") messages  0 with Not_found -> -1 in
          if error <0 then (begin
              win00#buffer#set_text  ((String.trim (win00#buffer#get_text ()))^"\n"^xx);
              (* insert ~iter:(win00#buffer#get_iter `END) ("\n"^xx); *)
              win10#buffer#set_text (get_tail_lines (win10#buffer#get_text ()));
              win11#buffer#set_text messages;
              let y = Coqstuff.soupgoal ic oc  () in

              id:=Coqstuff.fstid ic oc  !id;
              mainobj.ids <- !id::mainobj.ids;
              mainobj.goals <- (Processresults.processoutput y)::mainobj.goals;
              addListToNotebook  (Processresults.processoutput  y); ()
              (* win01#buffer#set_text (String.concat "\n_________________\n" (Processresults.processoutput  y)) *) end )
          else (win11#buffer#set_text messages) end)
      else  
        GToolbox.message_box ~title:"error" ~ok:"hi" (xx^"is not an accepted tactic please check your text.")
    ));
  let buttonundo = GButton.button ~label:"undo!"
      ~packing:row2#add () in
  ignore(buttonundo#connect#clicked ~callback: (fun () ->
      let last=get_last_line (win00#buffer#get_text ()) in 
      let first=get_first_lines (win00#buffer#get_text ()) in 
      win00#buffer#set_text first;
      win10#buffer#set_text (last^"\n"^(win10#buffer#get_text ()));
      match mainobj.ids with
        h::t::l->
        Coqstuff.movebackto  ic  t;
        let y = Coqstuff.soupgoal ic oc  () in
        Printf.printf "the time %s \n\n\n %s \n\n %s"  !id t;
        id:=t;
        mainobj.ids<-t::l;
        mainobj.goals <- List.tl mainobj.goals;
        addListToNotebook  (Processresults.processoutput  y); ();
      |t::[] -> Coqstuff.movebackto  ic  t;
        let y = Coqstuff.soupgoal ic oc  () in
        Printf.printf "the time s %s \n\n %s"  !id t;
        id:=t;
        addListToNotebook  (Processresults.processoutput  y); ()
    ));
  (* undo Button *)
  (* tree Button *)


  let buttontree = GButton.button ~label:"draw tree!"
      ~packing:row2#add () in
  ignore (buttontree#connect#clicked ~callback: (fun () -> 
      let tree = Treestuff.tree_from_list (List.rev mainobj.goals) in 
      Treestuff.create_tree tree
    ));


  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()

let () = main ()