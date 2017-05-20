open GMain
open GdkKeysyms
open Str
open String


open Soup

let id=ref "1"
let oldid=ref "1"
type mainthing ={ mutable ids : string list}
(* type statement ={ name: string; content: string};;
type goal ={number: string; hyps : statement list; conclusion: statement};; *)
(* let print_goal {name=b; content= c} = 
  Printf.sprintf "%s : %s " (String.trim b) (String.trim c);;
let print_goals {number=n; hyps=h; conclusion= c} = 
  n^"\n"^(String.concat "\n" (List.map print_goal h))^"\n================\n"^(print_goal c);;   *)
let oc,ic,ec = Unix.open_process_full "/Applications/CoqIDE_8.6.app/Contents/Resources/bin/coqtop -ideslave -main-channel stdfds" (Unix.environment ());;

set_binary_mode_in oc;;
Unix.set_nonblock (Unix.descr_of_in_channel oc);;
 

 let inc = open_in "hello.txt" in
  try 
    let line = input_line inc in  (* read line from in_channel and discard \n *)
    print_endline line;          (* write the result to stdout *)
    flush stdout;                (* write on the underlying device now *)
    close_in inc                  (* close the input channel *) 
  
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr inc;           (* emergency closing *)
    raise e                      (* exit with error: files are closed but*)
              



let newparse  a = Markup.string a |> Markup.parse_xml |> Markup.signals |> from_signals;;

let  readFromCoq () = 
  let bb=Bytes.create 1000000 in 
  let i=Unix.read (Unix.descr_of_in_channel oc) bb 0 1000000 in
  Bytes.sub bb  0 i;;
let rec stringReadFromCoq n = try readFromCoq () with
any-> stringReadFromCoq();;

let rec getmessages l =
let rec sgoal () = ignore (Printf.fprintf ic "%s" "<call  val =\"Goal\"><unit/></call>\n";flush_all ());
let x = stringReadFromCoq () in
if x!="" then try newparse x with _ -> sgoal() else sgoal () in
 let x = sgoal () in 
  if (List.mem (to_string x) (List.map to_string l) ) then l else getmessages (l@[x]);;

let rec mygoal str = ignore (Printf.fprintf ic "%s" "<call  val =\"Goal\"><unit/></call>\n";flush_all ());
let x = stringReadFromCoq () in
if str = x then x else mygoal x;;

let rec soupgoal () = 
let x = mygoal "" in
try newparse x with _ -> soupgoal();;

let evars () = Printf.fprintf ic "<call val=\"Evars\"><unit/></call>\n";flush_all ();;
let status () = Printf.fprintf ic "%s" "<call val=\"Status\"><bool val=\"false\"/></call>";flush_all ();;
let rec soupstatus () = ignore (status ()); try newparse (stringReadFromCoq ()) with _ -> soupstatus();;
(* let addtext str i = "</call><call val='Add'><pair><pair><string>"^str^"</string>
<int>0</int></pair><pair><state_id val='"^i^"'/><bool val='false'/></pair></pair></call>\n";; *)
let writeToCoq str i = Printf.fprintf ic "%s"  (Processinputs.addtext str i) ;flush_all ()
   
let printAST i = Printf.fprintf ic "%s" ("<call val=\"PrintAst\"><state_id val=\""^i^"\"/></call>");flush_all ();;
let movebackto i = Printf.fprintf ic "%s" ("<call val=\"Edit_at\"><state_id val=\""^i^"\"/></call>");flush_all ();;


let rec findstateid id = match (attribute "val" ((soupstatus ()) $ "state_id")) with
   Some x -> if int_of_string x >= int_of_string id then x else findstateid id
  |_ -> findstateid id;;
let rec fstid id = try (findstateid id) with any -> (* ignore (Printf.printf "%s" (Printexc.to_string any)); *)
 fstid id;;


List.map (fun x-> 
  (* Printf.printf "command %s\n\n " (Processinputs.addtext (snd x) !id); *)
  Printf.printf "%s" !id;
  Printf.fprintf ic "%s"  (Processinputs.addtext (snd x) (Coqstuff.fstid ic oc  !id)) ;flush_all ();
  (* Printf.printf "%s" (to_string (Coqstuff.soupgoal ic oc  ())); id:=Coqstuff.fstid ic oc  !id) Commands.commands;; *)

Printf.printf "the id is now %s" !id;;

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
  let mainobj = {ids=["1"]} in
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
      (begin writeToCoq xx !id;
      let x = getmessages [] in
      let messages =String.concat "\n\n" (List.map Processresults.printmessages  x) in
      let error =try Str.search_forward (Str.regexp "rror") messages  0 with Not_found -> -1 in
      if error <0 then (begin Printf.printf "found\n";
       win00#buffer#insert ~iter:(win00#buffer#get_iter `END) xx;
       win10#buffer#set_text (get_tail_lines (win10#buffer#get_text ()));
      
      (* Printf.printf "%s" messages;
      Printf.printf "%d" error;
      Printf.printf "%s" (string_of_bool (Str.string_match (Str.regexp "rror") messages 0 )); *)
      win11#buffer#set_text messages;
      let y = soupgoal () in
     
      id:=fstid !id;
      mainobj.ids <- !id::mainobj.ids;
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
    movebackto t;
    let y = soupgoal () in
   (*  Printf.printf "%s \n\n\n %s \n\n %s" (to_string y) !oldid (fstid !oldid); *)
    id:=t;
    mainobj.ids<-t::l;
    addListToNotebook  (Processresults.processoutput  y); ()
    |_ -> ()
  ));
(* undo Button *)
  (* tree Button *)
  

  let buttontree = GButton.button ~label:"draw tree!"
                              ~packing:row2#add () in
  ignore (buttontree#connect#clicked ~callback: (fun () -> 
 GToolbox.message_box ~title:"error" ~ok:"hi" "hello" 

));


  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()

let () = main ()
