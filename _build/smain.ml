open GMain
open GdkKeysyms
open Str
open String


open Soup

let id=ref "1"
type statement ={ name: string; content: string};;
type goal ={number: string; hyps : statement list; conclusion: statement};;
let print_goal {name=b; content= c} = 
  Printf.sprintf "%s : %s " (String.trim b) (String.trim c);;
let print_goals {number=n; hyps=h; conclusion= c} = 
  n^"\n"^(String.concat "\n" (List.map print_goal h))^"\n================\n"^(print_goal c);;  
let oc,ic,ec = Unix.open_process_full "/Applications/CoqIDE_8.6.app/Contents/Resources/bin/coqtop -ideslave -main-channel stdfds" (Unix.environment ());;

set_binary_mode_in oc;;
Unix.set_nonblock (Unix.descr_of_in_channel oc);;
let newparse  a = Markup.string a |> Markup.parse_xml |> Markup.signals |> from_signals;;

let  readFromCoq () = 
  let bb=Bytes.create 1000000 in 
  let i=Unix.read (Unix.descr_of_in_channel oc) bb 0 1000000 in
  Bytes.sub bb  0 i;;
let rec stringReadFromCoq n = try readFromCoq () with
any-> stringReadFromCoq();;

(* let writeToCoq str = Printf.fprintf ic "%s" str;flush_all() *)
let goal () =Printf.fprintf ic "%s" "<call  val =\"Goal\"><unit/></call>\n";flush_all ();;
let rec soupgoal () = ignore (goal ());
let x = stringReadFromCoq () in
if x!="" then try newparse x with _ -> soupgoal() else soupgoal ();;

let cleanstr x = 
  let t = Str.regexp "&gt;\\|<_>\\|</_>\\|&\\|amp;\\|nbsp;" in
  Str.global_replace t " "  x;;
let strToStatement x = let 
a =Str.split (Str.regexp ":") (cleanstr x) in
match a with
| h::[] -> {name=""; content = h}
| h::b -> {name= h; content = String.concat ":" b}
|_->{name =""; content = ""};;

let get_texts x = String.concat "" (texts x);;

let manage li =
 match li with
h::hyp::t::[]-> 
{number =get_texts h;hyps = List.map (fun x-> strToStatement (get_texts x) )(to_list (hyp$$"_")); conclusion = strToStatement (get_texts t)}
|_ -> {number= "" ;hyps =[]; conclusion ={name=""; content =""}};;
let goallist x = 
  to_list (x$$"goal");;

let processoutput x = 
  let goals= goallist x in 
  List.map (fun a ->print_goals (manage (to_list (children a)))) goals;;
let evars () = Printf.fprintf ic "<call val=\"Evars\"><unit/></call>\n";flush_all ();;
let status () = Printf.fprintf ic "%s" "<call val=\"Status\"><bool val=\"false\"/></call>";flush_all ();;
let rec soupstatus () = ignore (status ()); try newparse (stringReadFromCoq ()) with _ -> soupstatus();;
let addtext str i = "</call><call val='Add'><pair><pair><string>"^str^"</string>
<int>0</int></pair><pair><state_id val='"^i^"'/><bool val='false'/></pair></pair></call>\n";;
let writeToCoq str i = Printf.fprintf ic "%s"  (addtext str i) ;flush_all ();;
let printAST i = Printf.fprintf ic "%s" ("<call val=\"PrintAst\"><state_id val=\""^i^"\"/></call>");flush_all ();;

(* let findvalues str = ignore (Str.search_forward (Str.regexp "<value .+</value>") str 0); Str.matched_string str

let findfeedback str = ignore (Str.search_forward (Str.regexp "<feedback .+</feedback>") str 0); Str.matched_string str
 *)
(* This finds the state id, checks if it went forward or not. *)

let rec findstateid id = match (attribute "val" ((soupstatus ()) $ "state_id")) with
   Some x -> if int_of_string x >= int_of_string id then x else findstateid id
  |_ -> findstateid id;;
let rec fstid id = try (findstateid id) with any -> ignore (Printf.printf "%s" (Printexc.to_string any)); fstid id;;
(* let writeNow st = writeToCoq st (findstateid ()) *)
(* let printgoals () = 
  let clean st = Printf.sprintf "%s" "\n"^(Str.global_replace (Str.regexp "<_>") "\n-------\n" st ) in
  let x = soupgoal () in
  let li = (List.map (texts) (to_list (x $$ "goal"))) in 
  Printf.sprintf "%s" (String.concat "\n==========\n" (List.map (fun x -> clean (String.concat "\n"  x)) li));;
 *)
let xmltostr str = 
  Str.global_replace (Str.regexp "&nbsp;") " " str;;


let printgoals x =
  let clean st = Printf.sprintf "%s" "\n"^(Str.global_replace (Str.regexp "<_>") "\n-------\n" st ) in
  let ll =List.map (select "richpp") (to_list (x$$"goal")) in 
  let newlist=List.map (function a -> List.map (String.concat "") (List.map texts (to_list a))) ll in
  String.concat "\n========================\n" (List.map (function a -> xmltostr (String.concat "\n" a)) newlist);; 
let printmessages x =
let clean st = Printf.sprintf "%s" "\n"^(Str.global_replace (Str.regexp "<_>") "\n-------\n" st ) in
  let ll =List.map (select "richpp") (to_list (x$$"message")) in 
  let newlist=List.map (function a -> List.map (String.concat "") (List.map texts (to_list a))) ll in
  String.concat "\n========================\n" (List.map (function a -> xmltostr (String.concat "\n" a)) newlist);; 
let rec record l = let x = soupgoal () in 
  if (List.mem (to_string x) (List.map to_string l) ) then l else record (l@[x]);;


let locale = GtkMain.Main.init ()

let remove_book notebook () =
  while notebook#current_page >=0 do
  notebook#remove_page notebook#current_page
done;
  ();;
let addListToNotebook notebook list =
  remove_book notebook ();
List.map (fun x-> 
let text = snd x in
let label = GMisc.label ~text:("Goal " ^ (fst x)) () in
    let frame = GBin.frame ~label:text ~width:100 ~height:75 ~border_width:10
  ~packing:(fun x-> Printf.printf "%s" (string_of_int (notebook#append_page ~tab_label:label#coerce x))) () in
let label = GMisc.label ~text ~packing:frame#add () in
()) list;;


let get_first_line str =
	match (Str.split (Str.regexp "\n") str) with
	| h::t -> h^"\n"
	|[] -> ""
let get_tail_lines str =
	match (Str.split (Str.regexp "\n") str) with
	| h::t -> String.concat "\n" t
	|[] -> ""
let main () =
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
  let scroll01=GBin.scrolled_window ~border_width:10
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:row0#add () in
  let win01 = GText.view ~editable:false
                              ~packing:scroll01#add () in
  win01#buffer#set_text "";
  win01#misc#modify_base [(`NORMAL, `NAME "lightgrey")];
  
  (*second row*)
  let row1 = GPack.hbox ~homogeneous:true ~packing:vbox#add () in
 
  (*left window*)

  let scroll10=GBin.scrolled_window ~border_width:10
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:row1#add () in
  let win10 = GText.view 
                              ~packing:scroll10#add () in
  win10#buffer#set_text "multi-\nline\ntext";
   win10#misc#modify_base [(`NORMAL, `NAME "lightyellow")];
 (*right window*)
  let win11 = GText.view 
                              ~packing:row1#add () in
  win11#buffer#set_text "";
   (*third row*)
    let row2 = GPack.hbox ~height:20 ~homogeneous:true ~packing:vbox#pack () in
  (* run Button *)
  
  
  
  let buttonrun = GButton.button ~label:"run!"
                              ~packing:row2#add () in
  ignore (buttonrun#connect#clicked ~callback: (fun () -> 
let xx=(get_first_line (win10#buffer#get_text ())) in 
    (win00#buffer#insert ~iter:(win00#buffer#get_iter `END)
  	(xx^(!id)));
  		(win10#buffer#set_text
  		(get_tail_lines (win10#buffer#get_text ())));
      
      writeToCoq xx !id;
      let x= record [] in
      Printf.printf "%s\n\n\n\n\n" (String.concat "\n\n\n" (List.map to_string x));
      let y = soupgoal () in
      id:=fstid !id;
      win11#buffer#set_text (String.concat "\n\n" (List.map printmessages  x));
      win01#buffer#set_text (String.concat "\n_________________\n" (processoutput  y))
    ));
let buttonundo = GButton.button ~label:"undo!"
                              ~packing:row2#add () in
  ignore(buttonundo#connect#clicked ~callback: (fun () -> prerr_endline (win10#buffer#get_text ()) ));
(* undo Button *)
  (* tree Button *)
  

  let buttontree = GButton.button ~label:"draw tree!"
                              ~packing:row2#add () in
  ignore (buttontree#connect#clicked ~callback: (fun () -> prerr_endline "Ouch!"));


  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()

let () = main ()