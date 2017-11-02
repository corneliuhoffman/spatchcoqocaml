open Soup
let newparse  a = Markup.string a |> Markup.parse_xml |> Markup.signals |> from_signals;;

let  readFromCoq oc () = 
  let bb=Bytes.create 100000 in 
  let i=Unix.read (Unix.descr_of_in_channel oc) bb 0 100000 in
  
  Bytes.sub bb  0 i;;
let rec stringReadFromCoq oc n = try readFromCoq oc () with
    any-> ( (* print_string (Printexc.to_string any); *)  stringReadFromCoq oc ());;


let rec readytoread ic oc =
  let a, b, c = Unix.select [Unix.descr_of_in_channel oc] [Unix.descr_of_out_channel ic] [] 50.0 in
  a != [];;

let nonblockread ic oc =

  if readytoread ic oc then Some (readFromCoq oc ())
  else  ( None);;

let rec repeatreading ic oc =
  let x = try (nonblockread ic oc) with any -> ( Some (repeatreading ic oc)) in
  match x with
    Some a -> a 
  |None->   ( repeatreading ic oc);;
 

let get_opsys () =
  let ic = Unix.open_process_in "uname" in
  let uname = input_line ic in
  let () = close_in ic in
  uname;;


let isMac ()=
  (try (Str.search_forward (Str.regexp "Unix") (Sys.os_type) 0)>=0 with _-> false) &&
  (try (Str.search_forward (Str.regexp "Darw") (get_opsys ()) 0)>=0 with _-> false)
let isLinux ()=
  (try (Str.search_forward (Str.regexp "Unix") (Sys.os_type) 0)>=0 with _-> false) &&
  (try (Str.search_forward (Str.regexp "Linux") (get_opsys ()) 0)>=0 with _-> false)
let isWin ()=
  try (Str.search_forward (Str.regexp "Win") (Sys.os_type) 0)>=0 with _-> false

let rec getmessages ic oc l =
  let rec sgoal ic oc () = ignore (Printf.fprintf ic "%s" "<call  val =\"Goal\"><unit/></call>\n";  flush_all ());
   
    let x = if isWin () then   repeatreading ic oc  else stringReadFromCoq oc () in
    
   
    if x!="" then try newparse x with _ -> ((* print_string "ha\n";  *)sgoal ic oc ()) else ((* print_string "tooo\n"; *) sgoal ic oc ()) in
  let b = sgoal ic oc () in 
  
  if (l!= [] && (to_string b) = (to_string (List.hd l) )) then (  l) else  ( getmessages ic oc (b::l));;



let rec mygoal ic oc str = ignore (Printf.fprintf ic "%s" "<call  val =\"Goal\"><unit/></call>\n";flush_all ());
  let x = if isWin () then repeatreading ic oc  else stringReadFromCoq oc () in
  if str = x then x else ( (* print_string "ho";flush_all(); *) mygoal ic oc x);;

let rec soupgoal ic oc () = 
  let x = mygoal ic oc "" in
  try newparse x with _ ->  soupgoal ic oc ();;

let rec readnow ic oc str =
  let a, b, c = Unix.select [Unix.descr_of_in_channel oc] [Unix.descr_of_out_channel ic] [] 25.0 in
  if a != [] then
    let x= (readFromCoq (Unix.in_channel_of_descr (List.hd a)) ()) in
    readnow ic oc (str^x) else str;;



let evars ic () = Printf.fprintf ic "<call val=\"Evars\"><unit/></call>\n";flush_all ();;
let status ic () = Printf.fprintf ic "%s" "<call val=\"Status\"><bool val=\"false\"/></call>";flush_all ();;
let rec soupstatus ic oc () = ignore (status ic ()); try newparse (stringReadFromCoq oc ()) with _ -> soupstatus ic oc ();;
(* let addtext str i = "</call><call val='Add'><pair><pair><string>"^str^"</string>
   <int>0</int></pair><pair><state_id val='"^i^"'/><bool val='false'/></pair></pair></call>\n";; *)
let writeToCoq ic  str i = Printf.fprintf ic "%s"  (Processinputs.addtext str i) ;flush_all ()

let printAST ic i = Printf.fprintf ic "%s" ("<call val=\"PrintAst\"><state_id val=\""^i^"\"/></call>");flush_all ();;
let movebackto ic i = Printf.fprintf ic "%s" ("<call val=\"Edit_at\"><state_id val=\""^i^"\"/></call>");flush_all ();;


let rec findstateid ic oc id = match (attribute "val" ((soupstatus ic oc  ()) $ "state_id")) with
    Some x -> if int_of_string x >= int_of_string id then (Printf.printf "found it %s\n" x;flush_all ();  x) else (Printf.printf "still sameid";flush_all (); findstateid ic oc id)
  |_ -> Printf.printf "none in findstateid";flush_all (); findstateid ic oc id;;
let rec fstid ic oc id = try (findstateid ic oc id) with any -> Printf.printf "noneinfstid";flush_all (); fstid ic oc id;;
let rec reallyread ic oc id check =
  let mes = getmessages ic oc [] in
  
  let messages =String.concat "\n\n" (List.map Processresults.printmessages mes) in
   
  let error =try Str.search_forward (Str.regexp "rror") messages  0 with Not_found -> -1 in

  let newid =  fstid ic oc id in
  if check then ((* Printf.printf "old id = %s, newid= %s worked\n %s \n " id newid messages;  *)

mes)
  else 
  let c=  int_of_string newid > int_of_string id || error >= 0 || check in
  if c then ((* Printf.printf "old id = %s, newid= %s worked\n %s \n " id newid messages;  *)

mes)
  else ((* Printf.printf  "old id = %s, newid= %s tried again\n %s\n" id newid messages;flush_all (); *) reallyread ic oc id check )
    

  

