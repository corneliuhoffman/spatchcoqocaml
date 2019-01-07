open Soup
let newparse  a = Markup.string a |> Markup.parse_xml |> Markup.signals |> from_signals;;
let rec zip l1 l2 l = match l1 with
    [] -> l
  |h1::t1 -> match l2 with  []-> l | h2::t2 -> zip t1 t2 ([[h1;h2]]::l);;
let  readFromCoq oc () = 
  let bb=Bytes.create 100000 in 
  let i=Unix.read (Unix.descr_of_in_channel oc) bb 0 100000 in

  let res= Bytes.sub bb  0 i in
  res
;;
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


    if x!=Bytes.empty then try newparse (Bytes.to_string x) with _ -> ((* print_string "ha\n";  *)sgoal ic oc ()) else ((* print_string "tooo\n"; *) sgoal ic oc ()) in
  let b = sgoal ic oc () in 

  if (l!= [] && (to_string b) = (to_string (List.hd l) )) then (  l) else  ( getmessages ic oc (b::l));;
let rec getlisttext ic oc st =
  let x = if isWin () then   repeatreading ic oc  else stringReadFromCoq oc () in
  let nst = st^(Bytes.to_string x) in
  let v =try Str.search_forward (Str.regexp "/value") (nst)  0 with Not_found -> -1 in
  if v >0 then nst else ( getlisttext ic oc nst);;







let rec mygoal ic oc str = ignore (Printf.fprintf ic "%s" "<call  val =\"Goal\"><unit/></call>\n";flush_all ());
  let x = if isWin () then repeatreading ic oc  else stringReadFromCoq oc () in
  if str = x then x else ( (* print_string "ho";flush_all(); *) mygoal ic oc x);;

let rec soupgoal ic oc () = 
  let x = mygoal ic oc Bytes.empty in
  try newparse (Bytes.to_string x) with _ ->  soupgoal ic oc ();;

let rec readnow ic oc str =
  let a, b, c = Unix.select [Unix.descr_of_in_channel oc] [Unix.descr_of_out_channel ic] [] 25.0 in
  if a != [] then
    let x= (readFromCoq (Unix.in_channel_of_descr (List.hd a)) ()) in
    readnow ic oc (str^(Bytes.to_string x)) else str;;


let get_opt x =
  match x with
  |Some x -> x
  |None ->"";;

let change x = String.concat "" (List.map (fun a-> Str.global_replace (Str.regexp "&nbsp;") " " a) x) 
let getok s =
  let v = newparse s in
  let att = (attribute "val" (v$"value")) in
  let mess_lev = try (get_opt (attribute "val" (v$"message_level"))) with _-> "" in
  match att with
  | Some "fail" -> let st = get_opt (attribute "loc_s" (v$"value")) in
    let en = get_opt (attribute "loc_e" (v$"value")) in
    ("fail", "Found error between position  "^st^" and position "^en^"\n"^(change (texts (v$"value"))))
  |Some "good" -> if mess_lev ="" 
    then ("good", "")

    else 
      let m = v$"message" in
      if (attribute "val" (m$"option") = Some "some")
      then 

        let st = get_opt (attribute "start" (m$"loc")) in
        let en = get_opt (attribute "stop" (m$"loc")) in
        let txt = change (texts m) in
        (mess_lev, "Found "^mess_lev^" between position  "^st^" and position "^en^"\n"^txt)
      else 
        (mess_lev, change (texts (v$"message")))
  | _ -> ("strange", "") ;;


let prodgoals s=
  let v = newparse s in 

  let l1 = v$$"goal" in
  let l12= List.map (fun x-> 
      try (List.map (fun z -> change (texts z)) (x$"list"$$"pp"|>to_list)) with _ ->[]) (l1|>to_list) in
  let l11=List.map (fun x-> [change (texts (List.hd (List.rev  (x$$"pp"|>to_list))))]) (l1|>to_list) in
  List.rev (zip l12 l11 []);;

let about ic oc () = Printf.fprintf ic "<call val=\"About\"><unit/></call>\n";flush_all ();
  let result = newparse (Bytes.to_string (repeatreading ic oc)) in
  if (List.hd (texts (result$"coq_info")))>= "8.8" then 
    (print_string "big")
  else   (print_string "small"); flush_all ();
  List.hd (texts (result$"coq_info"))
;;
let evars ic () = Printf.fprintf ic "<call val=\"Evars\"><unit/></call>\n";flush_all ();;
let status ic () = Printf.fprintf ic "%s" "<call val=\"Status\"><bool val=\"false\"/></call>";flush_all ();;
let rec soupstatus ic oc () = ignore (status ic ()); try newparse (Bytes.to_string (stringReadFromCoq oc ())) with _ -> soupstatus ic oc ();;
(* let addtext str i = "</call><call val='Add'><pair><pair><string>"^str^"</string>
   <int>0</int></pair><pair><state_id val='"^i^"'/><bool val='false'/></pair></pair></call>\n";; *)
let writeToCoq ic  str i = Printf.fprintf ic "%s"  (Processinputs.addtext str i) ;flush_all ()

let printAST ic i = Printf.fprintf ic "%s" ("<call val=\"PrintAst\"><state_id val=\""^i^"\"/></call>");flush_all ();;
let movebackto ic i = Printf.fprintf ic "%s" ("<call val=\"Edit_at\"><state_id val=\""^i^"\"/></call>");flush_all ();;


let rec findstateid ic oc id = match (attribute "val" ((soupstatus ic oc  ()) $ "state_id")) with
    Some x -> if int_of_string x >= int_of_string id then (Printf.printf "found it %s\n" x;flush_all ();  x) else (Printf.printf "still sameid";flush_all (); findstateid ic oc id)
  |_ -> Printf.printf "none in findstateid";flush_all (); findstateid ic oc id;;

let rec fstid ic oc id = try (findstateid ic oc id) with any -> Printf.printf "noneinfstid";flush_all (); fstid ic oc id;;
let rec fstid8 ic oc id = let mes= List.hd (getmessages ic oc []) in
  let ids =  List.map (fun a ->try (attribute "val" a) with _-> None) (mes $$ "state_id"|>to_list) in
  let maxid =  List.fold_left (fun a b -> 
      let z = (match b with
            Some z -> int_of_string z
          |None -> -1) in  max a z ) 0 ids in
  string_of_int (max maxid (int_of_string id))  


let rec reallyread ic oc id check =
  let mes = getmessages ic oc [] in

  let messages =String.concat "\n\n" (List.map Processresults.printmessages mes) in

  let error =try Str.search_forward (Str.regexp "rror") messages  0 with Not_found -> -1 in

  let newid =  fstid ic oc id in
  if check then (Printf.printf "old id = %s, newid= %s worked\n %s \n " id newid messages; 

                 mes)
  else 
    let c=  int_of_string newid > int_of_string id || error >= 0 || check in
    if c then (Printf.printf "old id = %s, newid= %s worked\n %s \n " id newid messages; 

               mes)
    else (Printf.printf  "old id = %s, newid= %s tried again\n %s\n" id newid messages;flush_all (); reallyread ic oc id check )

let rec  last2 strings =
  match strings with
    [] ->""
  |x::[] -> x
  |x::y::[] -> x^"\n"^y
  |h::t -> last2 t;;

let searchresults str =
  let obj = (newparse str)$$"coq_object"|>to_list in
  List.map (fun a-> let strings = a$$"string"|>to_list in 
             String.concat "\n" (List.map (fun b -> (change (texts b))) strings)) obj;;

let searchpattern ic oc str =
  let _ =  getlisttext ic oc "" in

  let s = "<call val=\"Search\"><list>
    <pair>
      <search_cst val=\"subtype_pattern\">
        <string> "^str^" </string>
      </search_cst>
      <bool val=\"true\"/>
    </pair>
  </list>
</call>" in
  Printf.fprintf ic "%s" s ;flush_all ();
  let res =  getlisttext ic oc "" in
  print_string res ;flush_all ();
  searchresults res;;

let query ic oc str id =

  let r =  getlisttext ic oc "" in
  print_string r;flush_all ();

  let s = "<call val=\"Query\">
  <pair>
    <route_id val=\"0\"/>
  <pair>
    <string> "^(Processinputs.prepareforxml str)^"</string>
    <state_id val=\""^id^"\"/>
  </pair>
  </pair>
</call>" in 


  Printf.fprintf ic "%s" s ;flush_all ();

  let res =  getlisttext ic oc "" in
  print_string res;flush_all ();
  print_string ("\n\n\n"^id^"\n\n"); flush_all ();
  let v = newparse res in
  let response = getok res in
  if fst response = "fail" then
    (snd response)
  else
    (
      let m =v$$"message"|>to_list in
      let ss=String.concat "\n" (List.map (fun a-> change (texts a))  m) in
      ss);;





