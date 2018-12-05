open Soup
type statement ={ name: string; content: string};;


type goal ={number: string; hyps : statement list; conclusion: statement; mutable state_id: int ; mutable leaving_tactic: string; mutable values:string array};;


let emptygoal ={number="-1"; hyps=[];conclusion ={name=""; content="no goals"}; state_id=0; leaving_tactic =""; values =[||] } 



let print_goal {name=b; content= c} =
  (* let ast = Formulaparsing.parse (String.trim c) in
     Printf.printf "\n\n\n\ this is the %s\n\n\n form %s\n\n" (String.trim c) (Ast.to_string ast);flush_all (); *)
  let d = try (Formulaparsing.print (Formulaparsing.parse (String.trim c))) with any ->  (String.trim c) (* raise any *) in

  if b ="" 
  then
    Printf.sprintf "%s "  (String.trim d)
  else
    Printf.sprintf "%s : %s " (String.trim b) (String.trim d);;
let print_goals {number=n; hyps=h; conclusion= c; leaving_tactic=l; values = values} = 
  n^"\n--\n"^(String.concat "\n" (List.map print_goal h))^"\n================\n"^(print_goal c)^"\nleavingtactic:"^l;;  

(* let oc,ic,ec = Unix.open_process_full "/Applications/CoqIDE_8.6.app/Contents/Resources/bin/coqtop -ideslave -main-channel stdfds" (Unix.environment ());;
*)

let astofstr str goal=
  let n, formula = if goal then  "", str 
    else (
      let break = Str.split (Str.regexp ":") (String.trim str) in
      let n, formula = if List.length break =1 then "", str else List.hd break, String.concat ":" (List.tl break)
      in n, formula) in 
  Printf.printf "formula %s\n " formula ; flush_all ();
  let f = try (Formulaparsing.parse formula) with any -> Var formula in

  n,  f

let listofstr str goal =

  let name,ast =  astofstr str goal in

  Formulaparsing.getlist ast

let xmltostr str = 
  Str.global_replace (Str.regexp "&nbsp;") " " str;;

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
    Printf.printf "the concll = %s\n\n\n\n"  (cleanstr (get_texts t));
    {emptygoal with number =get_texts h;hyps = List.map (fun x-> strToStatement (get_texts x) )(to_list (hyp$$"_")); conclusion = {name =""; content = (cleanstr (get_texts t))}}
  |_ -> {emptygoal with number= "" ;hyps =[]; conclusion ={name=""; content =""}};;
let goallist x = 
  let goals=to_list (x$$"goals") in
  match goals with
  | [] -> []
  | _ -> 
    let gg = to_list ((List.hd goals)$$"list") in
    (* List.map (fun a -> print_string (to_string a); print_string "\n\n") gg;
       print_int (List.length gg); *)
    match gg with
    | [] -> []
    | _ ->  to_list ((List.hd gg)$$"goal") 

let processoutput x = 
  Printf.printf "here is a lst: %s\n\n\n\n" (cleanstr (get_texts x)); flush_all (); 
  let goals= goallist x in 
  if goals =[] then [emptygoal]
  else   let g =List.map (fun a -> (manage (to_list (children a)))) goals in
    g;;

let printmessages x =
  let clean st = Printf.sprintf "%s" "\n"^(Str.global_replace (Str.regexp "<_>") "\n-------\n" st ) in
  let ll =List.map (select "richpp") (to_list (x$$"message")) in 

  let newlist=List.map (function a -> List.map (String.concat "") (List.map texts (to_list a))) ll in
  String.concat "\n========================\n" (List.map (function a -> xmltostr (String.concat "\n" a)) newlist);; 
let get_a_goal li = 
  let procs = List.filter (fun x-> processoutput x  != [emptygoal]) li in
  match procs with
    []-> List.hd li
  | _-> List.hd (List.rev procs)












