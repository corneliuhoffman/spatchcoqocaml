open Soup
type statement ={ name: string; content: string};;

 
type goal ={number: string; hyps : statement list; conclusion: statement; mutable state_id: int ; mutable leaving_tactic: string; mutable values:string array};;


let emptygoal ={number="0"; hyps=[];conclusion ={name=""; content=""}; state_id=0; leaving_tactic =""; values =[||] } 



let print_goal {name=b; content= c} = if b ="" 
		then
			Printf.sprintf "%s "  (String.trim c)
		else
  			Printf.sprintf "%s : %s " (String.trim b) (String.trim c);;
let print_goals {number=n; hyps=h; conclusion= c; leaving_tactic=l; values = values} = 
  n^"\n--\n"^(String.concat "\n" (List.map print_goal h))^"\n================\n"^(print_goal c)^"\n";;  

(* let oc,ic,ec = Unix.open_process_full "/Applications/CoqIDE_8.6.app/Contents/Resources/bin/coqtop -ideslave -main-channel stdfds" (Unix.environment ());;
 *)
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
{emptygoal with number =get_texts h;hyps = List.map (fun x-> strToStatement (get_texts x) )(to_list (hyp$$"_")); conclusion = strToStatement (get_texts t)}
|_ -> {emptygoal with number= "" ;hyps =[]; conclusion ={name=""; content =""}};;
let goallist x = 
  to_list (x$$"goal");;

let processoutput x = 
  let goals= goallist x in 
  if goals =[] then ["no more goals"]
else   List.map (fun a ->print_goals (manage (to_list (children a)))) goals;;

let printmessages x =
let clean st = Printf.sprintf "%s" "\n"^(Str.global_replace (Str.regexp "<_>") "\n-------\n" st ) in
  let ll =List.map (select "richpp") (to_list (x$$"message")) in 
  let newlist=List.map (function a -> List.map (String.concat "") (List.map texts (to_list a))) ll in
  String.concat "\n========================\n" (List.map (function a -> xmltostr (String.concat "\n" a)) newlist);; 
