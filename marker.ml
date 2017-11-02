 open Printf
open Treestuff
open Processresults
open Sexplib
open Sexplib.Std

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  input ic s 0 n;
  close_in ic;
  (s)
;;
   
let clean a =
  Scanf.unescaped (Str.global_replace (Str.regexp "\"\|<b>\|</b>") "" a) 

let get_trees file =
  let s = String.trim (load_file file) in
      
      let sext = try (Sexplib.Sexp.of_string s) with _ -> Sexplib.Sexp.of_string "" in

      
      let treelist = Sexplib.Std.list_of_sexp (fun x -> Treestuff.tree_of_sexp  Sexplib.Sexp.to_string  x) sext in 
      
      List.rev treelist;;
let get_lematext txt =
  let rex = Pcre.regexp "--(.*?)==" in
  let ti = Pcre.extract_all ~rex ( txt) in
  let hi = Array.get ti 0 in
  Array.get hi 1

let rec red li =
  match li with
  |[] -> []
  |(h1,h2,h3)::t -> if List.mem h1 (List.map (fun (h1,h2,h3) -> h1) t) then red t else let t1 = red t in (h1,h2,h3)::t1

let rec findcheats p tree =
  let cheats = (match tree with
  LEAF a -> if (Pcre.pmatch ~rex:(Pcre.regexp "no goals") a ) then [] else [(a,p)] 
  |TREE (a, l) -> 
    if (Pcre.pmatch ~rex:(Pcre.regexp "I am stuck and") a ) then [(a, p)]
    else  List.concat (List.map (fun x -> findcheats (p+1) x) l)) in
  cheats

let getlemas file =
  let treelist = get_trees file in
  let rex1  = Pcre.regexp "label{(.*?)}" in
  let rex = Pcre.regexp "--(.*?)==" in

  List.map (fun x->

   let core1 = Pcre.extract_all ~rex (Treestuff.get_title x) in

                    let hi = Array.get core1 0 in
                    let core =(Scanf.unescaped (Array.get hi 1)) in 
    let titl = Pcre.extract_all ~rex:rex1 (Treestuff.get_title x) in

                    let ti = Array.get titl 0 in
                    let titl =Array.get ti 1 in 
(titl,core, String.concat "\n" (List.map (fun (a, p)->  (Printf.sprintf "Cheat \n %s                          at level %i\n" (clean a) p))(* "statement "^(Printf.sprintf "%s" a)^" at levels "^(string_of_int p) *) (findcheats 0 x)))
) treelist






type cheat = {penalty:float; text:string}
type listofresults ={title:string; cheats:cheat list}
type student = {id:string; results:listofresults list }
let printlistofresults list=
	"title:"^list.title^"\n cheats:"^(String.concat "\n" (List.map (fun x-> (string_of_float x.penalty)^","^x.text) list.cheats));;
let printstudent student =
	let ss= (String.concat "\n----\n" (List.map (fun x -> (printlistofresults x)) student.results)) in 
	Printf.printf "id:%s \n cheats: %s \n\n\n\n
	" student.id ss;;

let firsttwo list = match list with
[]->list
|h::[]->list
|h::tl -> h::[List.hd tl]
let rec remove li = match li with
[] -> li
| x::tl -> if List.mem x tl then (remove tl) else x::(remove tl);;

let get_lemas listofstudents =
	let list_of_titles = List.concat (List.map (fun x ->  List.map (fun a -> (get_lematext a.title)) x.results) listofstudents) in
	remove (List.filter (fun x-> Pcre.pmatch ~rex:(Pcre.regexp "Lemma|Proposition|Theorem") x) list_of_titles)

let clean init =
	let re =(Str.regexp "\"") in
	Scanf.unescaped (Str.global_replace re " " init)
	
let countcheats tree =
	let leaves = Treestuff.get_leaves tree in
	List.fold_left (fun x y -> if (Pcre.pmatch ~rex:(Pcre.regexp "I am stuck and") y) then x+1 else x ) 0 leaves;;

let rec findcheats p tree =
	let cheats = (match tree with
	LEAF a -> if (Pcre.pmatch ~rex:(Pcre.regexp "-1") a ) then [] else [(a,p)] 
	|TREE (a, [LEAF b]) -> 
		if (Pcre.pmatch ~rex:(Pcre.regexp "I am stuck and") a ) then [(a, p)]
		else []
	|TREE (a, l) -> List.concat (List.map (fun x -> findcheats (p+1) x) l)) in
	cheats

let printcheats tree =
		let cheats = findcheats 0 tree in
		let score = 100. -. (List.fold_left (fun x y -> x+. (100./. 2.0**float_of_int(snd y-1))) 0. cheats) in
		if score =100. then {title = Treestuff.get_title tree; cheats =[]}
	else 
		 {title = Treestuff.get_title tree; cheats = List.map (fun x -> {penalty =100./. float_of_int(snd x); text = fst x}) cheats}		


let rec print_mytree t = match t with
	| LEAF x -> LEAF (sprintf "%s" (clean x))
    | TREE (a, l) ->TREE ((sprintf "%s" (clean a)), (List.map print_mytree l))

  let readdir dir =
    	
    let list = List.filter (fun x-> match Str.split (Str.regexp "\.") x with
    |[_;"mrk"] -> true
    |_ -> false )
    		(Array.to_list (Sys.readdir dir)) in list ;;(* in
    		(* List.map print_string list; flush_all (); *)
    	let strings=List.map (fun text ->
    	let treelist = List.map print_mytree (get_trees (dir^"/"^text)) in 
    	let students =
    	{id= text ; results = List.map 
    		(fun  tree -> 
    		(* Treestuff.print_tree (fun x ->Printf.printf "%s"  x)  tree; flush_all (); *)
      		let cheats = findcheats 0 tree in
			let score = 100. -. (List.fold_left (fun x y -> x+. (100./. 2.0**float_of_int(snd y-1))) 0. cheats) in
	
      		let tt = sprintf  " %s, %i, %i, %i, %s" text  (* (Treestuff.get_title tree) *) (Treestuff.countvertices tree) (Treestuff.depth tree) (countcheats tree) (string_of_float score)  in
      		printcheats tree

      	 ) treelist} in 
    	students

    	)
    	list in
    	(* print_string (String.concat "\n====\n"  (List.map (fun x-> String.concat "\n" (List.map (fun a-> a.text) x)) strings));
 *)
    	(* printf "%i\n" (List.length strings);
    	List.map (fun x -> printf "%s, %s\n" x.id 
    		(String.concat "\n" (List.map (fun a-> (a.title^(sprintf "number of cheats =%i" (List.length a.cheats)))) x.results) )) strings; *)
    	(* List.map print_string (get_lemas strings);flush_all(); *)
    	strings;; *)
(* 
let dir = Sys.argv.(1) in	
    let stud=readdir dir in
    let out = open_out "try.txt" in
       
    List.map (fun x->
     
      let lem =getlemas (dir^x) in
      List.map (fun l-> Printf.fprintf out "%s" (Scanf.unescaped l)) lem;
      let trees =get_trees (dir^x) in
      List.map (fun tr -> let cheats = findcheats 0  tr in
      List.map (fun l-> Printf.printf "%s\n" (fst l)) cheats
    ) trees )
    stud;

    flush_all ();

       close_out  out; *)



