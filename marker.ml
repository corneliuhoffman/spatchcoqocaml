 open Printf
open Treestuff
open Processresults
open Sexplib
open Sexplib.Std

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  input ic s 0 n;
  close_in ic;
  (s)
;;
   


   

type cheat = {penalty:float; text:string}
type listofresults ={title:string; cheats:cheat list}
type student = {id:string; results:listofresults list }
let firsttwo list = match list with
[]->list
|h::[]->list
|h::tl -> h::[List.hd tl]
let rec remove li = match li with
[] -> li
| x::tl -> if List.mem x tl then (remove tl) else x::(remove tl);;

let get_lemas listofstudents =
	let list_of_titles = List.concat (List.map (fun x -> List.map (fun s ->
	let strings = Str.split (Str.regexp " \|(") (String.trim s.title) in
	String.concat " "  (firsttwo (List.filter (fun x-> x!="") strings))
) x.results) listofstudents) in
	remove list_of_titles

let clean init =
	let re =(Str.regexp "\"") in
	Scanf.unescaped (Str.global_replace re " " init)
	(* let init1 = Str.global_replace re "" init in
	 let init2 =Str.global_replace (Str.regexp "\\\\n") "\n" init1 in
	 Str.global_replace (Str.regexp "\\\\226\\\\136\\\\168") "\226\136\168" init2 *)
let countcheats tree =
	let leaves = Treestuff.get_leaves tree in
	List.fold_left (fun x y -> if (Pcre.pmatch ~rex:(Pcre.regexp "CHEAT") y) then x+1 else x ) 0 leaves;;

let rec findcheats p tree =
	let cheats = (match tree with
	LEAF a -> []
	|TREE (a, [LEAF b]) -> 
		if (Pcre.pmatch ~rex:(Pcre.regexp "CHEAT") b ) then [(a, p)]
		else []
	|TREE (a, l) -> List.concat (List.map (fun x -> findcheats (p+1) x) l)) in
	cheats

let printcheats tree=
		let cheats = findcheats 0 tree in
		let score = 100. -. (List.fold_left (fun x y -> x+. (100./. 2.0**float_of_int(snd y-1))) 0. cheats) in
		if score =100. then {title = Treestuff.get_title tree; cheats =[]}
	else 
		 {title = Treestuff.get_title tree; cheats = List.map (fun x -> {penalty =100./. 2.0**float_of_int(snd x-1); text = fst x}) cheats}		
		(* sprintf " %s" 
		(String.concat ", " (List.map (fun x -> 
			 (fst x)^"\n at a suggested penalty of: "^(string_of_float(100./. 2.0**float_of_int(snd x-1)))) cheats)) 
		
 *)


	(* if (countcheats tree > 0) then
		let leaves = Treestuff.get_leaves_with_depth 0 tree in

		let filteredleaves = List.filter (fun x-> (Pcre.pmatch ~rex:(Pcre.regexp "CHEAT") (fst x))) leaves in
		let score = string_of_float(100.-.(List.fold_left (fun x y -> x+. (100./. 2.0**float_of_int(snd y-2))) 0. filteredleaves)) in 
		sprintf "\n The unsolved statements are \n %s\n the total score is %s  " 
		(String.concat "\n" (List.map (fun x -> 
			(String.concat "\n\n" (Treestuff.findparents (LEAF (fst x)) tree)^"\n at a suggested penalty of: "^(string_of_float(100./. 2.0**float_of_int(snd x-2))))) filteredleaves)) score
		
	else "You get 100%" *)


let get_trees file =
	let s = String.trim (load_file file) in
    	
    	let sext = try (Sexplib.Sexp.of_string s) with _ -> Sexplib.Sexp.of_string "" in
    	(* print_string (Sexplib.Sexp.to_string  sext); *)
    	
    	let treelist = Sexplib.Std.list_of_sexp (fun x -> Treestuff.tree_of_sexp Sexplib.Sexp.to_string x) sext in 
    	List.rev treelist;;
let rec print_mytree t = match t with
| LEAF x -> LEAF (sprintf "%s" (clean x))
| TREE (a, l) ->TREE ((sprintf "%s" (clean a)), (List.map print_mytree l))

  let readdir dir =
    	(* let dir= Sys.argv.(1) in
    	let index = Sys.argv.(2) in *)
    	let list = List.filter (fun x-> match Str.split (Str.regexp "\.") x with
    |[_;"mrk"] -> true
    |_ -> false )
    		(Array.to_list (Sys.readdir dir)) in

    	let strings=List.map (fun text ->
    	let treelist = List.map print_mytree (get_trees (dir^"/"^text)) in 
    	let students =
    	{id= text ; results = List.map 
    		(fun  tree -> 
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
    	List.map print_string (get_lemas strings);
    	strings;;






