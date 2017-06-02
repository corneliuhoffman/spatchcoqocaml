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
		if score =100. then "\n The total score is 100\n"
	else 

		sprintf "\n The unsolved statements are \n %s\n The total score is %s  " 
		(String.concat "\n" (List.map (fun x -> 
			 (fst x)^"\n at a suggested penalty of: "^(string_of_float(100./. 2.0**float_of_int(snd x-1)))) cheats)) (string_of_float score)
		



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

  let () =
    	let dir= Sys.argv.(1) in
    	let list = List.filter (fun x-> match Str.split (Str.regexp "\.") x with
    |[_;"mrk"] -> true
    |_ -> false )
    		(Array.to_list (Sys.readdir dir)) in

    	List.map (fun text ->
    	let treelist = List.map print_mytree (get_trees (dir^"/"^text)) in 
    	printf " The file %s has %i statements:\n ------- \n" text (List.length treelist);
      	List.map (fun x-> printf  " The statement \n %s\n\n whose total number of vertices is %i and maximum depth is %i and number of cheats is %i. %s \n--\n" (Treestuff.get_title x) (Treestuff.countvertices x) (Treestuff.depth x) (countcheats x) (printcheats x)) treelist; printf     "\n \n\n\n\n\n\n\n"; )
    	list; ();;






