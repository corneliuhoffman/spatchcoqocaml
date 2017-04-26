let rec removed l =
match l with
[]->[]
|h::t -> let s = List.filter (fun x -> not (x=h)) t in
h::(removed s);;

let conclusion (tree:Processresults.goal Treestuff.tree)= 
	match tree with
	LEAF x -> x.conclusion
	|TREE (x,li) -> x.conclusion
let gethyps (tree:Processresults.goal Treestuff.tree)=
	match tree with
	LEAF x -> x.hyps
	|TREE (x,li) -> x.hyps
let newconclusion (tree:Processresults.goal Treestuff.tree) n: Processresults.statement  =
	match tree with
	LEAF x -> {name ="error"; content="SOMETHING IS WRONG"}
	|TREE (x,li) -> conclusion (List.nth li n)
let newhyp (oldgoal:Processresults.goal)  (newgoal:Processresults.goal Treestuff.tree) : Processresults.statement list = 
	
	let oldhyps = oldgoal.hyps in
	let newhyps = gethyps newgoal in
	List.filter (fun a -> not (List.mem a oldhyps)) newhyps
let deadhyp (tree:Processresults.goal Treestuff.tree) : Processresults.statement list =
	match tree with
	LEAF x -> [{name ="error"; content="SOMETHING IS WRONG"}]
	|TREE (x,li) ->
	let oldhyps = x.hyps in
	let newhyps = List.concat (List.map (fun x-> gethyps x) li) in
	List.filter (fun a -> not (List.mem a newhyps)) oldhyps

let change pattern string replist =
	let s = try (Pcre.extract_all  ~rex:(Pcre.regexp (pattern^"\{(.*?)\}")) string) with _ -> [||] in
	let tt = Array.to_list s in 
	if tt != [] then 
List.fold_left 
(fun a b -> Str.global_replace (Str.regexp (Array.get b 0)) 
  (List.nth replist ((int_of_string (Array.get b 1))-1)) a) string tt 
	else string;;
let changestrings string list = 
	List.fold_left (fun x a -> Pcre.replace ~rex:(Pcre.regexp  (fst a)) ~templ:(snd a) x) string list ;;
(* 
let rec changetree  pattern (tree:Processresults.goal Treestuff.tree) replist=
match tree with
LEAF x ->
if x = Processresults.emptygoal then  "This is done"
else "\\red{THIS STILL NEEDS A PROOF}"
|TREE (x, li)->
	let leave = x.leaving_tactic in
	change pattern leave li ;; *)
let header ="
\\documentclass[11pt, oneside]{article}   	


\\usepackage{graphicx}				
										
\\usepackage{amssymb}
\\newtheorem{Theorem}{Theorem}
\\newtheorem{Lemma}{Lemma}
\\newtheorem{Proposition}{Proposition}

\\title{Brief Article}
\\author{The Author}
\\date{}							% Activate to display a given date or no date

\\begin{document}
\\maketitle";;


let rec latex (tree:Processresults.goal Treestuff.tree) =
	match tree with
	LEAF x ->
	if x = Processresults.emptygoal then  "This is done"
	else "\\red{THIS STILL NEEDS A PROOF}"
	|TREE (x, li)-> 
	let leave = (x.leaving_tactic^"\n") in
	let nohyps = change "@newhyp" leave (List.map (fun a -> 
			 (String.concat "\n" (List.map Processresults.print_goal (newhyp x a)) )) li) in
	let noconcl = Str.global_replace (Str.regexp "@conc{}") (Processresults.print_goal (conclusion tree)) nohyps in 
	let nodeadhyp = Str.global_replace (Str.regexp "@deadhyp{}")  
		(String.concat "\n" (List.map (fun x-> Processresults.print_goal x) (deadhyp tree))) noconcl in
	let nonewcon = change "@newconclusion" nodeadhyp (List.map (fun a -> 
			  Processresults.print_goal (conclusion a)) li) in
	let novals = change "@val" nonewcon (Array.to_list x.values) in
	let uncleaned = change "@latex" novals (List.map latex li) in
	(changestrings uncleaned [ ("∨", "\\lor ");  ("→", "\\Rightarrow "); ("∀", "\\forall "); 
	("∃", "\\exists ");  ("∧", "\\land "); ("↔", "\\Leftrightarrow ")])



