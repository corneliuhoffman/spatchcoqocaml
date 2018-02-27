open Treestuff
open Processresults
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
  |TREE (x,li) ->  try (conclusion (List.nth li n)) with any -> Printf.printf " neconclusion n = %i, length l =%i" n (List.length li); flush_all (); {name=""; content= ""}
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
Printf.printf "I am changing %s and %s " pattern string; 
List.map (fun a-> Printf.printf "%s;\n" a) replist; flush_all ();

  let s = try (Pcre.extract_all  ~rex:(Pcre.regexp (pattern^"\{(.*?)\}")) string) with _ -> [||] in
  let tt = Array.to_list s in 
  List.map (fun a-> Printf.printf "%i;\n" (Array.length a)) tt; flush_all ();

  if tt != [] then 
    List.fold_left 
      (fun a b ->           Printf.printf "\n\n\n\n n = %i, length l =%i, a=%s , b = %s " ((int_of_string (Array.get b 1))-1) (List.length replist) a (Array.get b 0); flush_all ();
 
        Str.global_replace (Str.regexp (Array.get b 0)) 
          (try (List.nth replist ((int_of_string (Array.get b 1))-1)) with _ -> print_string "ouch";flush_all (); "") a) string tt 
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

\\usepackage{color}
\\usepackage{graphicx}				
\\usepackage{amssymb}
\\newtheorem{Theorem}{Theorem}
\\newtheorem{Lemma}{Lemma}
\\newtheorem{Proposition}{Proposition}
\\newtheorem{Definition}{Definition}
\\newtheorem{Inductive}{Inductive Definition}
\\newtheorem{Variable}{Variable}
\\newtheorem{Notation}{Notation}
\\newtheorem{Axiom}{Axiom}
 \\usepackage{tcolorbox}
 \\tcbuselibrary{skins}
 \\tcbuselibrary{theorems}
\\tcbuselibrary{breakable}


\\newcommand{\\mybox}[1]{\\begin{tcolorbox}[colback=white,colframe=gray!20!white, breakable, skin=enhancedmiddle]#1 \\end{tcolorbox}}
\\title{Brief Article}
\\author{The Author}
\\date{}							% Activate to display a given date or no date

\\begin{document}
\\maketitle\n\n";;

let preplatex str = 
  changestrings str [ ("∨", "\\lor ");  ("→", "\\Rightarrow "); ("∀", "\\forall "); 
                               ("∃", "\\exists ");  ("∧", "\\land "); ("↔", "\\Leftrightarrow "); (" +", " "); 
                               ("∈", "\\in "); ("∩", "\\cap "); ("∪", "\\cup ");("_", "-"); ("⊆", "\\subseteq "); ("∅", "\\emptyset "); ("∁", "\\complement "); ("<b>", ""); ("</b>", "")]

let rec latex (tree:Processresults.goal Treestuff.tree) =
  match tree with
    LEAF x ->
    if x = Processresults.emptygoal then  "This is done"
    else (
      if not (x.leaving_tactic = "") then  x.leaving_tactic 
      else "{\\color{red} THIS STILL NEEDS A PROOF}")
  |TREE (x, li)-> 
    let leave = (x.leaving_tactic^"\n") in
    let nohyps =  change "@newhyp" leave (List.map (fun a -> 
        (String.concat "$$ $$" (List.map Processresults.print_goal (newhyp x a)) )) li) in
    let noconcl = Str.global_replace (Str.regexp "@conc{}") (Processresults.print_goal (conclusion tree)) nohyps in 
    let nodeadhyp = Str.global_replace (Str.regexp "@deadhyp{}")  
        (String.concat "\n" (List.map (fun x-> Processresults.print_goal x) (deadhyp tree))) noconcl in
    let nonewcon =  change "@newconclusion" nodeadhyp (List.map (fun a -> 
        Processresults.print_goal (conclusion a)) li) in
    let novals =  Printf.printf "\n\n\n\nI am trying the values %i\n\n\n\n" (List.length (Array.to_list x.values)) ; flush_all (); change "@val" nonewcon (Array.to_list x.values) in
    let uncleaned = String.trim (change "@latex" novals (List.map (fun a -> (* "\\begin{subproof}"^ *)(latex a)^"\n\n" (* ^"\\end{subproof}" *) ) li)) in
    (changestrings uncleaned [ ("∨", "\\lor ");  ("→", "\\Rightarrow "); ("∀", "\\forall "); 
                               ("∃", "\\exists ");  ("∧", "\\land "); ("↔", "\\Leftrightarrow "); (" +", " "); 
                               ("∈", "\\in "); ("∩", "\\cap "); ("∪", "\\cup ");("_", "-"); ("⊆", "\\subseteq "); ("∅", "\\emptyset "); ("∁", "\\complement "); ("<b>", ""); ("</b>", "")])



