let replacelist str li =
  List.fold_left (fun y (a,b) -> Str.global_replace (Str.regexp a) b  y) str li;;
let cleanstr str = 
  replacelist str
    [(" +", " "); (" +\\.", "."); ("^ +", "");];;
let cleanforsearch str = 
  replacelist str
    [(" +", " "); (" +\\.", "."); ("^ +", ""); ("\"", "")];;

let makevar str =
  cleanstr (replacelist (cleanforsearch str)
              [("Tactic Notation", ""); (":=.+?", ".");("\'", ""); ("constr(.?)\|hyp(.?)\|reference(.?)\|ident(.?)", "VAR")]);;


let makeregexp str =
  Pcre.regexp (Str.global_replace (Str.regexp "VAR") "(.+)" (makevar str));;


let checkinput str list = 
  if Pcre.pmatch ~rex:(Pcre.regexp "Lemma|Qed|Admitted|Theorem|Proposition|Search|Fixpoint|Require|Axiom|Check|Print|Definition|Inductive|Open|Variable|Notation") str  then true
  else  (List.exists (fun x-> x) (List.map (fun a-> Pcre.pmatch ~rex:(makeregexp a) str) list));;

let get_tactic str list =
  List.map List.hd (List.filter (fun a -> Pcre.pmatch ~rex:(makeregexp (List.nth a 1)) str) list);;

let get_values str list =
  if Pcre.pmatch ~rex:(Pcre.regexp "Lemma|Qed|Admitted|Theorem|Proposition|Fixpoint|Search|Require|Axiom|Check|Print|Definition|Inductive|Open|Variable|Notation") str  then [||]
  else
    let tac =List.hd (List.filter (fun a -> Pcre.pmatch ~rex:(makeregexp (List.nth a 1)) str) list) in

    let array = Array.get (Pcre.extract_all ~rex:(makeregexp (List.nth tac 1)) str) 0 in

    Array.sub array 1 ((Array.length array) -1);;

let prepareforxml str = replacelist 
    (cleanstr str ) 
    [("&", "&amp;"); ("\"", "&quot;"); ("'", "&apos;"); (">", "&gt;"); ("<", "&lt;"); ("forall", "∀")]
let prepareforprint str = replacelist 
    (cleanstr str ) 
    [("forall", "∀"); ("->", "→"); ("exists", "∃"); ("<->", "↔"); ("\\\\/", "∨"); ("/\\\\ ", "∧")] ;;	

let separate lemma =
  let lem = prepareforprint lemma in

  let firstbreak = if (Pcre.pmatch ~rex:(Pcre.regexp "Definition|Axiom|Inductive|Notation|Variable") lem) then 
      Pcre.split ~rex:(Pcre.regexp "(\s*\(.*:.*\)\s*)*:=") lem
    else Pcre.split ~rex:(Pcre.regexp "(\s*\(.*:.*\)\s*)*:") lem in
  let cleaner = List.map (fun x-> String.trim (cleanstr x)) firstbreak in
  let clear = List.filter ( fun x -> not (x = "")) cleaner in
  let cont = 
    if (Pcre.pmatch ~rex:(Pcre.regexp "Definition|Axiom|Inductive|Notation|Variable") lem) then
      String.concat " " (List.tl (Str.split (Str.regexp " ") lem))
    else 
      (String.concat ":" (List.tl clear))
  in
  let hd = Pcre.split (List.hd clear) in
  if (Pcre.pmatch ~rex:(Pcre.regexp "Definition|Axiom|Inductive") lem) then
   "\\begin{"^(List.hd hd)^"}["^(List.nth hd 1)^"] \\label{"^(List.hd hd)^":"^(List.nth hd 1)^"}\n$"^(Str.global_replace (Str.regexp " ") "\," cont)^"$\n \\end{"^(List.hd hd)^"}\n"
 else 
    "\\begin{"^(List.hd hd)^"}\n$"^(Str.global_replace (Str.regexp " ") "\," cont)^"$\n \\end{"^(List.hd hd)^"}\n";;

let addtext str i = "<call val='Add'><pair><pair><string>"^(prepareforxml str)^"</string>
<int>0</int></pair><pair><state_id val='"^i^"'/><bool val='false'/></pair></pair></call>\n";;