



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
		[("Tactic Notation", ""); (":=.+?", "."); ("constr(.?)\|hyp(.?)\|reference(.?)\|ident(.?)", "VAR")]);;


let makeregexp str =
	Str.regexp (Str.global_replace (Str.regexp "VAR") "\(.+?\)" (makevar str));;


let checkinput str list = 
if (try (Str.search_forward (Str.regexp "Lemma\|Qed\|Theorem\|Proposition\|Search\|Require\|Axiom\|Check\|Print") str 0 )>=0  with _-> false) then true
else  (List.exists (fun x-> x>=0) (List.map (fun a-> try Str.search_forward (makeregexp a) str 0 with _ -> -1) list));;

let prepareforxml str = replacelist 
						(cleanstr str ) 
						[("&", "&amp;");  ("\"", "&quot;"); ("'", "&apos;"); (">", "&gt;"); ("<", "&lt;")]

let addtext str i = "<call val='Add'><pair><pair><string>"^(prepareforxml str)^"</string>
<int>0</int></pair><pair><state_id val='"^i^"'/><bool val='false'/></pair></pair></call>\n";;
