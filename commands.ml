

let string_to_utf str =
	String.map (fun ch -> if (Glib.Utf8.validate (String.make 1  ch)) then ch else '?') str


let lines ?encoding (src : [`Channel of in_channel | `String of string]) =
  let rec loop d buf acc = match Uutf.decode d with
  | `Uchar u ->
      begin match Uchar.to_int u with
      | 0x000A ->
          let line = Buffer.contents buf in
          Buffer.clear buf; loop d buf (line :: acc)
      | _ ->
          Uutf.Buffer.add_utf_8 buf u; loop d buf acc
      end
  | `End -> List.rev (Buffer.contents buf :: acc)
  | `Malformed _ -> Uutf.Buffer.add_utf_8 buf Uutf.u_rep; loop d buf acc
  | `Await -> assert false
  in
  let nln = `Readline (Uchar.of_int 0x000A) in
  loop (Uutf.decoder ~nln ?encoding src) (Buffer.create 512) [];;



let load_file1 f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
 ignore(input ic s 0 n);

 let changelist = [("¬", "not"); ("∨", " \\/ ");  ("→", " -> "); ("∀", "forall "); 
                               ("∃", "exists ");  ("∧", "/\\ "); ("↔", "< - >") ;("∈", "iin");
                              ("∩", "oint") ;("∪", "ouni"); ( "⊆", "osubs"); ("∅" ,"empty") ;("∁","compl")] in
    let changelist1 = [("¬", "not");   ("→", " -> "); ("∀", "forall "); 
                               ("∃", "exists ");   ("↔", "< - >") ;("∈", "iin");
                              ("∩", "oint") ;("∪", "ouni"); ( "⊆", "osubs"); ("∅" ,"empty") ;("∁","compl")] in                          
  close_in ic;
  let s1 = (Processinputs.replacelist  (Bytes.to_string s) changelist ) in


(* print_string (Glib.Convert.locale_to_utf8 "∈");flush_all ();
 *)  
 let slist = Str.split (Str.regexp "") s1 in
  let valist =List.map (fun a -> if (Glib.Utf8.validate a) then a else "?") slist in
  let ss = String.concat "" valist in
(*   print_string (snd  (Glib.Convert.get_charset ()));flush_all ();
 *)  (* Utf8conv.utf8_of_windows1252 ~undefined:(fun a -> "?") s in *)
  (* String.concat "\n" (lines (`String s)) in
  if Utf8conv.is_windows1252 ss then
  (Printf.printf "\n --\n the text is\n %s \n ----" ss;flush_all())
   else (); *)
  if (Glib.Utf8.validate ss) then (Processinputs.replacelist ss (List.map (fun x-> (snd x, fst x)) changelist1))

else  "did not woek"

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
 ignore(input ic s 0 n);
  close_in ic;
  let s1 = (Processinputs.replacelist  (Bytes.to_string s) [("¬", "not"); ("∨", " \\/ ");  ("→", " -> "); ("∀", "forall "); 
                               ("∃", "exists ");  ("∧", "/\\ "); ("↔", "< - >")]) in


  (* let slist = Str.split (Str.regexp "") s1 in *)
  (* let valist =List.map (fun a ->flush_all (); if (Glib.Utf8.validate a) then a else "?") slist in *)
  (* let ss = String.concat "" valist in *)
  (* print_string (snd  (Glib.Convert.get_charset ()));flush_all (); *)
  (* Utf8conv.utf8_of_windows1252 ~undefined:(fun a -> "?") s in *)
  (* String.concat "\n" (lines (`String s)) in
  if Utf8conv.is_windows1252 ss then
  (Printf.printf "\n --\n the text is\n %s \n ----" ss;flush_all())
   else (); *)
  if (Glib.Utf8.validate s1) then s1

else  "did not woek"



let to_str x =
	match x with
	| Sexplib.Sexp.Atom a -> a
	| _ -> "" 

let to_li x =
	let to_list c =
	match c with
	| Sexplib.Sexp.List a -> a
	| _ -> [] in
	List.map (fun b -> List.map to_str (to_list b)) (to_list x);;

let commands () =
if Coqstuff.isMac () then
 Unix.chdir "/Applications/spatchcoq.app/Contents/MacOS" else ();

let s =load_file "commands.txt" in
let t = Sexplib.Sexp.of_string s in
let lii =  to_li t in
let com =List.map (fun x -> List.map (fun a-> Str.global_replace (Str.regexp "'") "\"" a ) x) lii in
let listofcommands= List.map (fun x -> List.map (fun a -> Printf.sprintf "%s" (Str.global_replace (Str.regexp "^\"\|\"$" ) "" a)) x) com in
let orderedlistofcommands= List.sort (fun x y -> String.compare (List.nth  x 1) (List.nth  y 1)) listofcommands in
orderedlistofcommands;;



(* 
let s= Core.Std.In_channel.read_all "commands.txt" in 
let t = Sexplib.Sexp.of_string s in
let lii= Core.Std.List.t_of_sexp (fun x-> Core.Std.List.t_of_sexp Core.Std.String.t_of_sexp x) t in
Core.Std.List.map lii (fun x-> Core.Std.List.map x (fun a-> Str.global_replace (Str.regexp "'") "\"" a ));;

 *)
(* 
let commands=[(" We will prove the left hand side of @conc{} that is we need to prove @newconclusion{1}.@latex{1} We are done with @newconclusion{1} and so @conc{}.", " Tactic Notation \"Prove\" \"left\" \"hand\" \"side\" := left.");
(" We will prove the right hand side of @conc{} that is we need to prove @newconclusion{1}.@latex{1} We are done with @newconclusion{1} and so @conc{}.", " Tactic Notation \"Prove\" \"right\" \"hand\" \"side\" := right.");
(" We will prove @newconclusion{1}.@latex{1} We are done with @newconclusion{1} and so @conc{}.", " Tactic Notation \"Prove\" constr(a) \"in\" \"the\" \"disjuntion\" := match goal with |-(a \/ ?b) => left | |-(?a \/ a) => right end.");
("Since we know @deadhyp{} we also know @newhyp{1}.@latex{1} We are now done with @conc{}.", " Tactic Notation \"Eliminate\" \"the\" \"conjuction\" \"in\" \"hypothesis\" hyp(a) :=match goal with| [a : (_ /\ _) |- _] => (let H1:=fresh \"Hyp\" in let H2:= fresh \"Hyp\" in destruct a as [H1 H2]) end.");
("Since we know @deadhyp{} we also know @newhyp{1}.@latex{1} We are now done with @conc{}.", "Tactic Notation \"Consider\" \"cases\" \"based\" \"on\" \"disjunction\" \"in\" \"hypothesis\" hyp(a) :=match goal with| [a : (_ \/ _) |- _] => (let H1:=fresh \"Hyp\" in let H2:=fresh \"Hyp\" in destruct a as [H1 | H2] ) end.");
("In order to prove @conc{} will first prove @newconclusion{1} and then @newconclusion{2}.First we show @newconclusion{1}.@latex{1} Next we show @newconclusion{2}.@latex{2} Since we showed @newconclusion{1} and @newconclusion{2} we also have @conc{}.", " Tactic Notation \"Prove\" \"the\" \"conjunction\" \"in\" \"the\" \"goal\" \"by\" \"first\" \"proving\" constr(a) \"then\" constr(b) := match goal with |-(a /\ b) => split end.");
("We will assume @newhyp{1} and show @newconclusion{1}.@latex{1} We have now showed that if @newhyp{1} then @newconclusion{1} a proof of @conc{}.", " Tactic Notation \"Assume\" constr(a) \"then\" \"prove\" constr(b) := match goal with |-(a -> b) => let H:= fresh \"Hyp\" in intro H end.");
("In order to show @conc{} we first show @newconclusion{1} and then @newconclusion{2}.First show @newconclusion{1}.@latex{1} Next show @newconclusion{2}.@latex{2} Since we showed both directions of the implication we have @conc{}.", " Tactic Notation \"Prove\" \"both\" \"directions\" \"of \" constr(a) \"iff \" constr(b) := match goal with |-(a <-> b) => split end.");
("In order to show @conc{} we pick an arbitrary @val{1} and show @newconclusion{1}.@latex{1} Since @val{1} was arbitrary this shows @conc{}.", " Tactic Notation \"Fix\" \"an\" \"arbitrary\" \"element\" ident(a) := match goal with |- forall _, _ => intro a end.");
("We choose a variable @val{1} in @val{2} to obtain @newhyp{1}.@latex{1} and so we have proved @conc{}.", " Tactic Notation \"Fix\" ident(a) \"the\" \"existentially\" \"quantified\" \"variable\" \"in\" hyp(H) := match goal with | [H:exists _, _|- _] => destruct H as [a H] end.");
("We immediately show @newhyp{1} by specialising @deadhyp{}.@latex{1} This finishes @conc{}.", " Tactic Notation \"Obtain\" constr(c) \"using\" \"variable\" constr(a) \"in\" \"the\" \"universally\" \"quantified\" \"hypothesis\" hyp(b) :=match goal with |[b: forall _, _ |- _] => let H0:=fresh \"Hyp\" in assert (H0:c) by apply (b a) end.");
("We shall prove @conc{} by showing @newconclusion{1}.@latex{1} Now @newconclusion{1} means that @conc{}.", " Tactic Notation \"Prove\" \"the\" \"existential\" \"claim\" \"is\" \"true\" \"for\" constr(a) := match goal with [|- exists _, _ ] => exists a end.");
("We rewrite the goal using @val{1} to obtain @newconclusion{1}.@latex{1} We have now proved @newconclusion{1} and so @conc{} follows.", "Tactic Notation \"Rewrite\" \"the\" \"goal\" \"using\" constr(b) := try rewrite b || rewrite <- b || apply b.");
("This follows immediately from arithmetic.@latex{1} ", " Tactic Notation \"True\" \"by\" \"arithmetic\" \"properties\" := match goal with |[|-_=_] => ring end.");
(" We will rewrite in @val{2} using @val{3} to obtain @newhyp{1}.@latex{1} Therefore we have @conc{}.", "Tactic Notation \"Claim\" constr(a) \"by\" \"rewriting\" hyp(c) \"using\" constr(b) := assert a by ((try (rewrite b in c) || (rewrite <- b in c));assumption).");
("We first prove @newconclusion{1}.@latex{1} and therefore we have proved @newconclusion{1}.@latex{2}.", " Tactic Notation \"Claim\" constr(a) := assert a.");
(" We use the definition of @val{2} in @val{1} to obtain @newhyp{1} @latex{1} therefore we have @conc{}.", " Tactic Notation \"Rewrite\" \"hypothesis\" ident(b) \"using\" \"the\" \"definition\" \"of\" reference(a) := unfold a in b.");
("Prove by induction.The initial case is @newconclusion{1}.@latex{1} Now assume @newhyp{2} and prove new @conclusion{2}.@latex{2} this finishes the induction.", " Tactic Notation \"Apply\" \"induction\" \"on\" constr(a) := induction a.");
 ("Rewriting the definition of @val{1} in @conc{}, we now need to show @newconclusion{1}.@latex{1} Therefore we have showed @newconclusion{1} and so @conc{}.", " Tactic Notation \"Rewrite\" \"goal\" \"using\" \"the\" \"definition\" \"of\" reference(a) := unfold a .");
 ("Now @val{3} and @val{2} imply @newhyp{1}.@latex{1} and so @conc{}.", " Tactic Notation \"obtain\" constr(a) \"applying\" constr(b) \"to\" hyp(c) :=assert a by apply (b c).");
 ("Assume that @conc{} is false, that is @newhyp{1} and prove a contradiction.@latex{1}.The conclusion @conc{} follows by contractiction.", " Tactic Notation \"Prove\" \"by\" \"contradiction\" := match goal with| |- ?A => let H:= fresh \"Hyp\" in assert (A\/not A) as H by apply (classic A); destruct H;[ assumption | exfalso] end.");
("This follows trivially from reflexivity.", " Tactic Notation \"This\" \"follows\" \"from\" \"reflexivity\" := reflexivity.");
("This follows trivially from symmetry.", " Tactic Notation \"This\" \"follows\" \"from\" \"symmetry\" := symmetry.");
("Apply theorem @val{1} to get @newconclusion{1}.@latex{1}", " Tactic Notation \"Apply\" \"result\" constr(a) := apply a.");
("Now @conc{} follows trivially from the assumptions.", " Tactic Notation \"This\" \"follows\" \"from\" \"assumptions\" := assumption.");
("We denote @val{1} by @val{2}.@latex{1}.", " Tactic Notation \"Denote\" constr(a) \"by\" ident(b) := remember a as b.")];;
 *)