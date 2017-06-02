open Sexplib.Std
open Sexplib
type before = string [@@deriving sexp];;
type after = string  [@@deriving sexp];;
type messages = string  [@@deriving sexp];;
          



type logs = RUN of int * before * after * messages
			|CRUN of int * before * after * messages
             |UNDO of int * before * after * messages 
             |ESC of int * before 
         	|DROP of int * before [@@deriving sexp];;



let text_of_list li =
	let se = Sexplib.Std.sexp_of_list sexp_of_logs li in
	Sexplib.Sexp.to_string se;;
let list_of_text txt =
	let se = Sexp.of_string txt in
	 list_of_sexp logs_of_sexp se;;

let loglist_of_sexp se =
	
	 list_of_sexp logs_of_sexp se;;


let print_log log =
	match log with
	|RUN (a,b,c,d) -> Printf.sprintf " Button RUN was pressed at %i. Beforehand we had \n %s,\n afterward \n %s \n  the resulting messages = %s" a b c d
|CRUN (a,b,c,d) -> Printf.sprintf " CTRL_R was pressed at %i. Beforehand we had \n %s,\n afterward \n %s \n  the resulting messages = %s" a b c d

|UNDO (a,b,c,d) -> Printf.sprintf " Button UNDO was pressed at %i. Beforehand we had \n %s,\n afterward \n %s \n  the resulting messages = %s" a b c d
|ESC  (a,b) -> Printf.sprintf " Button ESC was pressed at %i. The completion was \n%s\n" a b 
|DROP  (a,b) -> Printf.sprintf " The drop down menu was used at %i. The choice was \n%s\n" a b 


          
