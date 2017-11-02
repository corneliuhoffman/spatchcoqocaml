open Sexplib.Std
open Sexplib
type before = string [@@deriving sexp];;
type after = string  [@@deriving sexp];;
type messages = string  [@@deriving sexp];;
          



type logs = RUN of int * before * after * messages
			|CRUN of int * before * after * messages
             |UNDO of int * before * after * messages 
             |ESC of int * before 
             |PASTED of int*before
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
	|RUN (a,b,c,d) -> Printf.sprintf "\n---------------\n Button RUN was pressed at %i.\n Beforehand we had \n \n++++++++++\n%s\n++++++++++\n\n afterward we had \n++++++++++\n \n %s \n \n++++++++++\n the resulting messages = %s" a b c d
|CRUN (a,b,c,d) -> Printf.sprintf "\n---------------\n CTRL_R was pressed at %i.\n Beforehand we had \n \n++++++++++\n%s\n++++++++++\n\n afterward we had \n++++++++++\n \n %s \n \n++++++++++\n the resulting messages = %s" a b c d

|UNDO (a,b,c,d) -> Printf.sprintf "\n---------------\n Button UNDO was pressed at %i. \n Beforehand we had \n++++++++++\n \n %s \n++++++++++\n\n afterward we had \n++++++++++\n \n %s \n++++++++++\n\n  the resulting messages = %s" a b c d
|ESC  (a,b) -> Printf.sprintf "\n---------------\n Button ESC was pressed at %i.\n The completion was \n%s\n" a b 
|PASTED (a,b) ->Printf.sprintf "\n---------------\n At moment %i the user pasted the following text \n++++++++++\n \n %s\n++++++++++\n \n" a b
|DROP  (a,b) -> Printf.sprintf "\n---------------\n The drop down menu was used at %i.\n The choice was \n++++++++++\n \n%s \n++++++++++\n\n" a b 


          
