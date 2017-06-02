open Printf
open Treestuff
open Processresults
open Sexplib
open Sexplib.Std
open Logs


  let () =
    	let dir= Sys.argv.(1) in
    	let list = List.filter (fun x-> match Str.split (Str.regexp "\.") x with
    |[_;"log"] -> true
    |_ -> false )
    		(Array.to_list (Sys.readdir dir)) in

    	List.map (fun text ->
    		let ll = List.hd (Str.split (Str.regexp "\.") text) in
    		let sexp =Sexp.load_sexp (dir^"/"^text) in

    		let tt = Logs.loglist_of_sexp sexp in
    		(* List.map (fun x -> print_string (Logs.print_log x)) tt) list; *)
    		 
    		let str=  (String.concat "\n" (List.map (fun x -> (Logs.print_log x)) tt)) in
    		let file= open_out (dir^"/"^ll^".plog") in 
          Printf.fprintf file "%s\n" str; 
          close_out file
    	) list;
			()
    	;;






