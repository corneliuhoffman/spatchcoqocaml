open GMain
open GdkKeysyms
open Marker
type feedback ={students:int; cheataverage:float; worstcheat:Marker.cheat}
type score ={student:string; totalscore:float}
type lemma ={name:string; score:float; pen:float}
let remove_book notebook () =
  while notebook#current_page >=0 do
    notebook#remove_page notebook#current_page
  done;
  ()

let limax f  a list = List.fold_left (fun x y -> max x y) a list;; 

let computescores strings lems =
	 List.map (fun student ->
		let filteredresults = List.filter (fun x-> Pcre.pmatch ~rex:(Pcre.regexp "Lemma|Proposition|Theorem") x.title) student.results in
		let scores  =List.map (fun x-> 
			let thexlemma = List.filter (fun a-> print_string x.title;flush_all (); a.name  = x.title) lems in
			if thexlemma != [] then 
			let lem = List.hd thexlemma in 
			lem.score *. (100. -. 
			(List.fold_left (fun a b -> a +. lem.pen *. b.penalty /. 100.) 0. x.cheats))/. 100.
		 else 0.) filteredresults in
		{student=student.id; totalscore = List.fold_left (fun x y -> x +. y) 0. scores}
	
	) strings 

let modifylemmas score penalty label lemmas =
	let sc = try float_of_string score with _ -> 100. in
	let pen = try float_of_string penalty with _ -> 100. in
	List.map ( fun x-> if x.name = label then {name = x.name ;score = sc; pen = pen} else x) !lemmas



(* let process thm strings =
		let str = List.filter (fun st ->  List.mem thm (Marker.get_lemas [st])) strings in
		let cheats = List.map (fun student -> 
		let results = student.results in 
		let cheats = List.hd (List.filter (fun res -> 
			(* let strings = Str.split (Str.regexp " \|(") (String.trim res.title) in
			let title = String.concat " "  (firsttwo (List.filter (fun x-> x!="") strings)) in  *)
			thm = res.title) results) in
			cheats.cheats 
	) str in 
		let max = limax max 0. (List.map (fun x -> limax max 0.  (List.map (fun a ->a.penalty) x)) cheats) in 
		let listofcheats = List.concat (List.map (fun x-> List.filter (fun a-> a.penalty = max) x) cheats) in
		
		let worstcheats = String.concat "\n" (List.map (fun x-> Printf.sprintf "\n %s \n Penalty = %f\n"  x.text x.penalty) listofcheats) in
		 {students = List.length str; cheataverage =(float_of_int (List.fold_left (fun x y -> x + y) 0 (List.map List.length cheats)))/. (float_of_int (List.length str));worstcheat= {penalty=max; text =worstcheats} }
 *)
let cleanstr str=
Str.global_replace (Str.regexp "\"") "" str;;

let main () = 
  print_string "ha\n\n\n\n\n"; flush_all ();
let dir = ref "" in
let strings = ref [] in
let scores: score list ref = ref [] in
let lemmas: lemma list ref = ref[] in
let _ = GtkMain.Main.init () in
      let filew = GWindow.file_selection ~title:"File selection" ~border_width:10  
          ~filename:"" () in
      ignore(filew#ok_button#connect#clicked ~callback:( 
        fun () -> 
          dir := filew#filename; 
          (* print_string dir;
          strings := Marker.readdir dir;
          let lems = (Marker.get_lemas !strings) in
          lemmas := List.map (fun x -> {name = x; score=100.; pen =100.}) lems;
          scores:= computescores !strings !lemmas; *)
          filew#destroy (); 
          Main.quit ()));


      ignore(filew#cancel_button#connect#clicked ~callback:filew#destroy);
      filew#show ();
      Main.main ();


let  allfiles = readdir !dir in
let allthms = (List.concat (List.map (fun x->Marker.getlemas (!dir^"/"^x)) allfiles)) in

let thmnames = (List.map (fun (a,b,c)-> (a,b)) (Marker.red allthms)) in
let theorems = List.map (fun (a,b)-> let s = String.concat "\n" (List.map (fun (x,y,z)->(cleanstr z)) (List.filter (fun (x,y,z)-> x = a) allthms)) in (a,b,s)) thmnames in

let _ = GtkMain.Main.init () in
let window = GWindow.window ~width:1200 ~height:600
      ~title:"Feedback" () in
      ignore(window#connect#destroy ~callback:Main.quit);
let vbox = GPack.vbox  ~packing:window#add () in

let notebook = GPack.notebook ~tab_pos:`TOP ~height:550 
      ~packing:vbox#pack  () in

  let addListToNotebook  list = 
    remove_book notebook ();
    List.map (fun ( label,text, errors) ->
        (* let text =  (snd x) in *)
        let label = GMisc.label ~text:label () in

        let frame = GPack.vbox  ~width:100 ~height:550 ~border_width:10
            ~packing:(fun x-> Printf.printf  "%s" (string_of_int (notebook#append_page ~tab_label:label#coerce x))) () in
        let nframe = GBin.scrolled_window ~placement:`TOP_LEFT ~width:100 ~height:500 ~border_width:10
            ~packing:(frame#pack) () in
        let view =GText.view    ~packing:(nframe#add_with_viewport)() in
        print_string (String.escaped errors);flush_all ();

        view#buffer#set_text (text^errors);
        view#misc#modify_base [(`NORMAL, `NAME "lightgray")];
        let hbox = GPack.hbox ~packing:frame#add ()~height:50 ~homogeneous:true ~spacing:100  in 
		let score = GEdit.entry ~editable:true ~height:30 ~width:100  ~packing:(hbox#pack ~expand:false ~fill:true ~padding:100) () in
        score#set_text "score";
        score#misc#modify_base [(`NORMAL, `NAME "green")];
        let penalty = GEdit.entry ~editable:true ~height:30 ~width:100~packing:(hbox#pack ~expand:false ~fill:true ~padding:100) () in
        penalty#set_text "penalty";
        penalty#misc#modify_base [(`NORMAL, `NAME "lightgreen")];
        let buttonrun = GButton.button ~label:"apply!" 
      ~packing:(hbox#pack ~expand:false ~fill:true ~padding:100) () in
  ignore (buttonrun#connect#clicked ~callback:(fun ()-> 
lemmas:= modifylemmas  score#text penalty#text label#text lemmas
   ;flush_all (); ()
 ))
    ) list in

(* let numberofstr = List.length !strings in
print_int numberofstr; flush_all ();
let lems = (Marker.get_lemas !strings) in
let list =List.map (fun lem -> 
	let proc  = process lem !strings in 
	(lem ,Printf.sprintf "%i students tried it, the average number of cheats was %f and the worst cheats were \n%s"
proc.students proc.cheataverage proc.worstcheat.text)) lems in  *)
addListToNotebook theorems;
let buttonrun = GButton.button ~label:"process marks"
      ~packing:vbox#pack () in
  ignore (buttonrun#connect#clicked ~callback:(fun () -> (* let lab = notebook#get_nth_page 0 in
  	
  	let txt =  notebook#children in
  	let firstpage = (notebook#get_nth_page 0) in
  	let obj = GtkPack.Box.cast firstpage#as_widget in
  	let x = new GPack.box obj in
  	let a  = List.nth x#children 1 in
  	let obj1 = GtkPack.Box.cast a#as_widget in
  	let x1 = new GPack.box obj1 in
  	let aa = List.nth x1#children 0 in
  	let  obj2 = GtkText.View.cast aa#as_widget in
  	let x2 = new GText.view obj2 in
  	print_string (x2#buffer#get_text ()) ; *)
  	scores:= computescores !strings !lemmas;
	let filew = GWindow.file_selection ~title:"File selection" ~border_width:10  
          ~filename:"exercise.cvs" () in
      ignore(filew#ok_button#connect#clicked ~callback:(fun () ->
          let file=filew#filename in
          let out = open_out file in
           let printscores = String.concat "\n" (List.map (fun x -> Printf.sprintf "\"%s\", %f" x.student x.totalscore) !scores) in 

          Printf.fprintf out "%s" printscores;
          close_out out; 
          filew#destroy ()
        ));

      ignore(filew#cancel_button#connect#clicked ~callback:filew#destroy);
      filew#show ();


  	()));


window#show ();

 Main.main ();;







let () = main ()


(* let main () = 

let strings = ref [] in
let scores: score list ref = ref [] in
let lemmas: lemma list ref = ref[] in
let _ = GtkMain.Main.init () in
      let filew = GWindow.file_selection ~title:"File selection" ~border_width:10  
          ~filename:"" () in
      ignore(filew#ok_button#connect#clicked ~callback:( 
        fun () -> 
          let dir = filew#filename in 
          print_string dir;
          strings := Marker.readdir dir;
          let lems = (Marker.get_lemas !strings) in
          lemmas := List.map (fun x -> {name = x; score=100.; pen =100.}) lems;
          scores:= computescores !strings !lemmas;
          filew#destroy (); 
          Main.quit ()));

      ignore(filew#cancel_button#connect#clicked ~callback:filew#destroy);
      filew#show ();
      Main.main ();
let _ = GtkMain.Main.init () in
let window = GWindow.window ~width:1200 ~height:600
      ~title:"Spatchcoq" () in
      ignore(window#connect#destroy ~callback:Main.quit);
let vbox = GPack.vbox  ~packing:window#add () in

let notebook = GPack.notebook ~tab_pos:`TOP ~height:550 
      ~packing:vbox#pack  () in

  let addListToNotebook  list = 
    remove_book notebook ();
    List.map (fun x-> 
        let text = ( snd x ) in
        let label = GMisc.label ~text:(fst x) () in

        let frame = GPack.vbox  ~width:100 ~height:550 ~border_width:10
            ~packing:(fun x-> Printf.printf "%s" (string_of_int (notebook#append_page ~tab_label:label#coerce x))) () in
        let view = GText.view ~editable:false ~height:450 ~packing:(frame#pack  ~fill:true)() in
        view#buffer#set_text text;
        view#misc#modify_base [(`NORMAL, `NAME "lightgray")];
        let hbox = GPack.hbox ~packing:frame#pack ()~height:50 ~homogeneous:true ~spacing:100  in 
    let score = GEdit.entry ~editable:true ~height:30 ~width:100  ~packing:(hbox#pack ~expand:false ~fill:true ~padding:100) () in
        score#set_text "score";
        score#misc#modify_base [(`NORMAL, `NAME "green")];
        let penalty = GEdit.entry ~editable:true ~height:30 ~width:100~packing:(hbox#pack ~expand:false ~fill:true ~padding:100) () in
        penalty#set_text "penalty";
        penalty#misc#modify_base [(`NORMAL, `NAME "lightgreen")];
        let buttonrun = GButton.button ~label:"apply!" 
      ~packing:(hbox#pack ~expand:false ~fill:true ~padding:100) () in
  ignore (buttonrun#connect#clicked ~callback:(fun ()-> 
lemmas:= modifylemmas  score#text penalty#text label#text lemmas
   ;flush_all (); ()
 ))
    ) list in

let numberofstr = List.length !strings in
print_int numberofstr; flush_all ();
let lems = (Marker.get_lemas !strings) in
let list =List.map (fun lem -> 
  let proc  = process lem !strings in 
  (lem ,Printf.sprintf "%i students tried it, the average number of cheats was %f and the worst cheats were \n%s"
proc.students proc.cheataverage proc.worstcheat.text)) lems in 
addListToNotebook list;
let buttonrun = GButton.button ~label:"process marks"
      ~packing:vbox#pack () in
  ignore (buttonrun#connect#clicked ~callback:(fun () -> (* let lab = notebook#get_nth_page 0 in
    
    let txt =  notebook#children in
    let firstpage = (notebook#get_nth_page 0) in
    let obj = GtkPack.Box.cast firstpage#as_widget in
    let x = new GPack.box obj in
    let a  = List.nth x#children 1 in
    let obj1 = GtkPack.Box.cast a#as_widget in
    let x1 = new GPack.box obj1 in
    let aa = List.nth x1#children 0 in
    let  obj2 = GtkText.View.cast aa#as_widget in
    let x2 = new GText.view obj2 in
    print_string (x2#buffer#get_text ()) ; *)
    scores:= computescores !strings !lemmas;
  let filew = GWindow.file_selection ~title:"File selection" ~border_width:10  
          ~filename:"exercise.cvs" () in
      ignore(filew#ok_button#connect#clicked ~callback:(fun () ->
          let file=filew#filename in
          let out = open_out file in
           let printscores = String.concat "\n" (List.map (fun x -> Printf.sprintf "\"%s\", %f" x.student x.totalscore) !scores) in 

          Printf.fprintf out "%s" printscores;
          close_out out; 
          filew#destroy ()
        ));

      ignore(filew#cancel_button#connect#clicked ~callback:filew#destroy);
      filew#show ();


    ()));


window#show ();

 Main.main ();; *)


