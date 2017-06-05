open GMain
open GdkKeysyms

let remove_book notebook () =
  while notebook#current_page >=0 do
    notebook#remove_page notebook#current_page
  done;
  ();;

let main () = 
let strings = ref [] in
let _ = GtkMain.Main.init () in
      let filew = GWindow.file_selection ~title:"File selection" ~border_width:10  
          ~filename:"" () in
      ignore(filew#ok_button#connect#clicked ~callback:( 
        fun () -> 
          let dir = filew#filename in 
          print_string dir;
          strings := Marker.readdir dir;
          filew#destroy (); 
          Main.quit ()));

      ignore(filew#cancel_button#connect#clicked ~callback:filew#destroy);
      filew#show ();
      Main.main ();
let _ = GtkMain.Main.init () in
let window = GWindow.window ~width:1200 ~height:600
      ~title:"Spatchcoq" () in
      ignore(window#connect#destroy ~callback:Main.quit);


let notebook = GPack.notebook ~tab_pos:`TOP
      ~packing:window#add  () in

  let addListToNotebook  list = 
    remove_book notebook ();
    List.map (fun x-> 
        let text = x in
        let label = GMisc.label ~text:( x ) () in
        let frame = GBin.frame ~label:"" ~width:100 ~height:75 ~border_width:10
            ~packing:(fun x-> Printf.printf "%s" (string_of_int (notebook#append_page ~tab_label:label#coerce x))) () in
        let view = GText.view ~editable:false  ~packing:frame#add () in
        view#buffer#set_text text;
        view#misc#modify_base [(`NORMAL, `NAME "lightgray")]) list in
addListToNotebook (Marker.get_lemas !strings);

window#show ();

 Main.main ();;







let () = main ()