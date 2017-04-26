let pr fmt = Printf.ksprintf print_endline fmt
let model_of_list conv l =
  let cols = new GTree.column_list in
  let column = cols#add conv in
  let model = GTree.list_store cols in
  List.iter
    (fun data ->
      let row = model#append () in
      model#set ~row ~column data)
    l ;
  (model, column)

let string_completion_list = [ "hello" ; "hello world" ; "abcdef" ; "abcxyz" ]

let stock_completion_list = [ `HOME ; `GO_BACK ; `GO_FORWARD ; `DIALOG_WARNING ; `DIALOG_ERROR ]

let setup packing (make_entry : (GObj.widget -> unit) -> GEdit.entry) =
  let box = GPack.hbox ~packing () in
  let entry = make_entry box#pack in
  ()
  
let make_simple_entry packing =
  let entry = GEdit.entry ~packing () in
  let (model, col) = model_of_list Gobject.Data.string string_completion_list in
  let c = GEdit.entry_completion  ~model ~entry () in
  c#set_text_column col ;
  c#insert_action_markup 0 "<span foreground='blue'>action 0</span>" ;
  c#insert_action_markup 1 "<span foreground='red' >action 1</span>" ;
  entry

let is_prefix s1 s2 =
  (String.length s1 <= String.length s2) && (String.sub s2 0 (String.length s1) = s1)




let main () = 
  GMain.init ();
  let w = GWindow.window ~title:"GtkEntryCompletion demo" () in
  w#connect#destroy GMain.quit ;

  let _ = w#event#connect#key_press ~callback:begin fun ev ->
   let m = GdkEvent.Key.state ev in
   let k = GdkEvent.Key.keyval ev in
   if (List.mem `CONTROL m && k = GdkKeysyms._Tab) then pr "WOO HOO";flush_all ();
   if (List.mem `CONTROL m && k = GdkKeysyms._Escape) then pr "woo hoo"; flush_all ();
   false
  end in
  w#event#add [`KEY_PRESS];
  begin
    let box = GBin.scrolled_window ~packing:w#add () in
    setup box#add_with_viewport  make_simple_entry ;
  end ;
  w#show () ;
  GMain.main ()

let _ = main ()