open Gobject.Data
let locale = GtkMain.Main.init ()
(* file: icon.ml *)

let cols = new GTree.column_list
let col_icon: GdkPixbuf.pixbuf GTree.column = cols#add Gobject.Data.gobject
let col_text = cols#add Gobject.Data.string

let create_liststore () =
  let store = GTree.list_store cols in

  let icon = GdkPixbuf.from_file "gtk.xpm" in
  let row = store#append () in
  store#set ~row ~column:col_icon icon;
  store#set ~row ~column:col_text "example";
  store

let create_treeview ~packing () =
  let model = create_liststore () in

  let view = GTree.view ~model ~packing () in

  let renderer = (GTree.cell_renderer_pixbuf [], [("pixbuf", col_icon)]) in
  let col = GTree.view_column ~title:"Title" ~renderer () in
  view#append_column col;

  let renderer_text = GTree.cell_renderer_text [] in
  let col = GTree.view_column ~title:"Text"
      ~renderer:(renderer_text, [("text", col_text)]) () in
  view#append_column col;

  view


let main () =
  let window = GWindow.window ~title:"Name" ~border_width:10 () in
  window#connect#destroy ~callback:GMain.Main.quit;
  create_treeview ~packing:window#add ();
  window#show ();
  GMain.Main.main ()

let _ = Printexc.print main ()