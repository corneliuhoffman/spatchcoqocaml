

open Nethttp_client

let download url file =
  let pipeline = new pipeline in
  let get_call = new get url in
  get_call # set_response_body_storage (`File (fun () -> file));
  pipeline # add get_call;
  pipeline # run()
 

let run ()=

if not (Coqstuff.isWin ()) then
    Unix.chdir "/Applications/spatchcoq.app/Contents/MacOS" else ();


       download "http://spatchcoq.co.uk/spatchcoq/wp-content/uploads/2017/01/ver.tex" "ver.tex";
 flush_all();
 let oldversionfile = open_in "version" in 
     
      let oldver = (input_line oldversionfile) in
    let newversionfile = open_in "ver.tex" in 
     
      let newv = (input_line newversionfile) in
     
      close_in newversionfile;
      close_in oldversionfile;
if (int_of_string newv) != (int_of_string oldver) then
(let _ = GtkMain.Main.init () in
      let result =  GToolbox.question_box ~title:"Update"
       ~buttons:["update"; "skip"] ~default:0 "We found  a new version. Do you want to update?" in
       GtkMain.Main.quit ();
        print_int result;flush_all ();
       if result = 1 then
       (
        let out = open_out "version" in
       Printf.fprintf out "%s" newv; flush_all ();
       close_out  out;
       print_string "bfore"; flush_all ();
       download "http://spatchcoq.co.uk/spatchcoq/wp-content/uploads/2017/01/commands.txt" "commands.txt";

       if Coqstuff.isWin () then
        download "http://spatchcoq.co.uk/spatchcoq/wp-content/uploads/2017/01/spmain.exe" "spmain.exe"
      else
        download "http://spatchcoq.co.uk/spatchcoq/wp-content/uploads/2017/01/spmain" "spmain";

     )
);
(* if not (Coqstuff.isWin ()) then
    Unix.chmod (Filename.current_dir_name^"/spmain") 0o777
else ();; *)



(* let run ()=
	if not (Coqstuff.isWin ()) then

    Unix.chdir "/Applications/spatchcoq.app/Contents/MacOS" else ();

let fs1 = Nethttp_fs.http_fs "http://spatchcoq.co.uk/spatchcoq/wp-content/uploads/2017/01/"
in
let fs2 =  Netfs.local_fs ~enable_relative_paths:true () in 


let client = new Netftp_client.ftp_client() in    
client # exec (Netftp_client.connect_method ~host:"ftp.spatchcoq.co.uk" ());
   client # exec (Netftp_client.login_method ~user:"update@spatchcoq.co.uk"
                                ~get_password:(fun () -> "spatchcoq123")
                                ~get_account:(fun () -> "update@spatchcoq.co.uk") ());  
Netfs.copy (fs1 :> Netfs.stream_fs) "/version" (fs2 : Netfs.stream_fs) "./versionnew";



    let oldversionfile= open_in "version" in 
    let newversionfile = open_in "versionnew" in 
      let versionumber = (input_line oldversionfile) in
      let newv = (input_line newversionfile) in
      close_in oldversionfile;
      close_in newversionfile;

    (*   let bufferver = Buffer.create 1000 in


   let chver = new Netchannels.output_buffer bufferver in

   client # exec (Netftp_client.get_method ~file:(`NVFS "version")
                             ~representation:`Image
                             ~store:(fun _ -> `File_structure chver) ());
   let newv=Buffer.contents bufferver in
   print_string newv; flush_all (); *)
   if ( int_of_string newv) != (int_of_string versionumber) then


    (
    	let _ = GtkMain.Main.init () in
    	let result =  GToolbox.question_box ~title:"Update"
       ~buttons:["update"; "skip"] ~default:0 "We found  a new version. Do you want to update?" in
       GtkMain.Main.quit ();
        print_int result;flush_all ();
       if result = 1 then

		( let out = open_out "version" in
			 Printf.fprintf out "%s" newv; flush_all ();
			 close_out  out;
			Netfs.copy (fs1 :> Netfs.stream_fs) "/commands.txt" (fs2 : Netfs.stream_fs) "./commands.txt";

			 if Coqstuff.isWin () then
			 Netfs.copy (fs1 :> Netfs.stream_fs) "spmain.exe" (fs2 : Netfs.stream_fs) "spmain.exe"
			else 
			Netfs.copy (fs1 :> Netfs.stream_fs) "/spmain" (fs2 : Netfs.stream_fs) "./spmain";


			(* let spmain = if Coqstuff.isMac () then
    
        open_out (Filename.current_dir_name^"/spmain") else open_out (Filename.current_dir_name^"/spmain.exe") in
    
    
      
   
    
    
    
    
    let buffer = Buffer.create 1000 in
    
    let commands= open_out (Filename.current_dir_name^"/commands.txt") in
    
       let ch = new Netchannels.output_buffer buffer in
    if Coqstuff.isMac () then
       client # exec (Netftp_client.get_method ~file:(`NVFS "spmain")
                                 ~representation:`Image
                                 ~store:(fun _ -> `File_structure ch) ())
     else 
     client # exec (Netftp_client.get_method ~file:(`NVFS "spmain.exe")
                                 ~representation:`Image
                                 ~store:(fun _ -> `File_structure ch) ());
    Buffer.output_buffer spmain buffer;
    let buffercom = Buffer.create 1000 in
    let chcom = new Netchannels.output_buffer buffercom in
    client # exec (Netftp_client.get_method ~file:(`NVFS "commands.txt")
                                 ~representation:`Image
                                 ~store:(fun _ -> `File_structure chcom) ());
    
    Buffer.output_buffer commands buffercom;
    
    close_out commands;
    close_out spmain; *)
     *)

(* Unix.execv ("open -a /Applications/spatchcoq.app") [||] ();;
 *)
(* run "commands.txt";
GMain.Main.quit (); 
if Coqstuff.isMac () then

	run "spmain";


if Coqstuff.isWin () then
	run "spmain.exe"; *)

run ();

(* Sys.command ("/Applications/spatchcoq.app/Contents/MacOS/spmain");
 *)

(* Unix.execv ("/Applications/spatchcoq.app/Contents/MacOS/spmain")  [||] ();; *)
 exit 0