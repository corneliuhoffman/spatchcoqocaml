


if Coqstuff.isMac () then
(Sys.command ("/Applications/spatchcoq.app/Contents/MacOS/update");

Sys.command ("/Applications/spatchcoq.app/Contents/MacOS/spmain") 
)
else 
	(Sys.command ("update.exe");

Sys.command ("spmain.exe") )