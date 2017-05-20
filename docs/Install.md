# How to install spatchcoq

Spatchcoq is written in Ocaml.  See https://ocaml.org/

Spatchcoq requires ocaml version 4.02 or later.

To install ocaml see (http://www.ocaml.org/docs/install.html) and the opam package manager.  In debian, for example, type

    apt-get install ocaml opam gtk2.0

then start opam

    opam init
    eval `opam config env`

Now install the packages.

    opam install lablgtk
    opam install lambdasoup
    opam install pcre
    opam install sexplib
    

## Compile spatchcoq

To compile spatchcoq:

    ocamlfind opt  -o spatchcoq -linkpkg -package str,lambdasoup,lablgtk2,pcre,sexplib  processinputs.mli processresults.mli coqstuff.mli treestuff.mli commands.mli latexstuff.mli spmain.mli

    ocamlfind opt  -o spatchcoq -linkpkg -package str,lambdasoup,lablgtk2,pcre,sexplib  processinputs.ml processresults.ml coqstuff.ml treestuff.ml commands.ml latexstuff.ml spmain.ml 

or shorter
    ocamlbuild -use-ocamlfind 'spmain.native'
    mv spmain.native spatchcoq