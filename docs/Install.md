# How to install spatchcoq

Spatcoq is written in Ocaml.  See https://ocaml.org/

To install ocaml see (http://www.ocaml.org/docs/install.html) and the opam package manager.  In debian, for example, type

    apt-get install ocaml opam gtk2.0

then start opam

    opam init
    eval `opam config env`

Now install the packages.

    opam install lablgtk
    opam install lambdasoup
