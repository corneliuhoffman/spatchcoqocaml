spmainmake:
	opam install  lambdasoup  lablgtk pcre  ppx_deriving ocamlnet  ppx_sexp_conv  sexplib  js_of_ocaml
	ocamlbuild -use-ocamlfind -use-menhir "spmain.native"
