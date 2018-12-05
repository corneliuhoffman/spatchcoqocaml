spmainmake:
	opam install  lambdasoup  lablgtk pcre  ppx_deriving ocamlnet  ppx_sexp_conv  sexplib  js_of_ocaml menhir
	ocamlbuild -use-ocamlfind -use-menhir "spmain.native"
