all:
	ocamlbuild -use-ocamlfind -use-menhir gloc.d.byte

clean:
	ocamlbuild -clean
