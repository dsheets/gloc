all:
	ocamlbuild -use-ocamlfind -use-menhir esslpp_main.d.byte

clean:
	ocamlbuild -clean
