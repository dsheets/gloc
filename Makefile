all:
	ocamlbuild -use-ocamlfind -use-menhir esslpp_main.byte

clean:
	ocamlbuild -clean