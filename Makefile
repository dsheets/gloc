all:
	ocamlbuild -use-ocamlfind glsl_lex.byte

clean:
	ocamlbuild -clean