.PHONY: all clean

all: gloc.d.byte # glol_js.js

%.js: %.byte
	js_of_ocaml $*.byte

glol_js.byte: glol_js.ml
	ocamlbuild -use-ocamlfind glol_js.byte

gloc.d.byte: gloc.ml
	ocamlbuild -use-ocamlfind gloc.d.byte

clean:
	ocamlbuild -clean
	@echo ""
	rm -f glol_js.js
