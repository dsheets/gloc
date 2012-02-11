.PHONY: all clean

all: gloc.d.byte # glol_js.d.js

%.js: %.byte
	js_of_ocaml -pretty -noinline $*.byte

glol_js.d.byte: glol_js.ml
	ocamlbuild -use-ocamlfind glol_js.d.byte

gloc.d.byte: gloc.ml
	ocamlbuild -use-ocamlfind gloc.d.byte

clean:
	ocamlbuild -clean
	@echo ""
	rm -f glol_js.d.js
