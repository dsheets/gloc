.PHONY: all clean

all: gloc.d.byte glol_js.d.js gloc_js.d.js

gloc_js.d.js: gloc_js.ml
	ocamlbuild -use-ocamlfind gloc_js.d.js

glol_js.d.js: glol_js.ml
	ocamlbuild -use-ocamlfind glol_js.d.js

gloc.d.byte: gloc_posix.ml
	ocamlbuild -use-ocamlfind gloc_posix.d.byte
	mv gloc_posix.d.byte gloc.d.byte

clean:
	ocamlbuild -clean
	@echo ""
	rm -f glol_js.d.js gloc_js.d.js
