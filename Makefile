.PHONY: all clean

all: gloc.d.byte glol_js.d.js gloc_js.d.js

gloc_js.d.js:
	ocamlbuild -use-ocamlfind gloc_js.d.js

glol_js.d.js:
	ocamlbuild -use-ocamlfind glol_js.d.js

gloc.d.byte:
	ocamlbuild -use-ocamlfind gloc_posix.d.byte
	mv gloc_posix.d.byte gloc.d.byte

gloc_xml.d.byte:
	ocamlbuild -use-ocamlfind gloc_xml.d.byte

clean:
	ocamlbuild -clean
	@echo ""
	rm -f glol_js.d.js gloc_js.d.js
