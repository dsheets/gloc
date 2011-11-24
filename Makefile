.PHONY: all clean

all: glol_js.js gloc.d.byte

%.js: %.byte
	js_of_ocaml $*.byte

glol_js.byte: glol_js.ml
	ocamlbuild -use-ocamlfind glol_js.byte

gloc.d.byte: standard.mly gloc.ml
	ocamlbuild -use-ocamlfind -use-menhir -menhir "menhir --stdlib .." gloc.d.byte

standard.mly: standard.mly.orig pptok.mly
	cp standard.mly.orig standard.mly
	cat pptok.mly >> standard.mly
	echo "%%" >> standard.mly

clean:
	ocamlbuild -clean
	@echo ""
	rm -f standard.mly glol_js.js
