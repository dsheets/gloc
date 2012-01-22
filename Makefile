ATDGEN_SOURCES=glo.atd
include atdgen-make/Atdgen.mk

.PHONY: all clean

all: gloc.d.byte # glol_js.js

%.js: %.byte
	js_of_ocaml $*.byte

glol_js.byte: glol_js.ml $(ATDGEN_OUTFILES)
	ocamlbuild -use-ocamlfind glol_js.byte

gloc.d.byte: standard.mly gloc.ml $(ATDGEN_OUTFILES)
	ocamlbuild -use-ocamlfind -use-menhir -menhir "menhir --stdlib .." gloc.d.byte

standard.mly: standard.mly.orig pptok.mly
	cp standard.mly.orig standard.mly
	cat pptok.mly >> standard.mly
	echo "%%" >> standard.mly

clean:
	ocamlbuild -clean
	@echo ""
	rm -f standard.mly glol_js.js $(ATDGEN_OUTFILES)
