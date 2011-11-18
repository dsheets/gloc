.PHONY: all clean
all: standard.mly
	ocamlbuild -use-ocamlfind -use-menhir -menhir "menhir --stdlib .." gloc.d.byte

standard.mly: standard.mly.orig pptok.mly
	cp standard.mly.orig standard.mly
	cat pptok.mly >> standard.mly
	echo "%%" >> standard.mly

clean:
	ocamlbuild -clean
	@echo ""
	rm -f standard.mly
