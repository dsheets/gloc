.PHONY: all clean

RM_ANNOT=rm -f *.annot
OCAMLBUILD=ocamlbuild -use-ocamlfind -cflag -annot
CP_ANNOT=cp _build/*.annot .

all: gloc.d.byte glol_js.d.js gloc_js.d.js

gloc_js.d.js:
	${RM_ANNOT}
	${OCAMLBUILD} gloc_js.d.js
	${CP_ANNOT}
gloc_js.js:
	${RM_ANNOT}
	${OCAMLBUILD} gloc_js.js
	${CP_ANNOT}

glol_js.d.js:
	${RM_ANNOT}
	${OCAMLBUILD} glol_js.d.js
	${CP_ANNOT}
glol_js.js:
	${RM_ANNOT}
	${OCAMLBUILD} glol_js.js
	${CP_ANNOT}

gloc.d.byte:
	${RM_ANNOT}
	${OCAMLBUILD} gloc_posix.d.byte
	${CP_ANNOT}
	mv gloc_posix.d.byte gloc.d.byte
gloc.byte:
	${RM_ANNOT}
	${OCAMLBUILD} gloc_posix.byte
	${CP_ANNOT}
	mv gloc_posix.byte gloc.byte

gloc_xml.d.byte:
	${RM_ANNOT}
	${OCAMLBUILD} gloc_xml.d.byte
	${CP_ANNOT}

clean:
	ocamlbuild -clean
	@echo ""
	rm -f glol_js.d.js gloc_js.d.js
