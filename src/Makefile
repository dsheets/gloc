.PHONY: all glocode clean

RM_ANNOT=rm -f *.annot
OCAMLBUILD=ocamlbuild -use-ocamlfind -cflag -annot
CP_ANNOT=cp _build/*.annot .
GLOCODE=../doc/glocode
CODEMIRROR=../CodeMirror2

all: gloc.d.byte glol_js.d.js gloc_js.d.js

glocode: gloc_xml.d.byte
	rm -rf ${GLOCODE}
	mkdir -p ${GLOCODE}
	./gloc_xml.d.byte | xsltproc - > ${GLOCODE}/index.html
	rm -rf ${GLOCODE}/CodeMirror2
	mkdir -p ${GLOCODE}/CodeMirror2/lib
	mkdir -p ${GLOCODE}/CodeMirror2/mode
	mkdir -p ${GLOCODE}/CodeMirror2/keymap
	cp -R ${CODEMIRROR}/lib ${GLOCODE}/CodeMirror2/
	cp -R ${CODEMIRROR}/mode ${GLOCODE}/CodeMirror2/
	cp -R ${CODEMIRROR}/keymap ${GLOCODE}/CodeMirror2/
	cp gloc.css ${GLOCODE}/
	cp gloc_platform_js.js ${GLOCODE}/
	mkdir -p ${GLOCODE}/_build/
	make gloc_js.d.js
	cp _build/gloc_js.d.js ${GLOCODE}/_build/

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
