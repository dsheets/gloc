.PHONY: all glocode clean

RM_ANNOT:=rm -f *.annot
OCAMLBUILD:=ocamlbuild -use-ocamlfind -cflag -annot
CP_ANNOT:=cp _build/*.annot .
DOC:=../doc
GLOC:=./gloc.d.byte
GLOCODE:=$(DOC)/glocode
CODEMIRROR:=../CodeMirror2

all: gloc.d.byte glol_js.d.js gloc_js.d.js

glocode: gloc_xml.d.byte
	rm -rf $(GLOCODE)
	mkdir -p $(GLOCODE)
	./gloc_xml.d.byte | xsltproc - > $(GLOCODE)/index.html
	xsltproc doc.xsl doc.xml > $(DOC)/index.html
	rm -rf $(GLOCODE)/CodeMirror2
	mkdir -p $(GLOCODE)/CodeMirror2/lib
	mkdir -p $(GLOCODE)/CodeMirror2/mode
	mkdir -p $(GLOCODE)/CodeMirror2/keymap
	cp -R $(CODEMIRROR)/lib $(GLOCODE)/CodeMirror2/
	cp -R $(CODEMIRROR)/mode $(GLOCODE)/CodeMirror2/
	cp -R $(CODEMIRROR)/keymap $(GLOCODE)/CodeMirror2/
	cp gloc.css $(DOC)/
	cp gloc_platform_js.js $(GLOCODE)/
	mkdir -p $(GLOCODE)/_build/
	make gloc_js.d.js
	cp _build/gloc_js.d.js $(GLOCODE)/_build/
	cp glol.js $(DOC)/
	make gloc.d.byte
	cd ../test/webgl/glol && make html
	mkdir -p $(DOC)/test/webgl/glol
	cp ../test/webgl/glol/test_page.html $(DOC)/test/webgl/glol/index.html
	cp ../test/webgl/glol/test_glol.js $(DOC)/test/webgl/glol/
	make glol_js.d.js
	mkdir -p $(DOC)/_build/
	cp _build/glol_js.d.js $(DOC)/_build/
	mkdir -p $(DOC)/Pretty-Diff/
	cp ../Pretty-Diff/prettydiff.js $(DOC)/Pretty-Diff/

gloc_js.d.js:
	$(RM_ANNOT)
	$(OCAMLBUILD) gloc_js.d.js
	$(CP_ANNOT)
gloc_js.js:
	$(RM_ANNOT)
	$(OCAMLBUILD) gloc_js.js
	$(CP_ANNOT)

glol_js.d.js:
	$(RM_ANNOT)
	$(OCAMLBUILD) glol_js.d.js
	$(CP_ANNOT)
glol_js.js:
	$(RM_ANNOT)
	$(OCAMLBUILD) glol_js.js
	$(CP_ANNOT)

gloc.d.byte:
	$(RM_ANNOT)
	$(OCAMLBUILD) gloc_posix.d.byte
	$(CP_ANNOT)
	mv gloc_posix.d.byte gloc.d.byte
gloc.byte:
	$(RM_ANNOT)
	$(OCAMLBUILD) gloc_posix.byte
	$(CP_ANNOT)
	mv gloc_posix.byte gloc.byte

gloc_xml.d.byte:
	$(RM_ANNOT)
	$(OCAMLBUILD) gloc_xml.d.byte
	$(CP_ANNOT)

clean:
	ocamlbuild -clean
	@echo ""
	rm -f glol_js.d.js gloc_js.d.js
