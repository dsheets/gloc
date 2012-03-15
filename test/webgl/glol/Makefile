GLOC = OCAMLRUNPARAM=b ../../../src/gloc.d.byte

ARGSETS := $(wildcard *.argset)
ARGSYMS := $(ARGSETS:.argset=.argsym)

TESTS := $(wildcard *.tout)
OUTS  := $(TESTS:.tout=.out)
ERRS  := $(TESTS:.tout=.err)
DOUTS := $(TESTS:.tout=.dout)
DERRS := $(TESTS:.tout=.derr)
XMLS  := $(TESTS:.tout=.xml)

.PHONY : all test args clean html
all :
	@$(MAKE) clean
	@$(MAKE) args
	@$(MAKE) test

html : $(XMLS)
	xsltproc \
	--stringparam suite "`pwd | awk -F / '{ printf "%s/%s/%s/%s\n",$$5,$$6,$$7,$$8 }'`" \
	--stringparam tests "$(XMLS)" \
	html_of_tests.xsl test_page.xml > test_page.html

sym: sym.ml
	ocamlc -o sym sym.ml

# BEGIN test specifics

shadow_glom.001.out shadow_glom.001.err \
shadow_glom.002.out shadow_glom.002.err : \
shadow_glom.002.glo shadow_glom.001.glo
shadow_glom.001.glo : shadow_a.glsl shadow_b.glsl
	$(GLOC) -o shadow_glom.001.glo -c -v `cat shadow.001.args`
shadow_glom.002.glo : shadow_a.glsl shadow_b.glsl
	$(GLOC) -o shadow_glom.002.glo -c -v `cat shadow.002.args`

monkey.%.out monkey.%.err : monkey.glo monkey.%.args
	@$(GLOC) `cat monkey.$*.args` 2> monkey.$*.err > monkey.$*.out || true
monkey.glo : monkey_a.glsl monkey_b.glsl monkey_c.glsl
	$(GLOC) -o monkey.glo -c -v $^

# END test specifics

%.argsym : %.argset sym
	@rm -f $*.*.args
	@export N=1 && ./sym < $*.argset | while read -r args; do \
		touch `printf "$*.%03d.tout" $$N`; \
		touch `printf "$*.%03d.terr" $$N`; \
		echo "making $$N for $*"; \
		echo "$$args" > `printf "$*.%03d.args" $$N`; \
		true $$[N++]; \
	done
	@touch $*.argsym

%.out %.err : %.args
	@$(GLOC) `cat $*.args` 2> $*.err > $*.out || true

%.dout : %.tout %.out
	@if diff -u $*.tout $*.out > $*.dout; then true;\
	else cat $*.dout; \
	fi
%.derr : %.terr %.err
	@if diff -u $*.terr $*.err > $*.derr; then true;\
	else cat $*.derr; \
	fi

%.xml : %.tout %.terr %.out %.err %.args
	$(GLOC) --xml `cat $*.args` -o $*.glom.xml
	xsltproc \
	--stringparam name "$*" \
	--stringparam args "`cat $*.args`" \
	--stringparam tout "`awk '{ printf \"%s\\\\n\", $$0 }' $*.tout`" \
	--stringparam terr "`awk '{ printf \"%s\\\\n\", $$0 }' $*.terr`" \
	--stringparam out  "`awk '{ printf \"%s\\\\n\", $$0 }' $*.out`"  \
	--stringparam err  "`awk '{ printf \"%s\\\\n\", $$0 }' $*.err`"  \
	annotate_glom.xsl $*.glom.xml > $*.xml

args : $(ARGSYMS)
test : $(OUTS) $(DOUTS) $(ERRS) $(DERRS)
	@printf "Test failures: "
	@ls *.dout *.derr 2> /dev/null | file -f - | grep -v empty | wc -l
clean :
	@rm -f *.argsym *.out *.err *.dout *.derr *.glo *.???.xml *.glom.xml *.html
