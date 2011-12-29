GLOC = OCAMLRUNPARAM=b ../../../gloc.d.byte

ARGSETS := $(wildcard *.argset)
ARGSYMS := $(ARGSETS:.argset=.argsym)

TESTS := $(wildcard *.tout)
OUTS  := $(TESTS:.tout=.out)
ERRS  := $(TESTS:.tout=.err)
DOUTS := $(TESTS:.tout=.dout)
DERRS := $(TESTS:.tout=.derr)

.PHONY : all test args clean
all :
	@$(MAKE) clean
	@$(MAKE) args
	@$(MAKE) test

sym: sym.ml
	ocamlc -o sym sym.ml

%.argsym : %.argset sym
	@rm -f $*.*.args
	@N=1
	@./sym < $*.argset | while read -r args; do \
		touch $*.$$N.tout; \
		touch $*.$$N.terr; \
		echo "$$args" > $*.$$[N++].args; \
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

args : $(ARGSYMS)
test : $(OUTS) $(DOUTS) $(ERRS) $(DERRS)
	@printf "Test failures: "
	@ls *.dout *.derr 2> /dev/null | file -f - | grep -v empty | wc -l
clean :
	@rm -f *.argsym *.out *.err *.dout *.derr
