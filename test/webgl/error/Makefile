PP = OCAMLRUNPARAM=b ../../../gloc.d.byte -e

INS := $(wildcard *.in)
OUTS := $(INS:.in=.out)

%.out : %.in %.true
	@$(PP) < $*.in &> $@ || true
	@if diff -u $*.true $@ > $*.diff; then \
	rm $*.diff;\
	else cat $*.diff;\
	fi

.PHONY : all test clean
all :
	@$(MAKE) clean
	@$(MAKE) test
test : $(OUTS)
	@printf "Test failures: "
	@ls *.diff 2> /dev/null | wc -l
clean :
	@rm *.out *.diff 2> /dev/null || true
%.git : %.in %.out %.true
	git add $^
%.new :
	touch $*.in
	echo "error" > $*.true
