PP = ../../esslpp_main.byte

INS := $(wildcard *.in)
OUTS := $(INS:.in=.out)

%.out : %.in %.true
	@$(PP) < $*.in > $@
	@echo $*
	@diff $@ $*.true || true

.PHONY : all test clean
all :
	@$(MAKE) clean
	@$(MAKE) test
test : $(OUTS)
clean :
	@rm *.out || true
%.git : %.in %.out %.true
	git add $^
%.new :
	touch $*.in
	echo "error" > $*.true
