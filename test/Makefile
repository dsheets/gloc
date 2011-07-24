DIRS := $(shell ls -l | grep '^d' | awk '{printf "%s.dir\n",$$9 }' )

%.dir :
	cd $*; $(MAKE)

.PHONY : all test
all :
	@$(MAKE) test
test : $(DIRS)
