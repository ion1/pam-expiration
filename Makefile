sources := $(shell find . -name '*.hs')

binary := pam-expiration

.PHONY : all
all :
	ghc -isrc -o '$(binary)' --make Main

.PHONY : clean
clean ::
	$(RM) '$(binary)' $(sources:.hs=.o) $(sources:.hs=.hi)
