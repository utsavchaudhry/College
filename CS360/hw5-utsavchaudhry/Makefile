CABAL ?= cabal
GHC ?= ghc
STACK ?= stack

.PHONY : all
all :
	$(STACK) test --no-run-tests

.PHONY : clean
clean :
	rm -rf .stack-work

.PHONY : test
test :
	$(STACK) test
