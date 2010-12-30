include common.mk

programs := tests

.PHONY: all
all: $(programs)

tests: ghc-flags += -package QuickCheck
tests: $(lib) Properties.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

.PHONY: clean
clean:
	-find . \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;
	-rm -f $(programs)
