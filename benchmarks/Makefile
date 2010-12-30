include ../tests/common.mk

programs := bench

.PHONY: all
all: $(programs)

bench: $(lib) Benchmarks.o Util.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

.PHONY: clean
clean:
	-find . \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;
	-rm -f $(programs)

Benchmarks.o: Util.o
