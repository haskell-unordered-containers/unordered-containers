package := unordered-containers
version := $(shell awk '/^version:/{print $$2}' ../$(package).cabal)
lib := ../dist/build/libHS$(package)-$(version).a
ghc := ghc
ghc-flags := -Wall -O -hide-all-packages \
	-package-conf ../dist/package.conf.inplace -package base -package mtl \
	-package unordered-containers -package containers -package criterion \
	-package deepseq -package hashable -package random -package bytestring

%.o: %.hs
	$(ghc) $(ghc-flags) -c -o $@ $<
