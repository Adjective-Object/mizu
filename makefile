all: mizu
.PHONY: clean test

TEMPLATE_DIR=images/road.jpg
OUTPUT_DIR=images/road.jpg
BUILD_PATH=dist/build

mizu: src/*
	cabal install --only-dependencies
	cabal configure
	cabal build
	cp $(BUILD_PATH)/mizu/mizu ./mizu

	@echo "Build Complete!" && echo ""

lint:
	hlint src/

test: mizu
	./mizu test/*.test -m x
	cat test/*.test.xgcm

testsvg: mizu
	./mizu test/fileopen.svg -m x

clean:
	rm -f *.o
	rm -f *.hi
	rm -f prism
	rm -f prism-prof
