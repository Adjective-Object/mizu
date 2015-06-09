all: prism
.PHONY: clean

TEMPLATE_DIR=images/road.jpg
OUTPUT_DIR=images/road.jpg
BUILD_PATH=dist/build

prism: src/*
	cabal install --only-dependencies
	cabal configure
	cabal build
	cp $(BUILD_PATH)/mizu/mizu ./mizu

lint:
	hlint src/

clean:
	rm -f *.o
	rm -f *.hi
	rm -f prism
	rm -f prism-prof
