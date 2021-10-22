PROG=mascarpone

all: exe web

exe: bin/$(PROG).exe

bin/$(PROG).exe:
ifeq (, $(shell command -v ghc 2>/dev/null))
	echo "ghc not found in PATH, skipping exe build"
else
	(cd src && ghc --make Main.hs -o ../bin/$(PROG).exe)
endif

web: demo/$(PROG).js

demo/$(PROG).js:
ifeq (, $(shell command -v hastec 2>/dev/null))
	echo "hastec not found in PATH, skipping web build"
else
	(cd src && hastec --make HasteMain.hs -o $(PROG).js && mv $(PROG).js ../demo/$(PROG).js)
endif

clean:
	rm -f bin/$(PROG).exe demo/$(PROG).js
	find . -name '*.o' -exec rm {} \;
	find . -name '*.hi' -exec rm {} \;
	find . -name '*.jsmod' -exec rm {} \;
