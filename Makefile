
build:
	ghc main.hs -o flp21-fun

run: flp21-fun
	./flp21-fun

clear: flp21-fun
	rm -rf flp21-fun
	rm -rf main.hi
	rm -rf main.o