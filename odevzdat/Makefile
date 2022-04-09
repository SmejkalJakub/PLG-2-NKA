
flp21-fun: src/*.hs
	ghc -Wall src/*.hs -o flp21-fun

clear:
	rm -rf flp21-fun
	rm -rf src/*.o
	rm -rf src/*.hi

tests: flp21-fun
	cd test/automatic && ./test.sh