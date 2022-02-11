
flp21-fun:
	ghc Main.hs -o flp21-fun

file: flp21-fun
	./flp21-fun -i test.txt

stdin: flp21-fun
	./flp21-fun -i

clear:
	rm -rf flp21-fun
	rm -rf Main.hi
	rm -rf Main.o