
flp21-fun: *.hs
	ghc Main.hs -o flp21-fun

file: flp21-fun
	./flp21-fun -1 test.txt

stdin: flp21-fun
	./flp21-fun -1

clear:
	rm -rf flp21-fun
	rm -rf *.o
	rm -rf *.hi