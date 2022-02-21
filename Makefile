
flp21-fun: *.hs
	ghc plg-2-nka.hs -o flp21-fun

file: flp21-fun
	./flp21-fun -2 test.txt

stdin: flp21-fun
	./flp21-fun -2

clear:
	rm -rf flp21-fun
	rm -rf *.o
	rm -rf *.hi