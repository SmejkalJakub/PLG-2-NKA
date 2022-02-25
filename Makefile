
flp21-fun: *.hs
	ghc plg-2-nka.hs -o flp21-fun

clear:
	rm -rf flp21-fun
	rm -rf *.o
	rm -rf *.hi