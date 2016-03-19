BINS:=interprete

_: ${BINS}

%: %.hs
	ghc -O2 -threaded -eventlog $^

clean:
	rm ${BINS} *.hi *.o
