minipurescript.exe:
	dune build minipurescript.exe

clean:
	dune clean

.PHONY: all clean minipurescript.exe