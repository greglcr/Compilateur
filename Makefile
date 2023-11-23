mini-pure-script.exe:
	dune build mini-pure-script.exe

clean:
	dune clean

.PHONY: all clean mini-pure-script.exe