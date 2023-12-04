all: ppurs.exe

ppurs.exe:
	dune build ppurs.exe

syntax: ppurs.exe
	bash test.sh -1 "dune exec ./ppurs.exe"

clean:
	dune clean

.PHONY: all clean ppurs.exe