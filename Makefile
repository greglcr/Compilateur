all: ppurs

ppurs.exe:
	dune build ppurs.exe

ppurs: ppurs.exe
	cp ppurs.exe ppurs

syntax: ppurs
	bash test.sh -1 ./ppurs

typing: ppurs
	bash test.sh -2 ./ppurs

exec: ppurs
	bash test.sh -3 ./ppurs

clean:
	rm ppurs
	dune clean

.PHONY: all clean ppurs.exe ppurs syntax