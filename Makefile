all: ppurs

ppurs: prt
	dune build src/ppurs.exe
	@cp src/ppurs.exe ppurs

prt:
	@make -C prt

test-syntax: ppurs
	@(cd test && bash test.sh -1 ../ppurs)

test-typing: ppurs
	@(cd test && bash test.sh -2 ../ppurs)

test-exec: ppurs
	@(cd test && bash test.sh -3 ../ppurs)

test: ppurs
	@(cd test && bash test.sh -all ../ppurs)

clean:
	dune clean
	rm -f ppurs

.PHONY: all clean ppurs prt test test-syntax test-typing test-exec
