all: ppurs

ppurs:
	dune build src/ppurs.exe
	@cp src/ppurs.exe ppurs

test-syntax: ppurs
	@(cd test && bash test.sh -1 ../ppurs)

test-typing: ppurs
	@(cd test && bash test.sh -2 ../ppurs)

test-exec: ppurs
	@(cd test && bash test.sh -3 ../ppurs)

test: test-syntax test-typing test-exec

clean:
	dune clean
	rm -f ppurs

.PHONY: all clean ppurs.exe ppurs syntax
