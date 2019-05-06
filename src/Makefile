MAKEFLAGS += Rr

.PHONY: default clean tests test

IN_FILES=$(wildcard tests/*.in)
OUT_FILES=$(IN_FILES:.in=.out)

default: main.byte

clean:
	rm -f main.byte
	rm -fr _build/
	rm -rf tests/*.out

main.byte: lexer.mll parser.mly expr.ml main.ml poly.ml
	ocamlbuild -use-menhir -use-ocamlfind -lflags -thread -cflags -thread -pkg core main.byte

tests/%.out: tests/%.in main.byte
	./main.byte $< > $@

tests: main.byte $(OUT_FILES)
	@echo "Done testing. Results in tests dir."

test: tests
