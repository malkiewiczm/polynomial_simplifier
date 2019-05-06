MAKEFLAGS += Rr

.PHONY: all clean tar

all: rpn

%: %.ml
	ocamlc str.cma -o $@ $<

clean:
	rm -f rpn *.cmo *.cmi *.tgz

tar:
	tar -czf `date +%s`.tgz makefile *.ml *.txt
