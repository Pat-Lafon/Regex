MODULES=parser
OBJECTS=$(MODULES:=.cmo)
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=oUnit,qcheck

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag debug $(TEST) && ./$(TEST)

bisect:
	$(OCAMLBUILD) -package bisect -syntax camlp4o,bisect_pp \
	  $(TEST) && ./$(TEST) -runner sequential

clean:
	ocamlbuild -clean
	rm -rf report bisect*.out
