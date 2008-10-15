OCAMLBUILS_OPTS:=-Xs doc,css,samples

all: test

test: install
	ocamlbuild $(OCAMLBUILS_OPTS) test.byte

lib:
	ocamlbuild $(OCAMLBUILS_OPTS) lambdoc.cma

apidoc:
	ocamlbuild $(OCAMLBUILS_OPTS) lambdoc.docdir/index.html

install: lib
	ocamlfind remove lambdoc
	ocamlfind install lambdoc META _build/lambdoc.cma _build/lambdoc.cmi

clean:
	ocamlbuild $(OCAMLBUILS_OPTS) -clean

