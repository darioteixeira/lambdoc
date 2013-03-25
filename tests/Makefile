all: fuzzer

fuzzer: fuzzer.ml
	ocamlfind ocamlc -g -thread -package unix,lambdoc -linkpkg -o $@ $<

