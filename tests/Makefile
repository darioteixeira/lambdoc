all: fuzzer.native fuzzer.byte

fuzzer.byte: fuzzer.ml
	ocamlfind ocamlc -g -thread -package unix,lambdoc -linkpkg -o $@ $<

fuzzer.native: fuzzer.ml
	ocamlfind ocamlopt -thread -package unix,lambdoc -linkpkg -o $@ $<

