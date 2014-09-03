NAME=serialisation_test

all: $(NAME).native $(NAME).byte

%.native: %.cmx
	ocamlfind ocamlopt -thread -package batteries,lambdoc -linkpkg -o $@ $<

%.byte: %.cmo
	ocamlfind ocamlc -thread -package batteries,lambdoc -linkpkg -o $@ $<

%.cmo: %.ml
	ocamlfind ocamlc -thread -package batteries,lambdoc -c $<

%.cmx: %.ml
	ocamlfind ocamlopt -thread -package batteries,lambdoc -c $<

clean:
	rm -f $(NAME).cm[ioa] $(NAME).[ao] $(NAME).byte $(NAME).native

