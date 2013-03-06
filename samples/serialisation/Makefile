NAME=test

all: $(NAME).native $(NAME).byte

%.native: %.cmx
	ocamlfind ocamlopt -thread -package lambdoc -linkpkg -o $@ $<

%.byte: %.cmo
	ocamlfind ocamlc -thread -package lambdoc -linkpkg -o $@ $<

%.cmo: %.ml
	ocamlfind ocamlc -thread -package lambdoc -c $<

%.cmx: %.ml
	ocamlfind ocamlopt -thread -package lambdoc -c $<

clean:
	rm -f $(NAME).cm[ioa] $(NAME).[ao] $(NAME).byte $(NAME).native

