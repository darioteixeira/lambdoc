NAME=test
PKGS=lambdoc,benchmark

all: $(NAME).native $(NAME).byte

%.native: %.cmx
	ocamlfind ocamlopt -thread -package $(PKGS) -linkpkg -o $@ $<

%.byte: %.cmo
	ocamlfind ocamlc -thread -package $(PKGS) -linkpkg -o $@ $<

%.cmo: %.ml
	ocamlfind ocamlc -thread -package $(PKGS) -c $<

%.cmx: %.ml
	ocamlfind ocamlopt -thread -package $(PKGS) -c $<

clean:
	rm -f $(NAME).cm[ioa] $(NAME).[ao] $(NAME).byte $(NAME).native

