NAME=ocsigen_test
OOPTS=-w -40

all: $(NAME).cma $(NAME).cmxs

%.cma: %.cmo
	ocamlc -a -o $@ $+

%.cmxa: %.cmx
	ocamlopt -a -o $@ $+

%.cmxs: %.cmxa
	ocamlopt -shared -linkall -I `pwd` -o $@ $<

%.cmo: %.ml
	ocamlfind ocamlc $(OOPTS) -thread -package lwt,eliom.server,lambdoc,lwt.syntax -syntax camlp4o -c $<

%.cmx: %.ml
	ocamlfind ocamlopt $(OOPTS) -thread -package lwt,eliom.server,lambdoc,lwt.syntax -syntax camlp4o -c $<

clean:
	rm -f $(NAME).cm[ioa] $(NAME).cmxa $(NAME).cmxs $(NAME).[oa]

