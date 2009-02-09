NAME=sample

all: $(NAME).cma $(NAME).cmxs circle-small.png circle-big.png square-small.png square-big.png

%.cma: %.cmo
	ocamlc -a -o $@ $+

%.cmxa: %.cmx
	ocamlopt -a -o $@ $+

%.cmxs: %.cmxa
	ocamlopt -shared -linkall -I `pwd` -o $@ $<

%.cmo: %.ml
	ocamlfind ocamlc -thread -package lwt,ocsigen,lambdoc -c $<

%.cmx: %.ml
	ocamlfind ocamlopt -thread -package lwt,ocsigen,lambdoc -c $<

%-small.png: %.svg
	inkscape -D -f $< -e $@ -w 160

%-big.png: %.svg
	inkscape -D -f $< -e $@ -w 320

clean:
	rm -f $(NAME).cm[ioa] $(NAME).cmxa $(NAME).cmxs $(NAME).[oa]

