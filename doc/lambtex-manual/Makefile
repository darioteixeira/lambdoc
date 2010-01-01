NAME=lambtex_manual

all: $(NAME).cma $(NAME).cmxs circle.png square.png hierarchy.png

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

hierarchy.png: hierarchy.svg
	inkscape -D -f $< -e $@ -w 800

%.png: %.svg
	inkscape -D -f $< -e $@ -w 240

clean:
	rm -f $(NAME).[ao] $(NAME).cm[ioa] $(NAME).cmxa $(NAME).cmxs

