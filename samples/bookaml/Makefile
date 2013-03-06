all: module.cma module.cmxs

module.cma: bookaml_lambdoc.cmo test.cmo
	ocamlc -a -o $@ $+

module.cmxa: bookaml_lambdoc.cmx test.cmx
	ocamlopt -a -o $@ $+

module.cmxs: module.cmxa
	ocamlopt -shared -linkall -I `pwd` -o $@ $<

bookaml_lambdoc.cmo: bookaml_lambdoc.ml
	ocamlfind ocamlc -thread -package lwt,ocsigen,lambdoc,bookaml,lwt.syntax -syntax camlp4o -c $<

bookaml_lambdoc.cmx: bookaml_lambdoc.ml
	ocamlfind ocamlopt -thread -package lwt,ocsigen,lambdoc,bookaml,lwt.syntax -syntax camlp4o -c $<

test.cmo: test.ml bookaml_lambdoc.cmi
	ocamlfind ocamlc -thread -package lwt,ocsigen,lambdoc,bookaml,lwt.syntax -syntax camlp4o -c $<

test.cmx: test.ml bookaml_lambdoc.cmi
	ocamlfind ocamlopt -thread -package lwt,ocsigen,lambdoc,bookaml,lwt.syntax -syntax camlp4o -c $<

clean:
	rm -f *.cm[ioax] *.cmx[as] *.[oa]

