all: tut.cma

tut.cma: main.cmo
	ocamlc -a -o $@ $+

main.cmo: main.ml
	ocamlfind ocamlc -thread -package lwt,ocsigen,lambdoc -c $<

clean:
	rm -f *.cm[ioa]

