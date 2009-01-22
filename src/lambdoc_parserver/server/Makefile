CLIENT:=client
SERVER:=parserver
OCAMLBUILS_OPTS:=

.PHONY: all byte native clean

all: native

byte:
	ocamlbuild $(OCAMLBUILS_OPTS) $(SERVER).byte $(CLIENT).byte

native:
	ocamlbuild $(OCAMLBUILS_OPTS) $(SERVER).native $(CLIENT).native

clean:
	ocamlbuild $(OCAMLBUILS_OPTS) -clean
