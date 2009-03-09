#
# Configuration options.
#

PKG_NAME=lambdoc

SRC_DIR=src

LIB_BUILD_DIR=$(SRC_DIR)/_build
LIB_TARGETS=lambdoc.cma lambdoc.cmxa lambdoc.a lambdoc_core.cmi lambdoc_reader.cmi lambdoc_writer.cmi read_lambtex.cmi write_xhtml.cmi lambdoc_proxy.cmi
LIB_FQTARGETS=$(foreach TARGET, $(LIB_TARGETS), $(LIB_BUILD_DIR)/$(TARGET))

PARSERVER_TARGETS=parserver.byte parserver.native

OCAMLBUILD_OPTS=

#
# Rules.
#

all: lib parserver

lib:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) $(LIB_TARGETS)

parserver:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) $(PARSERVER_TARGETS)

apidoc:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) lambdoc.docdir/index.html

install: lib
	ocamlfind install $(PKG_NAME) $(SRC_DIR)/META $(LIB_FQTARGETS)

uninstall:
	ocamlfind remove $(PKG_NAME)

reinstall: lib
	ocamlfind remove $(PKG_NAME)
	ocamlfind install $(PKG_NAME) $(SRC_DIR)/META $(LIB_FQTARGETS)

clean:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) -clean

