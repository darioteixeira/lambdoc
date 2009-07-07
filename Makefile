#
# Makefile for Lambdoc.
#

#
# Configuration options.
#

PKG_NAME=lambdoc
SRC_DIR=src
BUILD_DIR=$(SRC_DIR)/_build

LIBFILES=lambdoc.cma lambdoc.cmxa lambdoc.a
COMPONENTS=lambdoc_core lambdoc_reader lambdoc_writer read_lambtex write_xhtml lambdoc_proxy
COMPONENTS_CMI=$(foreach ELEM, $(COMPONENTS), $(ELEM).cmi)
COMPONENTS_CMO=$(foreach ELEM, $(COMPONENTS), $(ELEM).cmo)
COMPONENTS_CMX=$(foreach ELEM, $(COMPONENTS), $(ELEM).cmx)
COMPONENTS_OBJ=$(foreach ELEM, $(COMPONENTS), $(ELEM).o)

TARGETS=$(LIBFILES) $(COMPONENTS_CMI) $(COMPONENTS_CMO) $(COMPONENTS_CMX) $(COMPONENTS_OBJ)
FQTARGETS=$(foreach TARGET, $(TARGETS), $(BUILD_DIR)/$(TARGET))

PARSERVER_TARGETS=parserver.byte parserver.native

OCAMLBUILD_OPTS=

#
# Rules.
#

all: lib parserver

lib:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) $(TARGETS)

parserver:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) $(PARSERVER_TARGETS)

apidoc:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) lambdoc.docdir/index.html

install: lib
	ocamlfind install $(PKG_NAME) $(SRC_DIR)/META $(FQTARGETS)

uninstall:
	ocamlfind remove $(PKG_NAME)

reinstall: lib
	ocamlfind remove $(PKG_NAME)
	ocamlfind install $(PKG_NAME) $(SRC_DIR)/META $(FQTARGETS)

clean:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) -clean

