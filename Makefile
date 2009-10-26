#
# Makefile for Lambdoc.
#

#
# Configuration options.
#

PKG_NAME=lambdoc
SRC_DIR=src
BUILD_DIR=$(SRC_DIR)/_build
OCAMLBUILD_OPTS=

LIBFILES=lambdoc.cma lambdoc.cmxa lambdoc.a
COMPONENTS=lambdoc_core lambdoc_reader lambdoc_writer lambdoc_proxy lambdoc_read_lambhtml lambdoc_read_lamblite lambdoc_read_lambtex lambdoc_write_xhtml
COMPONENTS_CMI=$(foreach ELEM, $(COMPONENTS), $(ELEM).cmi)
COMPONENTS_CMO=$(foreach ELEM, $(COMPONENTS), $(ELEM).cmo)
COMPONENTS_CMX=$(foreach ELEM, $(COMPONENTS), $(ELEM).cmx)
COMPONENTS_OBJ=$(foreach ELEM, $(COMPONENTS), $(ELEM).o)

TARGETS=$(LIBFILES) $(COMPONENTS_CMI) $(COMPONENTS_CMO) $(COMPONENTS_CMX) $(COMPONENTS_OBJ)
FQTARGETS=$(foreach TARGET, $(TARGETS), $(BUILD_DIR)/$(TARGET))


#
# Rules.
#

all:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) all.otarget

apidoc:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) lambdoc.docdir/index.html

install: all
	ocamlfind install $(PKG_NAME) $(SRC_DIR)/META $(FQTARGETS)

uninstall:
	ocamlfind remove $(PKG_NAME)

reinstall: all
	ocamlfind remove $(PKG_NAME)
	ocamlfind install $(PKG_NAME) $(SRC_DIR)/META $(FQTARGETS)

clean:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) -clean

