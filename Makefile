#
# Makefile for Lambdoc.
#

#
# Configuration options.
#

PKG_NAME=lambdoc
SRC_DIR=src
LIB_DIR=$(SRC_DIR)/_build/lib
OCAMLBUILD_OPTS=-use-ocamlfind -no-links

LIBFILES=lambdoc.cma lambdoc.cmxa lambdoc.cmxs lambdoc.a
COMPONENTS=lambdoc_core lambdoc_reader lambdoc_writer lambdoc_proxy lambdoc_read_lambhtml lambdoc_read_lamblite lambdoc_read_lambtex lambdoc_write_xhtml
COMPONENTS_CMI=$(foreach ELEM, $(COMPONENTS), $(ELEM).cmi)
COMPONENTS_CMO=$(foreach ELEM, $(COMPONENTS), $(ELEM).cmo)
COMPONENTS_CMX=$(foreach ELEM, $(COMPONENTS), $(ELEM).cmx)
COMPONENTS_OBJ=$(foreach ELEM, $(COMPONENTS), $(ELEM).o)

TARGETS=$(LIBFILES) $(COMPONENTS_CMI) $(COMPONENTS_CMO) $(COMPONENTS_CMX) $(COMPONENTS_OBJ)
FQTARGETS=$(foreach TARGET, $(TARGETS), $(LIB_DIR)/$(TARGET))


#
# Rules.
#

build:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) lib.otarget lambcmd.otarget parserver.otarget

lib:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) lib.otarget

lambcmd:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) lambcmd.otarget

parserver:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) parserver.otarget

apidoc:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) lambdoc.docdir/index.html

install: lib
	ocamlfind install $(PKG_NAME) META $(FQTARGETS)

uninstall:
	ocamlfind remove $(PKG_NAME)

reinstall: lib
	ocamlfind remove $(PKG_NAME)
	ocamlfind install $(PKG_NAME) META $(FQTARGETS)

clean:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) -clean

