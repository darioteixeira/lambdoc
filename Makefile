#
# Makefile for Lambdoc.
#

#
# Configuration options.
#

PKG_NAME=lambdoc
SRC_DIR=src
LIB_DIR=$(SRC_DIR)/_build/lib
OCAMLBUILD_OPTS=-use-ocamlfind -no-links -cflags -w,+a-4-6-9-27-40-42-45-48

LIBFILES=lambdoc.cma lambdoc.cmxa lambdoc.cmxs lambdoc.a
COMPONENTS=lambdoc_core \
	lambdoc_reader \
	lambdoc_writer \
	lambdoc_read_lambxml \
	lambdoc_read_lambwiki \
	lambdoc_read_lambtex \
	lambdoc_read_markdown \
	lambdoc_write_html5
COMPONENTS_CMI=$(foreach ELEM, $(COMPONENTS), $(ELEM).cmi)

TARGETS=$(LIBFILES) $(COMPONENTS_CMI)
FQTARGETS=$(foreach TARGET, $(TARGETS), $(LIB_DIR)/$(TARGET))


#
# Rules.
#

all:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) lib.otarget lambcmd.otarget

lib:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) lib.otarget

lambcmd:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) lambcmd.otarget

doc:
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

