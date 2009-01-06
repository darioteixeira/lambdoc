#
# Configuration options.
#

LIB_NAME=lambdoc
PKG_NAME=lambdoc

SRC_DIR=src
BUILD_DIR=$(SRC_DIR)/_build

TARGETS=lambdoc.cma lambdoc.cmxa lambdoc.a lambdoc_core.cmi lambdoc_reader.cmi lambdoc_writer.cmi read_lambtex.cmi write_xhtml.cmi

FQTARGETS=$(foreach TARGET, $(TARGETS), $(BUILD_DIR)/$(TARGET))

OCAMLBUILD_OPTS=


#
# Rules.
#

all: lib

lib:
	cd $(SRC_DIR) && ocamlbuild $(OCAMLBUILD_OPTS) $(TARGETS)

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

