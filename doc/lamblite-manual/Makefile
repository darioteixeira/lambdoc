LAMBCMD=../../src/_build/lambcmd/lambcmd.native

all: index.html

index.html: lamblite-manual.lambtex
	$(LAMBCMD) -f lambtex -t xhtml -i $< -o $@

clean:
	rm -rf index.html

