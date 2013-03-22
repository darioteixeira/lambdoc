LAMBCMD=../../src/_build/lambcmd/lambcmd.native

all: index.html

index.html: lamblite-manual.lambtex
	$(LAMBCMD) --title "The Lamblite Manual" -f lambtex -t html -i $< -o $@

clean:
	rm -rf index.html

