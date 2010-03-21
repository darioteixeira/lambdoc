LAMBCMD=../../src/_build/lambcmd/lambcmd.native

all: index.html features.png

index.html: lamblite-manual.lambtex
	$(LAMBCMD) --title "The Lamblite Manual" -f lambtex -t xhtml -i $< -o $@

features.png: features.svg
	inkscape -D -f $< -e $@ -w 320

clean:
	rm -rf index.html

