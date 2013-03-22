LAMBCMD=../../src/_build/lambcmd/lambcmd.native

all: index.html circle.png square.png hierarchy.png

index.html: lambtex-manual.lambtex
	$(LAMBCMD) --title "The Lambtex Manual" -f lambtex -t html -i $< -o $@

hierarchy.png: hierarchy.svg
	inkscape -D -f $< -e $@ -w 800

%.png: %.svg
	inkscape -D -f $< -e $@ -w 240

clean:
	rm -rf *.png index.html
