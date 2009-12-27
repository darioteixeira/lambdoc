LAMBCMD=../../src/_build/lambcmd/lambcmd.native

all: complete.html circle-small.png circle-big.png square-small.png square-big.png triangle-huge.png

complete.html: complete.lambtex
	$(LAMBCMD) -f lambtex -t xhtml -i $< -o $@

%-small.png: %.svg
	inkscape -D -f $< -e $@ -w 160

%-big.png: %.svg
	inkscape -D -f $< -e $@ -w 320

%-huge.png: %.svg
	inkscape -D -f $< -e $@ -w 960

clean:
	rm -rf *.png complete.html
