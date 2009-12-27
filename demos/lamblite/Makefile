LAMBCMD=../../src/_build/lambcmd/lambcmd.native

all: complete.html

complete.html: complete.lamblite
	$(LAMBCMD) -f lamblite -t xhtml -i $< -o $@

clean:
	rm -rf complete.html
