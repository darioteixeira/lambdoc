LAMBCMD=../../src/_build/lambcmd/lambcmd.native

all: complete.html

complete.html: complete.lambhtml
	$(LAMBCMD) -f lambhtml -t xhtml -i $< -o $@

clean:
	rm -rf complete.html
