LAMBCMD=../../src/_build/lambcmd/lambcmd.native

all: index.html

index.html: sample.lambhtml
	$(LAMBCMD) -f lambhtml -t xhtml -i $< -o $@

clean:
	rm -rf index.html
