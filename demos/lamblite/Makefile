LAMBCMD=../../src/_build/lambcmd/lambcmd.native

all: index.html

index.html: sample.lamblite
	$(LAMBCMD) -f lamblite -t xhtml -i $< -o $@

clean:
	rm -rf index.html
