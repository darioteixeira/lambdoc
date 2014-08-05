LAMBCMD=../../src/_build/lambcmd/lambcmd.native

all: index.html

index.html: sample.md
	$(LAMBCMD) -f markdown -t html -i $< -o $@

clean:
	rm -rf index.html
