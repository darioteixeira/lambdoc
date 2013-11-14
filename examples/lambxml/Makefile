LAMBCMD=../../src/_build/lambcmd/lambcmd.native

all: index.html

index.html: sample.lambxml
	$(LAMBCMD) -f lambxml -t html -i $< -o $@

clean:
	rm -rf index.html
