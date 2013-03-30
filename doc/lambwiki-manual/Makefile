LAMBCMD=../../src/_build/lambcmd/lambcmd.native

all: index.html

index.html: lambwiki-manual.lambtex
	$(LAMBCMD) --title "The Lambwiki Manual" -f lambtex -t html -i $< -o $@

clean:
	rm -rf index.html

