LAMBCMD=../../../src/_build/lambcmd/lambcmd.native

all: index.html

index.html: sample.lambwiki
	$(LAMBCMD) -f lambwiki -t html -i $< -o $@

clean:
	rm -rf index.html
