LAMBCMD=../../../_build/src/lambcmd/lambcmd.native

all: index.html

index.html: sample.wiki
	$(LAMBCMD) -f lambwiki -t html -i $< -o $@

clean:
	rm -rf index.html
