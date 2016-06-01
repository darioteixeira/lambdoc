LAMBCMD=../../../_build/src/lambcmd/lambcmd.native

all: index.html

index.html: sample.xml
	$(LAMBCMD) -f lambxml -t html -i $< -o $@

clean:
	rm -rf index.html
