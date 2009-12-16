IMAGES=boxout-warning.png boxout-tip.png

all: lambdoc.css $(IMAGES)

lambdoc.css: lambdoc.cssx lambdoc_*.cssx
	cpp -P $< $@
	#csstidy tmp.css $@ --silent=true
	#rm tmp.css

boxout-%.png: boxout-%.svg Makefile
	inkscape -D -f $< -e $@ -w 20 -h 20 --export-background-opacity=0.0

clean:
	rm -f lambdoc.css $(IMAGES)

