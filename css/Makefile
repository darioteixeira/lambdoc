IMAGES=boxout-warning.png boxout-tip.png

all: lambdoc.css $(IMAGES)

lambdoc.css: lambdoc.cssx lambdoc_*.cssx
	cpp -P $< $@
	#csstidy tmp.css $@ --silent=true
	#rm tmp.css

boxout-%.png: boxout-%.svg
	inkscape -D -f $< -e $@ -h 24 --export-background-opacity=0.0

clean:
	rm -f lambdoc.css $(IMAGES)

