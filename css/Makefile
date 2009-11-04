IMAGES=callout-warn.png callout-tip.png rule.png

all: lambdoc.css $(IMAGES)

lambdoc.css: lambdoc.cssx lambdoc_*.cssx
	cpp -P $< $@
	#csstidy tmp.css $@ --silent=true
	#rm tmp.css

callout-%.png: callout-%.svg
	inkscape -D -f $< -e $@ -w 20

rule.png: rule.svg
	inkscape -D -f $< -e $@ -h 8

clean:
	rm -f lambdoc.css $(IMAGES)

