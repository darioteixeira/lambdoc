all: callout-warn.png callout-tip.png rule.png

callout-%.png: callout-%.svg
	inkscape -D -f $< -e $@ -w 20

rule.png: rule.svg
	inkscape -D -f $< -e $@ -h 8
