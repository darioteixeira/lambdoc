IMAGES=boxout-warning.png boxout-tip.png
UNRESET_SOURCES=lambdoc_config.ccss lambdoc_common.ccss lambdoc_valid.ccss lambdoc_invalid.ccss lambdoc_custom.ccss
RESET_SOURCES=lambdoc_reset.ccss $(UNRESET_SOURCES)

all: reset_lambdoc.css unreset_lambdoc.css $(IMAGES)

reset_lambdoc.css: $(RESET_SOURCES)
	cat $+ | ccss > $@

unreset_lambdoc.css: $(UNRESET_SOURCES)
	cat $+ | ccss > $@

boxout-%.png: boxout-%.svg Makefile
	inkscape -D -f $< -e $@ -w 20 -h 20 --export-background-opacity=0.0

clean:
	rm -f reset_lambdoc.css unreset_lambdoc.css $(IMAGES)

