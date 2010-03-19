IMAGES=boxout-warning.png boxout-tip.png

EMBEDDED_TARGET=embedded_lambdoc.css
EMBEDDED_SOURCES=lambdoc_config.ccss lambdoc_valid.ccss lambdoc_invalid.ccss lambdoc_custom.ccss

STANDALONE_TARGET=standalone_lambdoc.css
STANDALONE_SOURCES=lambdoc_reset.ccss $(EMBEDDED_SOURCES) lambdoc_global.ccss


all: $(STANDALONE_TARGET) $(EMBEDDED_TARGET) $(IMAGES)

$(STANDALONE_TARGET): $(STANDALONE_SOURCES)
	cat $+ | ccss > $@

$(EMBEDDED_TARGET): $(EMBEDDED_SOURCES)
	cat $+ | ccss > $@

boxout-%.png: boxout-%.svg Makefile
	inkscape -D -f $< -e $@ -w 20 -h 20 --export-background-opacity=0.0

clean:
	rm -f $(STANDALONE_TARGET) $(EMBEDDED_TARGET) $(IMAGES)

