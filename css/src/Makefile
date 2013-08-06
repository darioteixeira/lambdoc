IMAGES=../boxout-warning.png ../boxout-tip.png

SOURCES=lambdoc_reset.ccss \
	lambdoc_config.ccss \
	lambdoc_valid.ccss \
	lambdoc_invalid.ccss \
	lambdoc_custom.ccss \
	lambdoc_root.ccss

TARGET=../lambdoc.css

all: $(TARGET) $(IMAGES)

$(TARGET): $(SOURCES)
	cat $+ | ccss > $@

../boxout-%.png: boxout-%.svg Makefile
	inkscape -D -f $< -e $@ -w 20 -h 20 --export-background-opacity=0.0

clean:
	rm -f $(TARGET) $(IMAGES)

