# gerbil-charts depends on local gerbil-svg and gerbil-cairo builds
GERBIL_SVG_LIB ?= $(HOME)/mine/gerbil-svg/.gerbil/lib
GERBIL_CAIRO_LIB ?= $(HOME)/mine/gerbil-cairo/.gerbil/lib
export GERBIL_LOADPATH := $(GERBIL_SVG_LIB):$(GERBIL_CAIRO_LIB)

build:
	gerbil build

clean:
	gerbil clean

test:
	gerbil test ./...
