# gerbil-charts depends on local gerbil-svg and gerbil-cairo builds
GERBIL_SVG_LIB ?= $(HOME)/mine/gerbil-svg/.gerbil/lib
GERBIL_CAIRO_LIB ?= $(HOME)/mine/gerbil-cairo/.gerbil/lib
GERBIL_PATH ?= $(HOME)/.gerbil
export GERBIL_LOADPATH := $(GERBIL_SVG_LIB):$(GERBIL_CAIRO_LIB)

build:
	gerbil build

clean:
	gerbil clean

test:
	gerbil test ./...

install: build
	mkdir -p $(GERBIL_PATH)/lib/gerbil-charts
	mkdir -p $(GERBIL_PATH)/lib/static/gerbil-charts
	cp -r .gerbil/lib/gerbil-charts/* $(GERBIL_PATH)/lib/gerbil-charts/
	cp -r .gerbil/lib/static/gerbil-charts/* $(GERBIL_PATH)/lib/static/gerbil-charts/
	mkdir -p $(HOME)/bin
	cp .gerbil/bin/gerbil-charts $(HOME)/bin/
