# gerbil-charts depends on local gerbil-svg and gerbil-cairo builds
GERBIL_SVG_LIB ?= $(HOME)/mine/gerbil-svg/.gerbil/lib
GERBIL_CAIRO_LIB ?= $(HOME)/mine/gerbil-cairo/.gerbil/lib
GERBIL_PATH ?= $(HOME)/.gerbil
LOCAL_LIB := $(CURDIR)/.gerbil/lib
export GERBIL_LOADPATH := $(LOCAL_LIB):$(GERBIL_SVG_LIB):$(GERBIL_CAIRO_LIB)

CAIRO_LDFLAGS := $(shell pkg-config --libs cairo libpng16 2>/dev/null || echo "-lcairo -lpng16")
CAIRO_LIBDIR := $(shell pkg-config --variable=libdir cairo 2>/dev/null)

build:
	gerbil build
	mkdir -p .gerbil/bin
	gxc -exe -o .gerbil/bin/gerbil-charts \
	  -ld-options "$(CAIRO_LDFLAGS) -Wl,-rpath,$(CAIRO_LIBDIR)" \
	  main.ss

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
