# gerbil-charts depends on local gerbil-svg, gerbil-cairo, and gerbil-graphviz builds
GERBIL_SVG_LIB ?= $(HOME)/mine/gerbil-svg/.gerbil/lib
GERBIL_CAIRO_LIB ?= $(HOME)/mine/gerbil-cairo/.gerbil/lib
GERBIL_GRAPHVIZ_LIB ?= $(HOME)/mine/gerbil-graphviz/.gerbil/lib
GERBIL_PATH ?= $(HOME)/.gerbil
LOCAL_LIB := $(CURDIR)/.gerbil/lib
export GERBIL_LOADPATH := $(LOCAL_LIB):$(GERBIL_SVG_LIB):$(GERBIL_CAIRO_LIB):$(GERBIL_GRAPHVIZ_LIB)

CAIRO_LDFLAGS := $(shell pkg-config --libs cairo libpng16 2>/dev/null || echo "-lcairo -lpng16")
CAIRO_LIBDIR := $(shell pkg-config --variable=libdir cairo 2>/dev/null)
GRAPHVIZ_LDFLAGS := $(shell pkg-config --libs libgvc 2>/dev/null || echo "-lgvc -lcgraph -lcdt")
GRAPHVIZ_LIBDIR := $(shell pkg-config --variable=libdir libgvc 2>/dev/null)

# Static binary build variables
PROJECT := gerbil-charts
ARCH := $(shell uname -m)
PWD := $(shell pwd)
DOCKER_IMAGE := "gerbil/gerbilxx:$(ARCH)-master"
UID := $(shell id -u)
GID := $(shell id -g)

build:
	gerbil build
	mkdir -p .gerbil/bin
	gxc -exe -o .gerbil/bin/gcharts \
	  -ld-options "$(CAIRO_LDFLAGS) -Wl,-rpath,$(CAIRO_LIBDIR) $(GRAPHVIZ_LDFLAGS) -Wl,-rpath,$(GRAPHVIZ_LIBDIR)" \
	  main.ss

clean:
	gerbil clean

clean-docker:
	-rm -rf .gerbil 2>/dev/null || true
	docker run --rm -v $(PWD):/src:z alpine rm -rf /src/.gerbil

test:
	gerbil test ./...

install:
	@if [ ! -f .gerbil/bin/gcharts ]; then \
		echo "Error: .gerbil/bin/gcharts not found. Run 'make build' or 'make static' first."; \
		exit 1; \
	fi
	mkdir -p $(GERBIL_PATH)/lib/gerbil-charts
	mkdir -p $(GERBIL_PATH)/lib/static
	cp -r .gerbil/lib/gerbil-charts/* $(GERBIL_PATH)/lib/gerbil-charts/
	if [ -d .gerbil/lib/static ] && [ -n "$$(ls -A .gerbil/lib/static)" ]; then \
		cp .gerbil/lib/static/gerbil-charts__* $(GERBIL_PATH)/lib/static/ 2>/dev/null || true; \
	fi
	mkdir -p $(HOME)/bin
	cp .gerbil/bin/gcharts $(HOME)/bin/

static: linux-static-docker

check-root:
	@if [ "${UID}" -eq 0 ]; then \
	  git config --global --add safe.directory /src; \
	fi

build-static: check-root
	gxpkg install github.com/ober/gerbil-svg github.com/ober/gerbil-cairo github.com/mighty-gerbils/gerbil-libyaml github.com/ober/gerbil-graphviz
	gxpkg build
	mkdir -p .gerbil/bin
	gxc -exe -o .gerbil/bin/gcharts \
	  -ld-options "-static $(shell pkg-config --static --libs cairo libpng16) -Wl,--whole-archive -lgvplugin_dot_layout -lgvplugin_neato_layout -lgvplugin_core -Wl,--no-whole-archive -Wl,--start-group -lgvc -lcommon -ldotgen -lneatogen -lfdpgen -lsfdpgen -lcircogen -ltwopigen -losage -lpatchwork -lutil -lpack -lortho -llabel -lsparse -lrbtree -lvpsc -lpathplan -lcgraph -lcdt -lxdot -Wl,--end-group -lexpat -lbsd -lmd -lyaml -lstdc++" \
	  main.ss

# Build graphviz from source with static libraries (no Alpine static package)
# - LTDL disabled: prevents demand-loading, uses built-in plugin table
# - UTIL_API=: makes gv_fopen visible (default is hidden visibility)
# - Copies ALL .a from build dir since make install skips plugin libs
build-graphviz:
	cd /tmp && \
	  wget -q https://gitlab.com/graphviz/graphviz/-/archive/12.2.1/graphviz-12.2.1.tar.gz && \
	  tar xzf graphviz-12.2.1.tar.gz && \
	  cd graphviz-12.2.1 && \
	  mkdir build && cd build && \
	  cmake .. \
	    -DCMAKE_INSTALL_PREFIX=/usr \
	    -DCMAKE_BUILD_TYPE=Release \
	    -DBUILD_SHARED_LIBS=OFF \
	    -DCMAKE_DISABLE_FIND_PACKAGE_LTDL=ON \
	    -DCMAKE_C_FLAGS="-fPIC -DUTIL_API=" && \
	  make -j$$(nproc) && \
	  make install && \
	  find . -name '*.a' ! -path './CMakeFiles/*' -exec cp {} /usr/lib/ \;

# libXau has no -static Alpine package; build from source
build-libxau:
	cd /tmp && \
	  wget -q https://xorg.freedesktop.org/releases/individual/lib/libXau-1.0.11.tar.xz && \
	  tar xf libXau-1.0.11.tar.xz && \
	  cd libXau-1.0.11 && \
	  ./configure --enable-static --disable-shared --prefix=/usr && \
	  make -j$$(nproc) && \
	  make install

linux-static-docker: clean-docker
	docker run --rm \
	  --ulimit nofile=1024:1024 \
	  -v $(PWD):/src:z \
	  $(DOCKER_IMAGE) \
	  sh -c "apk add --no-cache \
	           cmake bison flex g++ \
	           cairo-dev cairo-static \
	           libpng-dev libpng-static \
	           pixman-static \
	           freetype-static \
	           fontconfig-static \
	           expat-dev expat-static \
	           bzip2-static \
	           brotli-static \
	           zlib-static \
	           libx11-static \
	           libxcb-static \
	           libxext-static \
	           libbsd-static \
	           libmd-dev \
	           yaml-dev yaml-static && \
	         cd /src && \
	         make build-graphviz && \
	         make build-libxau && \
	         make build-static && \
	         chown -R $(UID):$(GID) .gerbil"
