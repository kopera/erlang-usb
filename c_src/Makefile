CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

ERL ?= erl
PKG_CONFIG ?= pkg-config

# System specific C compiler/flags.
UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -O2 -arch x86_64 -finline-functions -Wall -Wmissing-prototypes
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	CFLAGS ?= -O2 -finline-functions -Wall -Wmissing-prototypes
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS ?= -O2 -finline-functions -Wall -Wmissing-prototypes
endif

# Project specific C compiler/flags.
ifeq ($(ERTS_INCLUDE_DIR),)
	ERTS_INCLUDE_DIR := $(shell "$(ERL)" -noshell -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)])." -s erlang halt)
endif

CFLAGS += -std=c11 -fPIC -I $(ERTS_INCLUDE_DIR)
CFLAGS += $(shell $(PKG_CONFIG) --cflags libusb-1.0)

LDFLAGS += -shared
LDFLAGS += $(shell $(PKG_CONFIG) --libs libusb-1.0)

# Verbosity.
V ?= 0

c_verbose_0 = @echo " C     " $(@F);
c_verbose = $(c_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

# Files
HEADERS := $(wildcard *.h)

# Rules
.PHONY: install
install: _build/usb_nif.so | $(BASEDIR)/priv/
	cp $^ $(BASEDIR)/priv

.PHONY: all
all: _build/usb_nif.so

.PHONY: clean
clean:
	rm -f "$(BASEDIR)/priv/usb_nif.so"
	rm -Rf "_build/"

_build/%.so: _build/%.o  $(MAKEFILE_LIST) | _build/
	$(link_verbose) $(CC) "$<" $(LDFLAGS) -o "$@"

_build/%.o: %.c $(HEADERS) $(MAKEFILE_LIST) | _build/
	$(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c "$<" -o "$@"

_build/:
	mkdir -p "$@"

$(BASEDIR)/priv/:
	mkdir -p "$@"
