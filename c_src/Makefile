ERL ?= erl
PKG_CONFIG ?= pkg-config

BUILD_DIR := _build
INSTALL_DIR := ../priv

CFLAGS ?= -O3 -finline-functions -Wall -Wmissing-prototypes

ERTS_CFLAGS ?= -I$(shell "$(ERL)" -noshell -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)])." -s erlang halt)
ERTS_LDFLAGS ?=

LIBUSB_CFLAGS ?= $(shell $(PKG_CONFIG) --cflags libusb-1.0)
LIBUSB_LDFLAGS ?= $(shell $(PKG_CONFIG) --libs libusb-1.0)

# System specific compiler/linker flags.
UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	LDFLAGS ?= -flat_namespace -undefined dynamic_lookup
endif

CFLAGS += -std=c11 -fPIC $(ERTS_CFLAGS) $(LIBUSB_CFLAGS)
LDFLAGS += -shared $(ERTS_LDFLAGS) $(LIBUSB_LDFLAGS)

#
# Targets
#

.PHONY: all
all: $(BUILD_DIR)/usb_nif.so

.PHONY: install
install: $(BUILD_DIR)/usb_nif.so | $(INSTALL_DIR)/
	cp $(BUILD_DIR)/usb_nif.so $(INSTALL_DIR)/

.PHONY: clean
clean:
	rm -f "$(INSTALL_DIR)/usb_nif.so"
	rm -Rf "$(BUILD_DIR)/"

#
# Build
#

SOURCES := usb_nif.c
HEADERS := khash.h

$(BUILD_DIR)/usb_nif.o: $(SOURCES) $(HEADERS) $(MAKEFILE_LIST) | $(BUILD_DIR)/
	$(CC) $(CFLAGS) -c $(SOURCES) -o "$@"

$(BUILD_DIR)/usb_nif.so: $(BUILD_DIR)/usb_nif.o $(MAKEFILE_LIST) | $(BUILD_DIR)/
	$(CC) $(BUILD_DIR)/usb_nif.o $(LDFLAGS) -o "$@"

$(BUILD_DIR)/:
	mkdir -p "$@"
