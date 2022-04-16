# SPDX-FileCopyrightText: 2022 Max Reznik <reznikmm@gmail.com>
#
# SPDX-License-Identifier: Apache-2.0


GPRBUILD_FLAGS = -p -j0
PREFIX                 ?= /usr
GPRDIR                 ?= $(PREFIX)/share/gpr
LIBDIR                 ?= $(PREFIX)/lib
BINDIR                 ?= $(PREFIX)/bin
INSTALL_PROJECT_DIR    ?= $(DESTDIR)$(GPRDIR)
INSTALL_INCLUDE_DIR    ?= $(DESTDIR)$(PREFIX)/include/rst_unparser
INSTALL_EXEC_DIR       ?= $(DESTDIR)$(BINDIR)
INSTALL_LIBRARY_DIR    ?= $(DESTDIR)$(LIBDIR)
INSTALL_ALI_DIR        ?= ${INSTALL_LIBRARY_DIR}/rst_unparser

GPRINSTALL_FLAGS = --prefix=$(PREFIX) --sources-subdir=$(INSTALL_INCLUDE_DIR)\
 --lib-subdir=$(INSTALL_ALI_DIR) --project-subdir=$(INSTALL_PROJECT_DIR)\
 --link-lib-subdir=$(INSTALL_LIBRARY_DIR) --exec-subdir=$(INSTALL_EXEC_DIR)

all:
	gprbuild $(GPRBUILD_FLAGS) -P gnat/rst_unparser.gpr
	gprbuild $(GPRBUILD_FLAGS) -P gnat/rst_unparser_examples.gpr

install:
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/rst_unparser.gpr
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/rst_unparser_examples.gpr --mode=usage
clean:
	gprclean -q -P gnat/rst_unparser_examples.gpr
	gprclean -q -P gnat/rst_unparser.gpr

check: all
	@set -e; \
	echo No tests yet
