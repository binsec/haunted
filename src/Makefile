##########################################################################
#  This file is part of BINSEC.                                          #
#                                                                        #
#  Copyright (C) 2016-2019                                               #
#    CEA (Commissariat à l'énergie atomique et aux énergies              #
#         alternatives)                                                  #
#                                                                        #
#  you can redistribute it and/or modify it under the terms of the GNU   #
#  Lesser General Public License as published by the Free Software       #
#  Foundation, version 2.1.                                              #
#                                                                        #
#  It is distributed in the hope that it will be useful,                 #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#  GNU Lesser General Public License for more details.                   #
#                                                                        #
#  See the GNU Lesser General Public License version 2.1                 #
#  for more details (enclosed in the file licenses/LGPLv2.1).            #
#                                                                        #
##########################################################################

EXE=binsec

default: all

include ../Config.mk
include Targets.mk
include ../Piqi.mk
include ../Caml.mk

all: $(EXE)
debug: $(EXE).byt
unit-tests: test_runner
	./test_runner
.PHONY: unit-tests

DOCDIR = binsec.docdir

# Check if the user can -- and wants to -- use ocamlbuild
PLEASE_USE_OCAMLBUILD = yes
ifeq ($(OCAMLBUILD), no)
	PLEASE_USE_OCAMLBUILD = no
endif

ifeq ($(USE_OCAMLBUILD), no)
	PLEASE_USE_OCAMLBUILD = no
endif


# If ocamlbuild is both available and demanded by the user
ifneq ($(PLEASE_USE_OCAMLBUILD), no)

$(EXE): piqi-ml config.ml
	$(OCAMLBUILD) $(OCB_OPTIONS) main.native
	$(CP) _build/main.native $(EXE)

$(EXE).byt: piqi-ml config.ml
	$(OCAMLBUILD) $(OCB_OPTIONS) -tag debug main.byte
	$(CP) _build/main.byte $(EXE).byt

clean::
	$(OCAMLBUILD) -clean
	$(RRM) $(PIQI_DIR)/*.ml $(PIQI_DIR)/*.piqi $(EXE)
	$(RRM) *~ \#* */\#* */.\#*

veryclean:: clean clean-configure

# Documentation generation
ifneq ($(CAMLDOC), no)
ifneq ($(DOT), no)

OCB_DOCDIR = doc/$(DOCDIR)

doc_dot_files: camldoc
	$(OCAMLBUILD) $(OCB_OPTIONS) -docflags -dot-include-all $(OCB_DOCDIR)/modules.dot
	$(OCAMLBUILD) $(OCB_OPTIONS) -docflags -dot-include-all,-dot-types $(OCB_DOCDIR)/types.dot
	$(OCAMLBUILD) $(OCB_OPTIONS) -docflags -dot-include-all,-dot-reduce $(OCB_DOCDIR)/modules_reduce.dot
	$(OCAMLBUILD) $(OCB_OPTIONS) -docflags -dot-include-all,-dot-reduce,-dot-types $(OCB_DOCDIR)/types_reduce.dot

camldoc: $(EXE)
	$(OCAMLBUILD) $(OCB_OPTIONS) $(OCB_DOCDIR)/index.html

GEN_DOT_FILES_BASE=modules types modules_reduce types_reduce
GEN_DOT_FILES = $(GEN_DOT_FILES_BASE:%=$(DOCDIR)/%.dot)
SVG_FILES = $(GEN_DOT_FILES_BASE:%=%.svg)

# $(GEN_DOT_FILES): camldoc

doc:: $(SVG_FILES)
	@echo $(GEN_DOT_FILES) $(SVG_FILES)

types.dot: doc_dot_files
	$(CP) $(OCB_DOCDIR)/types.dot .

types.svg: types.dot
	$(DOT2SVG) -o $@ $<

.PHONY: doc

doc :: doc_dot_files camldoc
endif
endif

else

ifneq ($(WARN_ERROR), no) 
	CAMLWARNINGS +=  -warn-error +a
endif

PACKAGES=\
	piqilib piqirun.ext \
	hashset \
	containers \
	zmq \
	zarith \
	ocamlgraph \
	llvm \
	oUnit \
	str bigarray
PKG_FLAGS=$(PACKAGES:%=-package %)
CAMLFLAGS+=-w +a-4-3-18
CAMLYACOPTS+=--fixed-exception
CAMLBIN=$(CAMLFIND) ocamlopt $(PKG_FLAGS)
CAMLBYT=$(CAMLFIND) ocamlc $(PKG_FLAGS)

$(EXE):  $(CMX_FILES)
	$(CAMLBIN) $(CAMLINCLUDES) -linkpkg -o $@ $(CMX_FILES)

$(EXE).byt: $(CMO_FILES)
	$(CAMLBYT) $(CAMLINCLUDES) -linkpkg -o $@ $(CMO_FILES)

test_runner:  $(LIB_CMX_FILES) $(TESTS_CMX_FILES)
	$(CAMLBIN) $(CAMLINCLUDES) $(CAMLFLAGS) -g -linkpkg -o $@ $(LIB_CMX_FILES) $(TESTS_CMX_FILES)

test_runner.byt:  $(LIB_CMO_FILES) $(TESTS_CMO_FILES)
	$(CAMLBYT) $(CAMLINCLUDES) $(CAMLFLAGS) -g -linkpkg -o $@ $(LIB_CMO_FILES) $(TESTS_CMO_FILES)

beforedepend :: $(PIQI_ML_FILES) $(GENERATED_FILES)

clean::
	$(RRM) $(GENERATED_FILES)
	for d in $(DIRS) .; do \
		$(RRM) $$d/*.cmi $$d/*.cmo $$d/*.cmx $$d/*.o $$d/*.cmt $$d/*.cmti $$d/*.annot; \
	done

CAMLDOC_FLAGS=-sort -stars

camldoc: $(CMI_FILES) $(CMO_FILES)
	$(MKDIR_P) $(DOCDIR)
	$(CAMLFIND) $(CAMLDOC) $(CAMLDOC_FLAGS) $(PKG_FLAGS) $(CAMLINCLUDES) \
	-html -css-style "/assets/camldoc.css" -d $(DOCDIR) $(MLI_FILES)
# The link to the CSS is relative to the final destination of the doc

piqi/libcall_piqi.cmo: piqi/libcall_piqi.ml
	$(PP_BYT) $@
	$(CAMLBYT)  $(CAMLINCLUDES) -w +a-4-27 -c $<

piqi/libcall_piqi.cmx: piqi/libcall_piqi.ml
	$(PP_BYT) $@
	$(CAMLBIN) $(CAMLINCLUDES) -w +a-4-27 -c $<

endif

# include after having defined CAMLBIN & CAMLBY

BINSEC_SHARE_DIR = $(DATADIR)/binsec
VERSION = $(shell cat VERSION)

kernel/config.ml: Makefile
	echo "(* GENERATED by make *)" > $@
	echo "let bindir = \"$(BINDIR)\"" >> $@
	echo "let sharedir = \"$(BINSEC_SHARE_DIR)\"" >> $@
	echo "let version = \"$(VERSION)\"" >> $@

kernel/config.mli: Makefile
	echo "(* GENERATED by make *)" > $@
	echo "val bindir: string" >> $@
	echo "val sharedir : string" >> $@
	echo "val version : string" >> $@

config-clean:
	$(RRM) kernel/config.ml kernel/config.mli

beforedepend :: kernel/config.ml kernel/config.mli

install: $(EXE)
	$(PP) "Install $(EXE) to $(BINDIR)"
	$(INSTALL) $(EXE) $(BINDIR)
	$(PP) "Install $(SHARE_DIR) to $(BINSEC_SHARE_DIR)"
	$(MKDIR_P) $(BINSEC_SHARE_DIR)
	$(CP) $(wildcard $(SHARE_DIR)/*) $(BINSEC_SHARE_DIR)

.PHONY: uninstall
uninstall:
	$(PP) "Uninstall $(EXE)"
	$(RRM) $(BINDIR)/$(EXE)
	$(PP) "Uninstall share"
	$(RRM) $(BINSEC_SHARE_DIR)

.PHONY: $(SVG_FILES) doc-clean

doc-clean:
	$(RRM) $(DOCDIR) $(SVG_FILES)

clean:: doc-clean config-clean

.PHONY: all doc clean
