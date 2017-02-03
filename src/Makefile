#
#  Main authors:
#    Roberto Castaneda Lozano <rcas@sics.se>
#
#  This file is part of Unison, see http://unison-code.github.io
#
#  Copyright (c) 2016, SICS Swedish ICT AB
#  All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions are met:
#  1. Redistributions of source code must retain the above copyright notice,
#     this list of conditions and the following disclaimer.
#  2. Redistributions in binary form must reproduce the above copyright notice,
#     this list of conditions and the following disclaimer in the documentation
#     and/or other materials provided with the distribution.
#  3. Neither the name of the copyright holder nor the names of its contributors
#     may be used to endorse or promote products derived from this software
#     without specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
#  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
#  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
#  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
#  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
#  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
#  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
#  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
#  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#  POSSIBILITY OF SUCH DAMAGE.
#
CABALBIN := cabal

PREFIX := /usr/local
BINDIR := $(PREFIX)/bin

UNISONDIR := unison

PRESOLVERDIR := solvers/gecode
PRESOLVERBIN := $(PRESOLVERDIR)/gecode-presolver

SOLVERDIR := solvers/gecode
SOLVERBIN := $(SOLVERDIR)/gecode-solver

UNISON_SHOW_DETAILS ?= always

build: build-unison build-presolver build-solver

build-solver: $(SOLVERBIN)

build-presolver: $(PRESOLVERBIN)

build-unison: prepare-unison
	cd $(UNISONDIR); $(CABALBIN) configure --enable-tests; $(CABALBIN) build

prepare-unison:
	cd $(UNISONDIR); $(CABALBIN) install --only-dependencies --enable-tests

test:
	cd $(UNISONDIR); \
	$(CABALBIN) test --show-details=$(UNISON_SHOW_DETAILS) \
	--test-option=--directory=$(CURDIR)/$(UNISONDIR)/test/fast \
	--test-option=--presolver=$(CURDIR)/$(PRESOLVERBIN) \
	--test-option=--solver=$(CURDIR)/$(SOLVERBIN) \
	--test-option=--showprogress

install: install-unison install-solver install-presolver

install-unison:
	cp $(UNISONDIR)/dist/build/uni/uni $(BINDIR)

install-solver:
	cp $(SOLVERBIN) $(BINDIR)

install-presolver:
	cp $(PRESOLVERBIN) $(BINDIR)

include $(PRESOLVERDIR)/presolver.mk
include $(SOLVERDIR)/solver.mk

clean:
	cd $(UNISONDIR); $(CABALBIN) clean
	cd $(UNISONSPECSGENDIR); $(CABALBIN) clean
	$(MAKE) veryclean-solver veryclean-presolver

doc:
	cd $(UNISONDIR); $(CABALBIN) haddock
	cd $(UNISONSPECSGENDIR); $(CABALBIN) haddock

UNISONSPECSGENDIR := unison-specsgen

SGHEXAGONROOTDIR   = $(UNISONDIR)/src/Unison/Target/Hexagon/SpecsGen
SGHEXAGONINPUTYAML = $(SGHEXAGONROOTDIR)/input/hexagon.yaml
SGHEXAGONAUXYAML   = $(SGHEXAGONROOTDIR)/input/hexagon-manual.yaml

SGMIPSROOTDIR   = $(UNISONDIR)/src/Unison/Target/Mips/SpecsGen
SGMIPSINPUTYAML = $(SGMIPSROOTDIR)/input/mips.yaml
SGMIPSAUXYAML   = $(SGMIPSROOTDIR)/input/mips-manual.yaml

SGARMROOTDIR   = $(UNISONDIR)/src/Unison/Target/ARM/SpecsGen
SGARMINPUTYAML = $(SGARMROOTDIR)/input/arm.yaml
SGARMAUXYAML   = $(SGARMROOTDIR)/input/arm-manual.yaml

build-specsgen: prepare-specsgen
	cd $(UNISONSPECSGENDIR); $(CABALBIN) configure; $(CABALBIN) build

prepare-specsgen:
	cd $(UNISONSPECSGENDIR); $(CABALBIN) install --only-dependencies

install-specsgen:
	cp $(UNISONSPECSGENDIR)/dist/build/specsgen/specsgen $(BINDIR)

run-specsgen: run-specsgen-hexagon run-specsgen-mips run-specsgen-arm

run-specsgen-hexagon:
	specsgen --targetname=Hexagon $(SGHEXAGONINPUTYAML) $(SGHEXAGONAUXYAML) \
	         --outputdir=$(SGHEXAGONROOTDIR) \
	         --constantextend --infiniteregclass m32 --infiniteregclass m64

run-specsgen-mips:
	specsgen --targetname=Mips $(SGMIPSINPUTYAML) $(SGMIPSAUXYAML) \
	         --outputdir=$(SGMIPSROOTDIR) \
	         --infiniteregclass m32 --infiniteregclass m64

run-specsgen-arm:
	specsgen --targetname=ARM $(SGARMINPUTYAML) $(SGARMAUXYAML) \
	         --outputdir=$(SGARMROOTDIR) \
	         --regclass p2:CCR --infiniteregclass M32 --infiniteregclass M32t \
	         --infiniteregclass M64 --infiniteregclass M128 \
	         --promoteeffect CPSR:
