#
#  Main authors:
#    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
#
#  This file is part of Unison, see http://unison-code.github.io
#
#  Copyright (c) 2016, RISE SICS AB
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

UNISON_GHC_CONFIG := -O2

build:
	$(MAKE) build-unison
	$(MAKE) build-presolver build-solver

build-solver: $(SOLVERBIN)

build-presolver: $(PRESOLVERBIN)

build-unison: prepare-unison
	cd $(UNISONDIR); $(CABALBIN) build --ghc-options="$(UNISON_GHC_CONFIG)"

prepare-unison:
	$(MAKE) install-deps-unison
	$(MAKE) configure-unison

install-deps-unison:
	cd $(UNISONDIR); $(CABALBIN) install --only-dependencies --enable-tests

configure-unison:
	cd $(UNISONDIR); $(CABALBIN) configure --enable-tests

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

doc: prepare-unison prepare-specsgen
	cd $(UNISONDIR); $(CABALBIN) haddock
	cd $(UNISONSPECSGENDIR); $(CABALBIN) haddock

haskell-loc:
	cloc unison/src --exclude-dir=SpecsGen

cpp-loc:
	cloc solvers

UNISONSPECSGENDIR := unison-specsgen

SGHEXAGONROOTDIR   = $(UNISONDIR)/src/Unison/Target/Hexagon/SpecsGen
SGHEXAGONINPUTYAML = $(SGHEXAGONROOTDIR)/input/hexagon.yaml
SGHEXAGONAUXYAML   = $(SGHEXAGONROOTDIR)/input/hexagon-manual.yaml
SGHEXAGONREMATYAML = $(SGHEXAGONROOTDIR)/input/hexagon-remat.yaml

SGMIPSROOTDIR   = $(UNISONDIR)/src/Unison/Target/Mips/SpecsGen
SGMIPSINPUTYAML = $(SGMIPSROOTDIR)/input/mips.yaml
SGMIPSAUXYAML   = $(SGMIPSROOTDIR)/input/mips-manual.yaml
SGMIPSREMATYAML = $(SGMIPSROOTDIR)/input/mips-remat.yaml

SGARMROOTDIR   = $(UNISONDIR)/src/Unison/Target/ARM/SpecsGen
SGARMINPUTYAML = $(SGARMROOTDIR)/input/arm.yaml
SGARMAUXYAML   = $(SGARMROOTDIR)/input/arm-manual.yaml
SGARMREMATYAML = $(SGARMROOTDIR)/input/arm-remat.yaml


build-specsgen: prepare-specsgen
	cd $(UNISONSPECSGENDIR); $(CABALBIN) build

prepare-specsgen:
	$(MAKE) install-deps-specsgen
	$(MAKE) configure-specsgen

install-deps-specsgen:
	cd $(UNISONSPECSGENDIR); $(CABALBIN) install --only-dependencies

configure-specsgen:
	cd $(UNISONSPECSGENDIR); $(CABALBIN) configure

install-specsgen:
	cp $(UNISONSPECSGENDIR)/dist/build/specsgen/specsgen $(BINDIR)

run-specsgen: run-specsgen-hexagon run-specsgen-mips run-specsgen-arm

run-specsgen-hexagon:
	specsgen --targetname=Hexagon $(SGHEXAGONINPUTYAML) $(SGHEXAGONAUXYAML) \
	         --outputdir=$(SGHEXAGONROOTDIR) \
	         --constantextend --infiniteregclass M32 --infiniteregclass M64 \
	         --infiniteregclass RM32 --infiniteregclass RM64 \
	         --rematfile=$(SGHEXAGONREMATYAML)

run-specsgen-mips:
	specsgen --targetname=Mips $(SGMIPSINPUTYAML) $(SGMIPSAUXYAML) \
	         --outputdir=$(SGMIPSROOTDIR) \
	         --infiniteregclass m32 --infiniteregclass m64 \
	         --infiniteregclass RM32 --infiniteregclass RM64 \
	         --boundregclass ptr_rc \
	         --rematfile=$(SGMIPSREMATYAML)

run-specsgen-arm:
	specsgen --targetname=ARM $(SGARMINPUTYAML) $(SGARMAUXYAML) \
	         --outputdir=$(SGARMROOTDIR) \
	         --regclass p2:CCR --infiniteregclass M32 --infiniteregclass M32t \
	         --infiniteregclass M64 --infiniteregclass M128 \
		 --infiniteregclass M512 \
	         --promoteeffect CPSR \
	         --infiniteregclass RM32 --infiniteregclass RM64 \
	         --rematfile=$(SGARMREMATYAML)
