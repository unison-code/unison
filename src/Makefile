#
#  Main authors:
#    Roberto Castaneda Lozano <rcas@acm.org>
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
STACKBIN := stack

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

build-unison:
	$(STACKBIN) build unison \
	--ghc-options="$(UNISON_GHC_CONFIG)" \
	--copy-bins --local-bin-path $(UNISONDIR)/build

doc-unison:
	$(STACKBIN) haddock unison
	$(STACKBIN) haddock --open unison

test:
	$(STACKBIN) test unison \
	--test-arguments="--directory=$(CURDIR)/$(UNISONDIR)/test/fast \
	--presolver=$(CURDIR)/$(PRESOLVERBIN) \
	--solver=$(CURDIR)/$(SOLVERBIN) \
	--showprogress"

install: install-unison install-solver install-presolver

install-unison:
	cp $(UNISONDIR)/build/uni $(BINDIR)

install-solver:
	cp $(SOLVERBIN) $(BINDIR)

install-presolver:
	cp $(PRESOLVERBIN) $(BINDIR)

include $(PRESOLVERDIR)/presolver.mk
include $(SOLVERDIR)/solver.mk

clean:
	$(STACKBIN) clean unison
	$(STACKBIN) clean unison-specsgen
	$(MAKE) veryclean-solver veryclean-presolver

doc: doc-unison doc-specsgen

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

SGMINIMALROOTDIR   = $(UNISONDIR)/src/Unison/Target/Minimal/SpecsGen
SGMINIMALINPUTYAML = $(SGMINIMALROOTDIR)/input/minimal.yaml

build-specsgen:
	$(STACKBIN) build unison-specsgen \
	--ghc-options="$(UNISON_GHC_CONFIG)" \
	--copy-bins --local-bin-path $(UNISONSPECSGENDIR)/build

doc-specsgen:
	$(STACKBIN) haddock unison-specsgen
	$(STACKBIN) haddock --open unison-specsgen

install-specsgen:
	cp $(UNISONSPECSGENDIR)/build/specsgen $(BINDIR)

run-specsgen: run-specsgen-hexagon run-specsgen-mips run-specsgen-arm \
	            run-specsgen-minimal

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

run-specsgen-minimal:
	specsgen --targetname=Minimal $(SGMINIMALINPUTYAML) $(SGMINIMALAUXYAML) \
	         --outputdir=$(SGMINIMALROOTDIR) \
	         --infiniteregclass M32
