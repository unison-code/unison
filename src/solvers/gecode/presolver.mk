#
#  Main authors:
#    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
#
#  Contributing authors:
#    Mats Carlsson <mats.carlsson@ri.se>
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

PRESOLVERMAIN = $(PRESOLVERDIR)/presolver

SOLVERCOMMONDIR := $(PRESOLVERDIR)/common
SOLVERMODELSDIR := $(PRESOLVERDIR)/models
SOLVERBRANCHERSDIR := $(SOLVERDIR)/branchers
SOLVERPROCEDURESDIR := $(PRESOLVERDIR)/procedures
SOLVERPRESOLVERDIR := $(PRESOLVERDIR)/presolver

SOLVERCOMMON := definitions util jsonutil
SOLVERMODELS := parameters options model relaxedmodel completemodel localmodel \
globalmodel
SOLVERBRANCHERS := filters merit value printers pressureschedulingbrancher \
routingbrancher
SOLVERPROCEDURES := presolverprocedures commonprocedures
SOLVERPRESOLVER := presolver-options presolve before_presolver diff_temps	\
dominance infeasible_presolver last_use precedences unsafe_temp auxiliary	\
congr digraph across

PRESOLVERCLASSES := $(addprefix $(SOLVERCOMMONDIR)/, $(SOLVERCOMMON))	\
$(addprefix $(SOLVERMODELSDIR)/, $(SOLVERMODELS)) $(addprefix		\
$(SOLVERBRANCHERSDIR)/, $(SOLVERBRANCHERS)) $(addprefix			\
$(SOLVERPROCEDURESDIR)/, $(SOLVERPROCEDURES)) $(addprefix		\
$(SOLVERPRESOLVERDIR)/, $(SOLVERPRESOLVER))

PRESOLVERCPPSRC := $(addsuffix .cpp, $(PRESOLVERMAIN) $(PRESOLVERCLASSES))
PRESOLVERHPPSRC := $(addsuffix .hpp, $(PRESOLVERCLASSES))

PRESOLVERSRC := $(PRESOLVERCPPSRC)

PRESOLVERPROJECT := $(PRESOLVERDIR)/presolver.pro

PRESOLVERGENMAKEFILE = $(PRESOLVERDIR)/presolver-generated.mk

OSX_PRESOLVERBIN = $(PRESOLVERDIR)/gecode-presolver.app/Contents/MacOS/gecode-presolver

$(PRESOLVERBIN): $(PRESOLVERGENMAKEFILE)
	$(MAKE) -C $(PRESOLVERDIR) -f $(notdir $<)
	if [ -e $(OSX_PRESOLVERBIN) ]; then \
	    cp $(OSX_PRESOLVERBIN) $(PRESOLVERBIN); \
	fi; \

$(PRESOLVERGENMAKEFILE): $(PRESOLVERPROJECT) $(PRESOLVERSRC)
	qmake TARGET="gecode-presolver" CONFIG+="$(UNISON_SOLVER_CONFIG)" -o $@ $<

clean-presolver:
	rm -f $(PRESOLVERDIR)/*.o $(PRESOLVERDIR)/*~ $(PRESOLVERGENMAKEFILE) $(PRESOLVERDIR)/moc_*.cpp

veryclean-presolver: clean-presolver
	rm -f $(PRESOLVERBIN)
