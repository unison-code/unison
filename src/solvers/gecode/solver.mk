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

SOLVERCOMMONDIR := $(SOLVERDIR)/common
SOLVERMODELSDIR := $(SOLVERDIR)/models
SOLVERBRANCHERSDIR := $(SOLVERDIR)/branchers
SOLVERPROCEDURESDIR := $(SOLVERDIR)/procedures
SOLVERINSPECTORSDIR := $(SOLVERDIR)/inspectors

SOLVERMAIN = $(SOLVERDIR)/solver

SOLVERCOMMON := definitions util jsonutil
SOLVERMODELS := parameters options model localmodel completemodel globalmodel	\
simplemodel relaxedmodel
SOLVERBRANCHERS := filters merit value printers pressureschedulingbrancher	\
routingbrancher
SOLVERPROCEDURES := commonprocedures globalprocedures localprocedures
SOLVERINSPECTORS := consoleinspector modelgraphicsview modelinspector dot	\
registerarrayinspector issuecycleinspector liverangeinspector			\
assignmentinspector allocationinspector livedurationinspector			\
selectioninspector operandassignmentinspector resourceconsumptioninspector	\
dataflowinspector alignmentinspector alignmentpartitioninspector		\
operandallocationinspector congruenceallocationinspector precedenceinspector	\
precedencematrixinspector usersinspector operandlatencyinspector

SOLVERCLASSES := $(addprefix $(SOLVERCOMMONDIR)/, $(SOLVERCOMMON)) $(addprefix	\
$(SOLVERMODELSDIR)/, $(SOLVERMODELS)) $(addprefix $(SOLVERBRANCHERSDIR)/,	\
$(SOLVERBRANCHERS)) $(addprefix $(SOLVERPROCEDURESDIR)/, $(SOLVERPROCEDURES))	\
$(addprefix $(SOLVERINSPECTORSDIR)/, $(SOLVERINSPECTORS))

SOLVERCPPSRC := $(addsuffix .cpp, $(SOLVERMAIN) $(SOLVERCLASSES))
SOLVERHPPSRC := $(addsuffix .hpp, $(SOLVERCLASSES))

SOLVERSRC := $(SOLVERCPPSRC) $(SOLVERHPPSRC)

UNISON_SOLVER_CONFIG ?= graphics

SOLVERPROJECT := $(SOLVERDIR)/solver.pro

GENMAKEFILE = $(SOLVERDIR)/generated.mk
GENMAKEFILESTATIC = $(SOLVERDIR)/generated-static.mk

OSX_SOLVERBIN = $(SOLVERDIR)/gecode-solver.app/Contents/MacOS/gecode-solver

$(SOLVERBIN): $(GENMAKEFILE)
	$(MAKE) -C $(SOLVERDIR) -f $(notdir $<)
	if [ -e $(OSX_SOLVERBIN) ]; then \
	    cp $(OSX_SOLVERBIN) $(SOLVERBIN); \
	fi; \

$(GENMAKEFILE): $(SOLVERPROJECT) $(SOLVERSRC)
	qmake TARGET="gecode-solver" CONFIG+="$(UNISON_SOLVER_CONFIG)" -o $@ $<

$(SOLVERSTATICBIN): $(GENMAKEFILESTATIC)
	$(MAKE) -C $(SOLVERDIR) -f $(notdir $<)
	strip --strip-debug $(SOLVERSTATICBIN)

$(GENMAKEFILESTATIC): $(SOLVERPROJECT) $(SOLVERSRC)
	qmake TARGET="gecode-solver-static" CONFIG+="static" -o $@ $<

clean-solver:
	rm -f $(SOLVERDIR)/*.o $(SOLVERDIR)/*~ $(GENMAKEFILE) $(GENMAKEFILESTATIC) $(SOLVERDIR)/moc_*.cpp

veryclean-solver: clean-solver
	rm -f $(SOLVERBIN)
