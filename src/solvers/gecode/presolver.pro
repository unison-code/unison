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

HEADERS += common/definitions.hpp \
           common/util.hpp \
           common/jsonutil.hpp \
           models/parameters.hpp \
           models/options.hpp \
           models/model.hpp \
           models/relaxedmodel.hpp \
           models/completemodel.hpp \
           models/localmodel.hpp \
           models/globalmodel.hpp \
           branchers/filters.hpp \
           branchers/merit.hpp \
           branchers/value.hpp \
           branchers/printers.hpp \
           branchers/pressureschedulingbrancher.hpp \
           branchers/routingbrancher.hpp \
           procedures/commonprocedures.hpp \
           procedures/presolverprocedures.hpp \
           presolver/presolver-options.hpp \
           presolver/across.hpp \
           presolver/auxiliary.hpp \
           presolver/before_presolver.hpp \
           presolver/congr.hpp \
           presolver/diff_temps.hpp \
           presolver/digraph.hpp \
           presolver/dominance.hpp \
           presolver/infeasible_presolver.hpp \
           presolver/last_use.hpp \
           presolver/precedences.hpp \
           presolver/presolve.hpp \
           presolver/quasi_adjacent.hpp \
           presolver/unsafe_temp.hpp

SOURCES += third-party/jsoncpp/json_reader.cpp \
           third-party/jsoncpp/json_value.cpp \
           third-party/jsoncpp/json_writer.cpp \
           common/definitions.cpp \
           common/util.cpp \
           common/jsonutil.cpp \
           models/parameters.cpp \
           models/options.cpp \
           models/model.cpp \
           models/relaxedmodel.cpp \
           models/completemodel.cpp \
           models/localmodel.cpp \
           models/globalmodel.cpp \
           branchers/filters.cpp \
           branchers/merit.cpp \
           branchers/value.cpp \
           branchers/printers.cpp \
           branchers/pressureschedulingbrancher.cpp \
           branchers/routingbrancher.cpp \
           procedures/commonprocedures.cpp \
           procedures/presolverprocedures.cpp \
           presolver/presolver-options.cpp \
           presolver/across.cpp \
           presolver/auxiliary.cpp \
           presolver/before_presolver.cpp \
           presolver/congr.cpp \
           presolver/diff_temps.cpp \
           presolver/digraph.cpp \
           presolver/dominance.cpp \
           presolver/infeasible_presolver.cpp \
           presolver/last_use.cpp \
           presolver/precedences.cpp \
           presolver/presolve.cpp \
           presolver/quasi_adjacent.cpp \
           presolver/unsafe_temp.cpp \
           presolver.cpp

QMAKE_CXXFLAGS += -std=c++0x

LIBS += -lpthread -lgecodedriver
CONFIG(graphics) {
  LIBS -= -lpthread
  LIBS += -lgecodegist
}
LIBS += -lgecodesearch -lgecodeminimodel -lgecodeset -lgecodefloat -lgecodeint \
        -lgecodekernel -lgecodesupport

TARGET = gecode-presolver
CONFIG -= qt
CONFIG += warn_on
QT += script

CONFIG(graphics) {

  CONFIG += qt

  DEFINES += GRAPHICS NEWGV

  LIBS += -lgvc -lcgraph -lcdt

}

# Add 'profiling' to the CONFIG variable to generate gprof information
CONFIG(profiling) {
    QMAKE_CFLAGS+=-pg
    QMAKE_CXXFLAGS+=-pg
    QMAKE_LFLAGS+=-pg
}
