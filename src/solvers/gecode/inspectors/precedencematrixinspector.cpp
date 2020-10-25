/*
 *  Main authors:
 *    Roberto Castaneda Lozano <rcas@acm.org>
 *
 *  This file is part of Unison, see http://unison-code.github.io
 *
 *  Copyright (c) 2016, RISE SICS AB
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright notice,
 *     this list of conditions and the following disclaimer in the documentation
 *     and/or other materials provided with the distribution.
 *  3. Neither the name of the copyright holder nor the names of its
 *     contributors may be used to endorse or promote products derived from this
 *     software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 */


#include "precedencematrixinspector.hpp"

PrecedenceMatrixInspector::PrecedenceMatrixInspector() :
  ConsoleInspector("Precedence matrix") {}

void PrecedenceMatrixInspector::printb(const LocalModel& m0, ostream & pOs) {

  // Do noy worry, m will not be written to via p(o1, o2) since disabling
  // precedence variables disables this inspector
  LocalModel& m = const_cast<LocalModel&> (m0);

  int w = 1;
  for (operation o : m.input->mandatory[m.b]) {
    QString lab = "o" + QString::number(o);
    if (lab.size() > w) w = lab.size();
  }
  w++;

  pOs << fill("", w).toUtf8().constData();
  print_header(m, w, pOs);
  pOs << endl;
  for (operation o1 : m.input->mandatory[m.b]) {
    pOs << fill("o" + QString::number(o1), w).toUtf8().constData();
    for (operation o2 : m.input->mandatory[m.b])
      pOs << fill(sign(m.p(o1, o2)), w).toUtf8().constData();
    pOs << endl;
  }
}

void PrecedenceMatrixInspector::print_header(const LocalModel& m, int w, ostream & pOs) {
  for (operation o : m.input->mandatory[m.b])
    pOs << fill("o" + QString::number(o), w).toUtf8().constData();
}

void LocalPrecedenceMatrixInspector::print(const Model& s, ostream & pOs) {
  const LocalModel& m = static_cast<const LocalModel&>(s);
  printb(m, pOs);
}
