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


#include "congruenceallocationinspector.hpp"

GlobalCongruenceAllocationInspector::GlobalCongruenceAllocationInspector() :
  ConsoleInspector("Congruence allocation") {}

void GlobalCongruenceAllocationInspector::print(const Model& s, ostream & pOs) {

  const GlobalModel& m = static_cast<const GlobalModel&>(s);

  int max = 0;
  for (global_congruence g : m.input->G) {
    int n = show_global_congruence(m, g).length();
    if (n > max) max = n;
  }

  for (global_congruence g : m.input->G) {

    pOs <<
      fill(show_global_congruence(m, g) + ": ", max + 2).toUtf8().constData();

    pOs << "{";
    vector<QString> grss;
    for (SetVarGlbValues i(m.pals(g)); i(); ++i)
      grss.push_back(show_space(i.val(), m.input).c_str());
    pOs << sepBy(grss, QString(", ")).toUtf8().constData();
    pOs << "}";
    if (!m.pals(g).assigned()) {
      pOs << "..{";
      vector<QString> lrss;
      for (SetVarLubValues i(m.pals(g)); i(); ++i)
        lrss.push_back(show_space(i.val(), m.input).c_str());
      pOs << sepBy(lrss, QString(", ")).toUtf8().constData();
      pOs << "}";
    }
    pOs << endl;

  }

}
