/*
 *  Main authors:
 *    Roberto Castaneda Lozano <rcas@sics.se>
 *
 *  This file is part of Unison, see http://unison-code.github.io
 *
 *  Copyright (c) 2016, SICS Swedish ICT AB
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


#include "usersinspector.hpp"

UsersInspector::UsersInspector() : ConsoleInspector("Temporary users") {}

void UsersInspector::printb(const Model& m, ostream & pOs, block b) {
  for (temporary t : m.input->tmp[b]) {
    pOs << "t" << t << ":\t";
    vector<operand> ps;
    for (SetVarGlbValues i(m.users(t)); i(); ++i) ps.push_back(i.val());
    pOs << show(ps, ", ", "p", "{}");
    if (!m.users(t).assigned()) {
      pOs << "..";
      vector<operand> qs;
      for (SetVarLubValues i(m.users(t)); i(); ++i) qs.push_back(i.val());
      pOs << show(qs, ", ", "p", "{}");
    }
    pOs << endl;

  }
}

void GlobalUsersInspector::print(const Model& s, ostream & pOs) {

  const Model& m = static_cast<const Model&>(s);

  for (block b : m.input->B) {
    pOs << "b" << b << ":";
    pOs << endl << endl;
    printb(m, pOs, b);
    pOs << endl;
  }

}

void LocalUsersInspector::print(const Model& s, ostream & pOs) {
  const LocalModel& m = static_cast<const LocalModel&>(s);
  printb(m, pOs, m.b);
}
