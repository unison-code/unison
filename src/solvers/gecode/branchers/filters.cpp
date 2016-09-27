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


#include "filters.hpp"

bool schedulable(const Space& s, IntVar, int oi) {
  const Model& m = static_cast<const Model&>(s);
  return !m.v_a[oi].assigned() || m.v_a[oi].val();
}

bool ready(const Space& s, IntVar c, int oi) {
  if (!schedulable(s, c, oi)) return false;
  const LocalModel& m = static_cast<const LocalModel&>(s);
  operation o = m.input->in[m.b] + oi;
  // Test whether i is data-ready
  for (operand p : m.input->operands[o])
    if (m.input->use[p] && !m.is_disconnected(p)) {
      if (!m.y(p).assigned()) return false;
      temporary t = m.input->temps[p][m.y(p).val()];
      operation d = m.input->def_opr[t];
      // d is active since t is live
      if (!m.c(d).assigned()) return false;
    }
  // Test whether i is fixed-dependency ready
  for (operation pr : m.input->fixed_predecessors[o])
    if (!m.a(pr).assigned() || (m.a(pr).val() && !m.c(pr).assigned()))
      return false;
  return true;
}

bool assignable(const Space& s, IntVar, int ti) {
  const LocalModel& m = static_cast<const LocalModel&>(s);
  temporary t = m.input->tmp[m.b][0] + ti;
  return !m.l(t).assigned() || m.l(t).val();
}

bool allocatable(const Space& s, SetVar pals, global_congruence g) {
  const Model& m = static_cast<const Model&>(s);
  for (operand p : m.input->congr[m.input->regular[g]]) {
    assert(m.x(p).assigned());
    if (!m.x(p).val()) return false;
  }
  for (SetVarUnknownValues i(pals); i(); ++i)
    if (m.input->candidate_spaces[g].count(i.val())) return true;
  return false;
}

bool global_assignable(const Space& s, IntVar, temporary t) {
  const GlobalModel& m = static_cast<const GlobalModel&>(s);
  return !m.l(t).assigned() || m.l(t).val();
}
