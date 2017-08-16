/*
 *  Main authors:
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
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


#include "pressureschedulingbrancher.hpp"

class PressureSchedulingBrancher : public Brancher {

  enum SchedulingDecision { SCHEDULE, SPLIT };

  class SchedulingChoice :
    public DoubleChoice<PressureSchedulingBrancher, operation, int> {
  public:
    SchedulingChoice(const PressureSchedulingBrancher& b, operation o0, int n0)
      : DoubleChoice(b, o0, n0) {}
  };

  class SplitChoice :
    public DoubleChoice<PressureSchedulingBrancher, operation, int> {
  public:
    SplitChoice(const PressureSchedulingBrancher& b, operation o0, int n0)
      : DoubleChoice(b, o0, n0) {}
  };

protected:

  mutable operation so;
  mutable int sn;
  mutable int decision;
  ViewArray<Int::IntView> c;

public:

  PressureSchedulingBrancher(Home home, ViewArray<Int::IntView>& c0)
    : Brancher(home), so(-1), sn(-1), decision(-1), c(c0) {}

  static void post(Home home, ViewArray<Int::IntView>& c) {
    (void) new (home) PressureSchedulingBrancher(home, c);
  }

  virtual size_t dispose(Space& home) {
    (void) Brancher::dispose(home);
    return sizeof(*this);
  }

  PressureSchedulingBrancher(Space& home, PressureSchedulingBrancher& b)
    : Brancher(home,b), so(b.so), sn(b.sn), decision(b.decision)
  {
    c.update(home, b.c);
  }

  virtual Brancher* copy(Space& home) {
    return new (home) PressureSchedulingBrancher(home, *this);
  }

  virtual bool status(const Space& home) const {
    for (int oi = 0; oi < c.size(); ++oi)
      // o is an unassigned, ready operation
      if (ready(home, c[oi], oi) && !c[oi].assigned()) return true;
    return false;
  }

  virtual Choice* choice(Space& home) {

    const LocalModel& m = static_cast<const LocalModel&>(home);

    // select operation so

    unsigned int minsize = Int::Limits::max;
    int mino = -1;
    for (int oi = 0; oi < c.size(); ++oi)
      // o is an unassigned, ready operation
      if (ready(home, c[oi], oi) && !c[oi].assigned())
        if (c[oi].size() < minsize) {
          minsize = c[oi].size();
          mino = oi;
        }
    so = mino;
    operation o = m.input->in[m.b] + so;

    // select decision and cycle sn

    if (m.pressure_balance(o) >= 0.0) {
      // Consumer operation, try to schedule early
      decision = SCHEDULE;
      sn = c[so].min();
    } else { // Producer operation, try to schedule right before use
      // Earliest cycle of all consumers
      int ec = Int::Limits::max;
      // Maximum latency from producer operation o to a consumer
      int maxlat = 0;
      for (operand p : m.input->operands[o])
        if (!m.input->use[p])
          for (IntVarValues ti(m.y(p)); ti(); ++ti) {
            temporary t = m.input->temps[p][ti.val()];
            if (t != NULL_TEMPORARY)
              for (operand q : m.input->users[t])
                if (m.u(q, t).assigned() && m.u(q, t).val()) {
                  operation j = m.input->oper[q];
                  if (m.c(j).min() < ec) ec = m.c(j).min();
                  int lat = m.lt(p).max() + m.lt(q).max();
                  if (lat > maxlat) maxlat = lat;
                }
          }
      // Split cycle
      int lc = ec - maxlat;
      if (c[so].min() <= lc && c[so].max() > lc) {
        decision = SPLIT;
        sn = lc;
      } else { // The issue cycle of so is already split, try close cycles
        decision = SCHEDULE;
        if (c[so].max() <= lc) {
          sn = c[so].max();
        } else {
          sn = c[so].min();
        }
      }
    }
    assert(so >= 0);
    assert(sn >= 0);

    if (decision == SCHEDULE) {
      return new SchedulingChoice(*this, so, sn);
    } else if (decision == SPLIT) {
      return new SplitChoice(*this, so, sn);
    } else {
      GECODE_NEVER;
      return NULL;
    }

  }

  virtual Choice* choice(const Space&, Archive& e) {

    switch (decision) {
    case SCHEDULE:
      {
        operation o;
        int n;
        e >> o >> n;
        return new SchedulingChoice(*this, o, n);
      }
    case SPLIT:
      {
        operation o;
        int n;
        e >> o >> n;
        return new SplitChoice(*this, o, n);
      }
    default:
      {
        GECODE_NEVER;
        return NULL;
      }
    }

  }

  virtual ExecStatus commit(Space& home,
                            const Choice& ch,
                            unsigned int alternative) {

    if (typeid(ch) == typeid(SchedulingChoice)) {
      const SchedulingChoice& sc = static_cast<const SchedulingChoice&>(ch);
      operation o = sc.first;
      int n = sc.second;
      if (alternative == 0) // Issue o in n
        return me_failed(c[o].eq(home, n)) ? ES_FAILED : ES_OK;
      else // Do not issue o in n
        return me_failed(c[o].nq(home, n)) ? ES_FAILED : ES_OK;
    } else if (typeid(ch) == typeid(SplitChoice)) {
      const SplitChoice& sc = static_cast<const SplitChoice&>(ch);
      operation o = sc.first;
      int n = sc.second;
      if (alternative == 0) // Issue o before or at n
        return me_failed(c[o].lq(home, n)) ? ES_FAILED : ES_OK;
      else // Issue o after n
        return me_failed(c[o].gr(home, n)) ? ES_FAILED : ES_OK;
    }
    GECODE_NEVER;
    return ES_FAILED;
  }

  void print(const Space& home, const Gecode::Choice& ch,
             unsigned int alternative, std::ostream& out) const {

    const LocalModel& m = static_cast<const LocalModel&>(home);

    if (typeid(ch) == typeid(SchedulingChoice)) {
      const SchedulingChoice& sc = static_cast<const SchedulingChoice&>(ch);
      operation o = m.input->in[m.b] + sc.first;
      int n = sc.second;
      out << "c(o" << o << ") ";
      if (alternative == 0) // Issue o in n
        out << "= " << n;
      else // Do not issue o in n
        out << "!= " << n;
    } else if (typeid(ch) == typeid(SplitChoice)) {
      const SplitChoice& sc = static_cast<const SplitChoice&>(ch);
      operation o = m.input->in[m.b] + sc.first;
      int n = sc.second;
      out << "c(o" << o << ") ";
      if (alternative == 0) // Issue o before or at n
        out << "<= " << n;
      else // Issue o after n
        out << "> " << n;
    } else GECODE_NEVER;
    return;
  }

};

void branch_on_pressure_scheduling(Home home, const IntVarArgs& c0) {
  if (home.failed()) return;
  ViewArray<Int::IntView> c(home, c0);
  PressureSchedulingBrancher::post(home, c);
}
