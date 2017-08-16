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


#include "routingbrancher.hpp"

class RoutingBrancher : public Brancher {

  enum RoutingDecision { ACTIVENESS, INSTRUCTION, TEMPORARY };

  class ActivenessChoice : public SingleChoice<RoutingBrancher, unsigned int> {
  public:
    ActivenessChoice(const RoutingBrancher& b, unsigned int oi0)
      : SingleChoice(b, oi0) {}
  };

  class InstructionChoice :
    public DoubleChoice<RoutingBrancher, unsigned int, int> {
  public:
    InstructionChoice(const RoutingBrancher& b, unsigned int oi0, int ii0)
      : DoubleChoice(b, oi0, ii0) {}
  };

  class TemporaryChoice :
    public DoubleChoice<RoutingBrancher, unsigned int, int> {
  public:
    TemporaryChoice(const RoutingBrancher& b, unsigned int pi0, int ti0)
      : DoubleChoice(b, pi0, ti0) {}
  };

protected:

  mutable int decision;
  mutable int oi;
  mutable int pi;
  ViewArray<Int::BoolView> a;
  ViewArray<Int::IntView> i;
  ViewArray<Int::IntView> t;
  bool aggressive;
  const vector<operation> os;
  const vector<operand> ps;

public:

  RoutingBrancher(Home home, ViewArray<Int::BoolView>& a0,
                  ViewArray<Int::IntView>& i0,
                  ViewArray<Int::IntView>& t0,
                  bool aggressive0,
                  vector<operation>& os0, vector<operand>& ps0)
    : Brancher(home), decision(-1), oi(-1), pi(-1), a(a0), i(i0), t(t0),
      aggressive(aggressive0), os(os0), ps(ps0) {}

  static void post(Home home, ViewArray<Int::BoolView>& a,
                   ViewArray<Int::IntView>& i, ViewArray<Int::IntView>& t,
                   bool aggressive,
                   vector<operation>& os, vector<operand>& ps) {
    (void) new (home) RoutingBrancher(home, a, i, t, aggressive, os, ps);
  }

  virtual size_t dispose(Space& home) {
    (void) Brancher::dispose(home);
    return sizeof(*this);
  }

  RoutingBrancher(Space& home, RoutingBrancher& b)
    : Brancher(home,b), decision(b.decision), oi(b.oi), pi(b.pi),
      aggressive(b.aggressive), os(b.os), ps(b.ps)
  {
    a.update(home, b.a);
    i.update(home, b.i);
    t.update(home, b.t);
  }

  virtual Brancher* copy(Space& home) {
    return new (home) RoutingBrancher(home, *this);
  }

  virtual bool status(const Space&) const {
    for (int oi = 0; oi < a.size(); ++oi) {
      if (!a[oi].assigned()) {
        decision = ACTIVENESS;
        this->oi = oi;
        return true;
      }
      if (!i[oi].assigned()) {
        decision = INSTRUCTION;
        this->oi = oi;
        return true;
      }
    }
    for (int pi = 0; pi < t.size(); ++pi) {
      if (!t[pi].assigned()) {
        decision = TEMPORARY;
        this->pi = pi;
        return true;
      }
    }

    return false;
  }

  virtual Choice* choice(Space&) {

    if (decision == ACTIVENESS) {
      return new ActivenessChoice(*this, oi);
    } else if (decision == INSTRUCTION) {
      return new InstructionChoice(*this, oi, aggressive ? i[oi].min() : i[oi].max());
    } else if (decision == TEMPORARY) {
      return new TemporaryChoice(*this, pi, t[pi].min());
    } else {
      GECODE_NEVER;
      return NULL;
    }

  }

  virtual Choice* choice(const Space&, Archive& e) {

    // TODO: this function should not rely on 'decision', all information should
    // be embedded into the archived choice e

    switch (decision) {
    case ACTIVENESS:
      {
        unsigned int oi;
        e >> oi;
        return new ActivenessChoice(*this, oi);
      }
    case INSTRUCTION:
      {
        unsigned int oi;
        int ii;
        e >> oi >> ii;
        return new InstructionChoice(*this, oi, ii);
      }
    case TEMPORARY:
      {
        unsigned int pi;
        int ti;
        e >> pi >> ti;
        return new TemporaryChoice(*this, pi, ti);
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

    if (typeid(ch) == typeid(ActivenessChoice)) {
      const ActivenessChoice& ac = static_cast<const ActivenessChoice&>(ch);
      unsigned int oi = ac.first;
      if (!aggressive) alternative = !alternative;
      if (alternative == 0) // Make o inactive
        return me_failed(a[oi].eq(home, 0)) ? ES_FAILED : ES_OK;
      else // Make o active
        return me_failed(a[oi].eq(home, 1)) ? ES_FAILED : ES_OK;
    } else if (typeid(ch) == typeid(InstructionChoice)) {
      const InstructionChoice& oc = static_cast<const InstructionChoice&>(ch);
      unsigned int oi = oc.first;
      int ii = oc.second;
      if (alternative == 0) // Implement o with instruction ii
        return me_failed(i[oi].eq(home, ii)) ? ES_FAILED : ES_OK;
      else // Do not implement o with instruction ii
        return me_failed(i[oi].nq(home, ii)) ? ES_FAILED : ES_OK;
    } else if (typeid(ch) == typeid(TemporaryChoice)) {
      const TemporaryChoice& tc = static_cast<const TemporaryChoice&>(ch);
      unsigned int pi = tc.first;
      int ti = tc.second;
      if (alternative == 0) // Connect p to temporary ti
        return me_failed(t[pi].eq(home, ti)) ? ES_FAILED : ES_OK;
      else // Do not connect p to temporary ti
        return me_failed(t[pi].nq(home, ti)) ? ES_FAILED : ES_OK;
    }
    GECODE_NEVER;
    return ES_FAILED;
  }

  void print(const Space& home, const Gecode::Choice& ch,
             unsigned int alternative, std::ostream& out) const {

    const LocalModel& m = static_cast<const LocalModel&>(home);

    if (typeid(ch) == typeid(ActivenessChoice)) {
      const ActivenessChoice& sc = static_cast<const ActivenessChoice&>(ch);
      operation o = os[sc.first];
      if (alternative == 0) // Make o inactive
        out << "!";
      out << "a(o" << o << ")";
    } else if (typeid(ch) == typeid(InstructionChoice)) {
      const InstructionChoice& sc = static_cast<const InstructionChoice&>(ch);
      operation o = os[sc.first];
      int ii = sc.second;
      out << "i(o" << o << ") ";
      if (alternative == 0) out << "=";
      else out << "!=";
      out << " " << m.input->insname[m.input->instructions[o][ii]];
    } else if (typeid(ch) == typeid(TemporaryChoice)) {
      const TemporaryChoice& tc = static_cast<const TemporaryChoice&>(ch);
      operand p = ps[tc.first];
      int ti = tc.second;
      out << "t(p" << p << ") ";
      if (alternative == 0) out << "=";
      else out << "!=";
      out << "t" << m.input->temps[p][ti];
    } else GECODE_NEVER;
    return;
  }

};

void branch_on_routing(Home home, const BoolVarArgs& a0, const IntVarArgs& i0,
                       const IntVarArgs& t0, bool aggressive,
                       vector<operation>& os, vector<operand>& ps) {
  if (home.failed()) return;
  ViewArray<Int::BoolView> a(home, a0);
  ViewArray<Int::IntView> i(home, i0);
  ViewArray<Int::IntView> t(home, t0);
  RoutingBrancher::post(home, a, i, t, aggressive, os, ps);
}
