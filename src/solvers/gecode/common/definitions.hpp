/*
 *  Main authors:
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
 *    Mats Carlsson <mats.carlsson@ri.se>
 *
 *  Contributing authors:
 *    Noric Couderc <noric@sics.se>
 *    Mikael Almgren <mialmg@kth.se>
 *    Erik Ekstrom <eeks@sics.se>
 *    Daniel Lundén <daniel.lunden@sics.se>
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


#ifndef __SOLVER_DEFINITIONS__
#define __SOLVER_DEFINITIONS__

#include <vector>
#include <iostream>

using namespace std;

typedef int operation;
typedef int temporary;
typedef int operand;
typedef int instruction;
typedef int register_space;
typedef int register_class;
typedef int register_atom;
typedef int block;
typedef int resource;
typedef int congruence;
typedef int alignable;
typedef int global_congruence;
typedef int activation_class;
typedef int latency;
typedef int global_cluster;
typedef int issue_cycle;

enum OperationType
  { LINEAR, BRANCH, CALL, TAILCALL, IN, OUT, KILL, DEFINE, COMBINE, LOW, HIGH,
    SPLIT2, SPLIT4, FUN, COPY };

const instruction NULL_INSTRUCTION = 0;
const temporary NULL_TEMPORARY = -1;
const temporary NULL_REGISTER = -1;
const operation NULL_OPERATION = -1;
const operation NULL_ACTIVATION_CLASS = -1;

enum SchedulingStrategy { EARLIEST_INSTRUCTION, ISSUE_CYCLE_SIZE_MIN };

enum GoalType { STATIC_GOAL, DYNAMIC_GOAL };

const resource ISSUE_CYCLES = -1;

// Key of each search strategy
const char AGGRESSIVE_SEARCH = 'a';
const char TRIVIAL_SEARCH = 't';
const char MINIMUM_COST_SEARCH = 'm';
const char FAIL_FIRST_SEARCH = 'f';
const char CONSERVATIVE_SEARCH = 'c';

// Numeric ID of a Unison constraint expresion

enum UnisonConstraintExprId
  { OR_EXPR,
    AND_EXPR,
    XOR_EXPR,
    IMPLIES_EXPR,
    NOT_EXPR,
    ACTIVE_EXPR,		// ex-PRESOLVER_ACTIVENESS
    CONNECTS_EXPR,		// ex-PRESOLVER_OPERAND_TEMPORARY
    IMPLEMENTS_EXPR,		// ex-PRESOLVER_OPERATION
    DISTANCE_EXPR,
    SHARE_EXPR,			// ex-PRESOLVER_EQUAL_TEMPORARIES
    OPERAND_OVERLAP_EXPR,	// ex-PRESOLVER_OVERLAPPING_OPERANDS
    TEMPORARY_OVERLAP_EXPR,	// ex-PRESOLVER_OVERLAPPING_TEMPORARIES
    CALLER_SAVED_EXPR,		// ex-PRESOLVER_CALLER_SAVED_TEMPORARY
    ALLOCATED_EXPR,		// ex-PRESOLVER_OPERAND_CLASS
    ALIGNED_EXPR
  };


// Unison constraint expression

class UnisonConstraintExpr {
public:
  // default constructor means "true"
 UnisonConstraintExpr() : id(AND_EXPR), data({}), children({}) {}
 UnisonConstraintExpr(UnisonConstraintExprId id0, vector<int> data0,
                      vector<UnisonConstraintExpr> children0)
   : id(id0), data(data0), children(children0) {}
  UnisonConstraintExprId id;
  vector<int> data;
  vector<UnisonConstraintExpr> children;
  bool operator<(const UnisonConstraintExpr & e) const {
    if(id != e.id) return id < e.id;
    else if(data != e.data) return data < e.data;
    else return children < e.children;
  }
  bool operator==(const UnisonConstraintExpr& e) const {
    return (id == e.id) && (data == e.data) && (children == e.children);
  }
  bool operator!=(const UnisonConstraintExpr& e) const {
    return (id != e.id) || (data != e.data) || (children != e.children);
  }
};

typedef tuple<operand, vector<register_atom> > AvoidHint;

typedef vector<UnisonConstraintExpr> presolver_conj;

typedef vector<presolver_conj> presolver_disj;

class PresolverActiveTable {
public:
  vector<operation> os;
  vector<vector<int> > tuples;
  bool operator<(const PresolverActiveTable &t) const {
    if (os != t.os) return os < t.os;
    else return tuples < t.tuples;
  }
  bool operator==(const PresolverActiveTable& t) const {
    return (os == t.os) && (tuples == t.tuples);
  }
};

class PresolverCopyTmpTable {
public:
  vector<operation> os;
  vector<operand> ps;
  vector<vector<int> > tuples;
  bool operator==(const PresolverCopyTmpTable& t) const {
    return (os == t.os) && (ps == t.ps) && (tuples == t.tuples);
  }
};

class PresolverBeforeJSON {
public:
  operand p, q;
  UnisonConstraintExpr e;
  PresolverBeforeJSON() : p(-1), q(-1), e() {}
  PresolverBeforeJSON(operation p, operation q, UnisonConstraintExpr e):
    p(p), q(q), e(e) {}
  bool operator<(const PresolverBeforeJSON &b) const {
    if (p != b.p) return p < b.p;
    else if (q != b.q) return q < b.q;
    else return e < b.e;
  }
  bool operator==(const PresolverBeforeJSON& b) const {
    return (p == b.p) && (q == b.q) && (e == b.e);
  }
};

class PresolverAcrossItemJSON {
public:
  temporary t;
  UnisonConstraintExpr e;
  PresolverAcrossItemJSON() : t(-1), e() {}
  PresolverAcrossItemJSON(temporary t, UnisonConstraintExpr e):
    t(t), e(e) {}
  bool operator<(const PresolverAcrossItemJSON& that) const {
    if (t != that.t) return t < that.t;
    return e < that.e;
  }
  bool operator==(const PresolverAcrossItemJSON& that) const {
    return (t == that.t) && (e == that.e);
  }
};

class PresolverAcrossItem {
public:
  temporary t;
  presolver_disj d;
  bool operator<(const PresolverAcrossItem& that) const {
    if (t != that.t) return t < that.t;
    return d < that.d;
  }
  bool operator==(const PresolverAcrossItem& that) const {
    return (t == that.t) && (d == that.d);
  }
};

class PresolverAcross {
public:
  operation o;
  vector<register_atom> ras;
  vector<PresolverAcrossItem> as;
  bool operator<(const PresolverAcross &that) const {
    if (o != that.o) return o < that.o;
    if (ras != that.ras) return ras < that.ras;
    return as < that.as;
  }
  bool operator==(const PresolverAcross& that) const {
    return (o == that.o) && (ras == that.ras) && (as == that.as);
  }
};

class PresolverAcrossJSON {
public:
  operation o;
  vector<register_atom> ras;
  vector<PresolverAcrossItemJSON> as;
  bool operator<(const PresolverAcrossJSON &that) const {
    if (o != that.o) return o < that.o;
    if (ras != that.ras) return ras < that.ras;
    return as < that.as;
  }
  bool operator==(const PresolverAcrossJSON& that) const {
    return (o == that.o) && (ras == that.ras) && (as == that.as);
  }
};

class PresolverSetAcross {
public:
  operation o;
  vector<register_atom> ras;
  vector<vector<temporary> > tsets;
  bool operator==(const PresolverSetAcross& that) const {
    return (o == that.o) && (ras == that.ras) && (tsets == that.tsets);
  }
};

class PresolverDominates {
public:
  operation o1, o2;
  vector<instruction> ins;
  vector<temporary> temps;
  bool operator<(const PresolverDominates &d) const {
    if (o1 != d.o1) return o1 < d.o1;
    else if (o2 != d.o2) return o2 < d.o2;
    else if (ins != d.ins) return ins < d.ins;
    else return temps < d.temps;
  }
  bool operator==(const PresolverDominates& that) const {
    return (o1 == that.o1) && (o2 == that.o2) && (ins == that.ins) && (temps == that.temps);
  }
};

class PresolverPred {
public:
  vector<operation> p;
  operation q;
  int d;
  bool operator==(const PresolverPred& that) const {
    return (p == that.p) && (q == that.q) && (d == that.d);
  }
};

class PresolverSucc {
public:
  operation p;
  vector<operation> q;
  int d;
  bool operator==(const PresolverSucc& that) const {
    return (p == that.p) && (q == that.q) && (d == that.d);
  }
};

class PresolverInstrCond {
public:
  operation o;
  instruction i;
  operand q;
  bool operator==(const PresolverInstrCond& that) const {
    return (o == that.o) && (i == that.i) && (q == that.q);
  }
};

class PresolverValuePrecedeChain {
public:
  vector<temporary> ts;
  vector<vector<register_atom>> rss;
  bool operator==(const PresolverValuePrecedeChain& that) const {
    return (ts == that.ts) && (rss == that.rss);
  }
};

class PresolverInsnClass {
public:
  instruction insn;
  vector<register_class> rclass;
  bool operator<(const PresolverInsnClass& that) const {
    if (insn != that.insn) return insn < that.insn;
    else return rclass < that.rclass;
  }
  bool operator==(const PresolverInsnClass& that) const {
    return (insn == that.insn) && (rclass == that.rclass);
  }
};

class PresolverInsn2Class2 {
public:
  instruction insn1;
  instruction insn2;
  vector<register_class> class1;
  vector<register_class> class2;
  bool operator<(const PresolverInsn2Class2& that) const {
    if (insn1 != that.insn1) return insn1 < that.insn1;
    else if (insn2 != that.insn2) return insn2 < that.insn2;
    else if (class1 != that.class1) return class1 < that.class1;
    else return class2 < that.class2;
  }
  bool operator==(const PresolverInsn2Class2& that) const {
    return (insn1 == that.insn1) && (insn2 == that.insn2) && (class1 == that.class1) && (class2 == that.class2);
  }
};

class PrecedenceEdge {
public:
  operation i, j;
  latency n;
  bool operator<(const PrecedenceEdge& p) const {
    if(i != p.i) return i < p.i;
    else if(j != p.j) return j < p.j;
    else return n < p.n;
  }
  bool operator==(const PrecedenceEdge& p) const {
    return i == p.i && j == p.j && n == p.n;
  }
};

class PresolverPrecedence {
public:
  operation i, j;
  int n;
  presolver_disj d;
  // CONSTRUCTORS
  PresolverPrecedence() : i(-1), j(-1), n(-1), d() { }
  PresolverPrecedence(operation i, operation j, latency n, presolver_disj d):
      i(i), j(j), n(n), d(d) { }
  // OPERATORS
  bool operator<(const PresolverPrecedence & p) const {
    if(i != p.i) return i < p.i;
    else if(j != p.j) return j < p.j;
    else if(n != p.n) return n < p.n;
    else return d < p.d;
  }
  bool operator==(const PresolverPrecedence & p) const {
    return i == p.i && j == p.j && n == p.n && d == p.d;
  }
};

class PresolverBefore {
public:
  operand p, q;
  presolver_disj d;
  bool operator<(const PresolverBefore &b) const {
    if (p != b.p) return p < b.p;
    else if (q != b.q) return q < b.q;
    else return d < b.d;
  }
  bool operator==(const PresolverBefore& b) const {
    return (p == b.p) && (q == b.q) && (d == b.d);
  }
};

class PresolverAcrossTuple {
public:
  operation o;
  temporary t;
  presolver_disj d;
};


typedef pair<operation, unsigned int> InstructionAssignment;

#ifdef GRAPHICS
#define JSONVALUE QScriptValue
#define GIST_OPTIONS Gist::Options
#else
#define JSONVALUE Json::Value
#define GIST_OPTIONS GistOptionsStub
#endif

class GistOptionsStub {};

enum RegisterAtomClass
  {RA_CALLEE_SAVED, RA_CALLER_SAVED, RA_RESERVED};

enum TemporandType
  {TEMPORAND_OPERAND, TEMPORAND_TEMPORARY, TEMPORAND_REGISTER, TEMPORAND_NONE};

class Temporand {
private:
  int _id;
  TemporandType _type;
public:
  Temporand();
  Temporand(int id, TemporandType t);
  Temporand(const Temporand& t);

  bool operator==(const Temporand& t) const;

  bool operator!=(const Temporand& t) const;

  bool operator<(const Temporand& t) const;

  // friend function, for simpler debuging
  friend std::ostream& operator<<(std::ostream& os, const Temporand& t);

  // Returns the id
  int id() const;

  // Returns the type
  TemporandType type() const;

  // Returs if is operand
  bool is_operand() const;

  // Returs if is temporary
  bool is_temporary() const;

  // Returs if is register
  bool is_register() const;

  // Static member for instantiating a p temporand (operand)
  static Temporand p(int id);

  // Static member for instantiating a p temporand (temporary)
  static Temporand t(int id);

  // Static member for instantiating a p temporand (register)
  static Temporand r(int id);

  // Static member for instantiating a n temporand (no-type)
  static Temporand n(int id);   // Id has no relevant type, just a number
};

#endif
