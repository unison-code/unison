/*
 *  Main authors:
 *    Mats Carlsson <mats.carlsson@ri.se>
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


#include "util.hpp"

bool overlap(int x, int xw, int y, int yw) {
  return (x >= 0) && (y >= 0) && (x + xw > y) && (y + yw > x);
}

vector<register_atom> extend(register_atom ra, int w) {
  vector<register_atom> atoms;
  for (int era = max(0, ra - w + 1); era <= ra; era++) atoms.push_back(era);
  return atoms;
}

bool all_assigned(const IntVarArgs& x) {
  for (int i = 0; i < x.size(); i++) if (!x.assigned()) return false;
  return true;
}

bool holds(const BoolVar x) {
  return x.assigned() && x.val();
}

string show(const int i) {
  stringstream s;
  s << i;
  return s.str();
}

string show(const string s) {
  return s;
}

string show(const Temporand t) {
  stringstream s;
  s << t;
  return s.str();
}

string show(const UnisonConstraintExpr e) {
  return show(vector<string>{show((int)e.id), show(e.data), show(e.children)}, ", ");
}

string show(const PresolverBefore b) {
  return show(vector<string>{show(b.p), show(b.q), show(b.d)}, ", ");
}

string show(const PresolverBeforeJSON b) {
  return show(vector<string>{show(b.p), show(b.q), show(b.e)}, ", ");
}

string show(const PresolverDominates d) {
  return show(vector<string>{show(d.o1), show(d.o2), show(d.ins),
        show(d.temps)}, ", ");
}

string show(const PresolverInstrCond ic) {
  return show(vector<string>{show(ic.o), show(ic.i), show(ic.q)}, ", ");
}

string show(const PresolverValuePrecedeChain pvc) {
  return show(vector<string>{show(pvc.ts), show(pvc.rss)}, ", ");
}

string show(const PresolverInsnClass t) {
  return show(vector<string>{show(t.insn), show(t.rclass)}, ", ");
}

string show(const PresolverInsn2Class2 t) {
  return show(vector<string>{show(t.insn1), show(t.insn2), show(t.class1),
        show(t.class2)}, ", ");
}

string show(const PresolverActiveTable t) {
  return show(vector<string>{show(t.os), show(t.tuples)}, ", ");
}

string show(const PresolverCopyTmpTable t) {
  return show(vector<string>{show(t.os), show(t.ps), show(t.tuples)}, ", ");
}

string show(const PresolverPred p) {
  return show(vector<string>{show(p.p), show(p.q), show(p.d)}, ", ");
}

string show(const PresolverSucc p) {
  return show(vector<string>{show(p.p), show(p.q), show(p.d)}, ", ");
}

string show(const PresolverAcrossJSON x) {
  return show(vector<string>{show(x.o), show(x.ras), show(x.as)}, ", ");
}

string show(const PresolverAcrossTuple x) {
  return show(vector<string>{show(x.o), show(x.t), show(x.d)}, ", ");
}

string show(const PresolverAcrossItemJSON x) {
  return show(vector<string>{show(x.t), show(x.e)}, ", ");
}

string show(const PresolverSetAcross x) {
  return show(vector<string>{show(x.o), show(x.ras), show(x.tsets)}, ", ");
}

string show(const PresolverPrecedence x) {
  return show(vector<string>{show(x.i), show(x.j), show(x.n), show(x.d)}, ", ");
}

string show(const PrecedenceEdge x) {
  return show(vector<string>{show(x.i), show(x.j), show(x.n)}, ", ");
}

string emit_json(const int i) {
  stringstream s;
  s << i;
  return s.str();
}

string emit_json(const bool b) {
  stringstream s;
  s << (b ? "true" : "false");
  return s.str();
}

string emit_json(const double d) {
  stringstream s;
  s.precision(std::numeric_limits<double>::max_digits10);
  s << fixed << d;
  return s.str();
}

string emit_json(const string s) {
  stringstream ss;
  ss << "\"" << s << "\"";
  return ss.str();
}

string emit_json(const PresolverActiveTable x) {
  return show(vector<string>{emit_json(x.os), emit_json(x.tuples)}, ", ");
}

string emit_json(const PresolverCopyTmpTable x) {
  return show(vector<string>{emit_json(x.os), emit_json(x.ps),
        emit_json(x.tuples)}, ", ");
}

string emit_json(const PresolverBeforeJSON x) {
  return show(vector<string>{emit_json(x.p), emit_json(x.q),
        emit_json(x.e)}, ", ");
}

string emit_json(const PresolverAcrossJSON x) {
  return show(vector<string>{emit_json(x.o), emit_json(x.ras),
        emit_json(x.as)}, ", ");
}

string emit_json(const PresolverAcrossItemJSON x) {
  return show(vector<string>{emit_json(x.t), emit_json(x.e)}, ", ");
}

string emit_json(const PresolverSetAcross x) {
  return show(vector<string>{emit_json(x.o), emit_json(x.ras),
        emit_json(x.tsets)}, ", ");
}

string emit_json(const PresolverDominates x) {
  return show(vector<string>{emit_json(x.o1), emit_json(x.o2),
        emit_json(x.ins), emit_json(x.temps)}, ", ");
}

string emit_json(const PresolverPred x) {
  return show(vector<string>{emit_json(x.p), emit_json(x.q),
        emit_json(x.d)}, ", ");
}

string emit_json(const PresolverSucc x) {
  return show(vector<string>{emit_json(x.p), emit_json(x.q),
        emit_json(x.d)}, ", ");
}

string emit_json(const PresolverInstrCond x) {
  return show(vector<string>{emit_json(x.o), emit_json(x.i),
        emit_json(x.q)}, ", ");
}

string emit_json(const PresolverValuePrecedeChain pvc) {
  return show(vector<string>{emit_json(pvc.ts), emit_json(pvc.rss)}, ", ");
}

string emit_json(const UnisonConstraintExpr e) {
  vector<string> elements;
  stringstream ids;
  ids << e.id;
  elements.push_back(ids.str());
  for (int d : e.data)
    elements.push_back(show(d));
  for (UnisonConstraintExpr e0 : e.children)
    elements.push_back(emit_json(e0));
  return show(elements, ", ");
}

string show_class(register_class rc, const Parameters * p) {
  stringstream s;
  s << "rc" << rc << ":" << p->classname[rc];
  return s.str();
}

string show_space(register_space rs, const Parameters * p) {
  stringstream s;
  s << "rs" << rs << ":" << p->spacename[rs];
  return s.str();
}

string show_register(register_atom ra, int w, const Parameters * p) {
  string raname;
  if (ra == NULL_REGISTER) raname = "null";
  else {
    string fa = p->atomname[ra];
    string la = p->atomname[ra + w - 1];
    raname = (fa == la ? fa : fa + "-" + la);
  }
  return raname;
}

string show_instruction(instruction i, operation o, const Parameters * p) {
  if (i == NULL_INSTRUCTION) return p->insname[i];
  switch (p->type[o]) {
  case IN:      return "(in)";
  case OUT:     return "(out)";
  case KILL:    return "(kill)";
  case DEFINE:  return "(define)";
  case COMBINE: return "(combine)";
  case LOW:     return "(low)";
  case HIGH:    return "(high)";
  case SPLIT2:  return "(split2)";
  case SPLIT4:  return "(split4)";
  default:      return p->insname[i];
  }
}

string showInstructions(operation o, const IntVar& i, const Parameters * p) {
  vector<string> ops;
  for (IntVarValues ii(i); ii(); ++ii)
    ops.push_back(show_instruction(p->instructions[o][ii.val()], o, p));
  return showDomain(ops);
}

int gcd(int a, int b) {
  int temp;
  while( b != 0) {
    temp = a % b;
    a = b;
    b = temp;
  }
  return a;
}

void copy_domain(Home h, IntVar s, IntVar d) {
  Int::ViewRanges<Int::IntView> sr(s);
  Int::IntView dv(d);
  dv.narrow_r(h, sr);
}

void copy_domain(Home h, BoolVar s, BoolVar d) {
  Int::ViewRanges<Int::BoolView> sr(s);
  Int::BoolView dv(d);
  dv.narrow_r(h, sr);
}

string init(string s) {
  return s.substr(0, s.size() - 1);
}

bool in_block(PresolverActiveTable & ct, block b, const Parameters * input) {
  for (operation o : ct.os) if (input->oblock[o] != b) return false;
  return true;
}

bool in_block(PresolverCopyTmpTable & ctt, block b, const Parameters * input) {
  for (operation o : ctt.os) if (input->oblock[o] != b) return false;
  for (operand p : ctt.ps) if (input->pb[p] != b) return false;
  return true;
}

bool in_block(PresolverBeforeJSON & bf, block b, const Parameters * input) {
  if (input->pb[bf.p] != b) return false;
  if (input->pb[bf.q] != b) return false;
  return in_block(bf.e, b, input);
}

bool in_block(UnisonConstraintExpr & e, block b, const Parameters * input) {
  switch (e.id) {
  case OR_EXPR:
  case AND_EXPR:
  case XOR_EXPR:
  case IMPLIES_EXPR:
  case NOT_EXPR:
    for (UnisonConstraintExpr e0 : e.children)
      if (!in_block(e0, b, input)) return false;
    return true;
  case ACTIVE_EXPR:
  case IMPLEMENTS_EXPR:
  case DISTANCE_EXPR:
    return (input->oblock[e.data[0]] == b);
  case CONNECTS_EXPR:
  case SHARE_EXPR:
  case OPERAND_OVERLAP_EXPR:
  case ALLOCATED_EXPR:
    return (input->pb[e.data[0]] == b);
  case TEMPORARY_OVERLAP_EXPR:
  case CALLER_SAVED_EXPR:
    return (input->tb[e.data[0]] == b);
  case ALIGNED_EXPR:
    return (input->pb[e.data[0]] == b);
  }
  GECODE_NEVER;
  return true;
}

vector<int> var_vector(const IntVarArgs & v) {
  vector<int> x;
  for (int i = 0; i < v.size(); i++) {
    x.push_back(v[i].val());
  }
  return x;
}
