/*
 *  Main authors:
 *    Mats Carlsson <matsc@sics.se>
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

string show(const PresolverBefore b) {
  stringstream s;
  s << "[" << b.p
    << ", " << b.q
    << ", " << show(b.d) << "]";
  return s.str();
}

string show(const PresolverDominates d) {
  stringstream s;
  s << "[" << d.o1
    << ", " << d.o2
    << ", " << show(d.ins)
    << ", " << show(d.temps) << "]";
  return s.str();
}

string show(const PresolverInstrCond ic) {
  stringstream s;
  s << "[" << ic.o
    << ", " << ic.i
    << ", " << ic.q << "]";
  return s.str();
}

string show(const PresolverValuePrecedeChain pvc) {
  stringstream s;
  s << "[" << show(pvc.ts)
    << ", " << show(pvc.rss) << "]";
  return s.str();
}

string show(const PresolverInsnClass t) {
  stringstream s;
  s << "[" << show(t.insn)
    << ", " << show(t.rclass) << "]";
  return s.str();
}

string show(const PresolverInsn2Class2 t) {
  stringstream s;
  s << "[" << show(t.insn1)
    << ", " << show(t.insn2)
    << ", " << show(t.class1)
    << ", " << show(t.class2)
    << "]";
  return s.str();
}

string show(const PresolverActiveTable t) {
  stringstream s;
  s << "[" << show(t.os) << ", " << show(t.tuples) << "]";
  return s.str();
}

string show(const PresolverCopyTmpTable t) {
  stringstream s;
  s << "[" << show(t.os) << ", " << show(t.ps) << ", " << show(t.tuples) << "]";
  return s.str();
}

string show(const PresolverPred p) {
  stringstream s;
  s << "[" << show(p.p) << ", " << p.q << ", " << p.d << "]";
  return s.str();
}

string show(const PresolverSucc p) {
  stringstream s;
  s << "[" << p.p << ", " << show(p.q) << ", " << p.d << "]";
  return s.str();
}

string show(const PresolverAcross x) {
  stringstream s;
  s << "[" << x.o << ", " << show(x.ras) << ", " << show(x.as) << "]";
  return s.str();
}

string show(const PresolverAcrossTuple x) {
  stringstream s;
  s << "[" << x.o << ", " << x.t << ", " << show(x.d) << "]";
  return s.str();
}

string show(const PresolverAcrossItem x) {
  stringstream s;
  s << "[" << x.t << ", " << show(x.d) << "]";
  return s.str();
}

string show(const PresolverSetAcross x) {
  stringstream s;
  s << "[" << x.o << ", " << show(x.ras) << ", " << show(x.tsets) << "]";
  return s.str();
}

string show(const Temporand t) {
  stringstream s;
  s << t;
  return s.str();
}

string emit_json_object(const int i) {
  stringstream s;
  s << i;
  return s.str();
}

string emit_json_object(const bool b) {
  stringstream s;
  s << (b ? "true" : "false");
  return s.str();
}

string emit_json_object(const string s) {
  stringstream ss;
  ss << "\"" << s << "\"";
  return ss.str();
}

string emit_json_object(const PresolverActiveTable at) {
  stringstream ss;
  ss << "["
     << emit_json_object(at.os)
     << ", "
     << emit_json_object(at.tuples)
     << "]";
  return ss.str();
}

string emit_json_object(const PresolverCopyTmpTable at) {
  stringstream ss;
  ss << "["
     << emit_json_object(at.os)
     << ", "
     << emit_json_object(at.ps)
     << ", "
     << emit_json_object(at.tuples)
     << "]";
  return ss.str();
}

string emit_json_object(const PresolverPrecedence at) {
  stringstream ss;
  ss << "["
     << emit_json_object(at.i)
     << ", "
     << emit_json_object(at.j)
     << ", "
     << emit_json_object(at.n)
     << ", "
     << emit_json_object(at.d)
     << "]";
  return ss.str();
}

string emit_json_object(const PresolverBefore at) {
  stringstream ss;
  ss << "["
     << emit_json_object(at.p)
     << ", "
     << emit_json_object(at.q)
     << ", "
     << emit_json_object(at.d)
     << "]";
  return ss.str();
}

string emit_json_object(const PresolverAcross at) {
  stringstream ss;
  ss << "["
     << emit_json_object(at.o)
     << ", "
     << emit_json_object(at.ras)
     << ", "
     << emit_json_object(at.as)
     << "]";
  return ss.str();
}

string emit_json_object(const PresolverAcrossItem at) {
  stringstream ss;
  ss << "["
     << emit_json_object(at.t)
     << ", "
     << emit_json_object(at.d)
     << "]";
  return ss.str();
}

string emit_json_object(const PresolverSetAcross at) {
  stringstream ss;
  ss << "["
     << emit_json_object(at.o)
     << ", "
     << emit_json_object(at.ras)
     << ", "
     << emit_json_object(at.tsets)
     << "]";
  return ss.str();
}

string emit_json_object(const PresolverDominates at) {
  stringstream ss;
  ss << "["
     << emit_json_object(at.o1)
     << ", "
     << emit_json_object(at.o2)
     << ", "
     << emit_json_object(at.ins)
     << ", "
     << emit_json_object(at.temps)
     << "]";
  return ss.str();
}

string emit_json_object(const PresolverPred at) {
  stringstream ss;
  ss << "["
     << emit_json_object(at.p)
     << ", "
     << emit_json_object(at.q)
     << ", "
     << emit_json_object(at.d)
     << "]";
  return ss.str();
}

string emit_json_object(const PresolverSucc at) {
  stringstream ss;
  ss << "["
     << emit_json_object(at.p)
     << ", "
     << emit_json_object(at.q)
     << ", "
     << emit_json_object(at.d)
     << "]";
  return ss.str();
}

string emit_json_object(const PresolverInstrCond at) {
  stringstream ss;
  ss << "["
     << emit_json_object(at.o)
     << ", "
     << emit_json_object(at.i)
     << ", "
     << emit_json_object(at.q)
     << "]";
  return ss.str();
}

string emit_json_object(const PresolverValuePrecedeChain pvc) {
  stringstream ss;
  ss << "["
     << emit_json_object(pvc.ts)
     << ", "
     << emit_json_object(pvc.rss)
     << "]";
  return ss.str();
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

string show(const PresolverPrecedence& p) {
  stringstream s;
  s << "[" << p.i
    << ", " << p.j
    << ", " << p.n
    << ", " << show(p.d) << "]";
  return s.str();
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

bool in_block(presolver_disj & d, block b, const Parameters * input) {
  for (presolver_conj c : d) if (!in_block(c, b, input)) return false;
  return true;
}

bool in_block(presolver_conj & c, block b, const Parameters * input) {
  for (presolver_lit l : c) if (!in_block(l, b, input)) return false;
  return true;
}

bool in_block(presolver_lit & l, block b, const Parameters * input) {
  if (l[0] == PRESOLVER_EQUAL_TEMPORARIES) {
    operand p = l[1];
    operand q = l[2];
    return (input->pb[p] == b) && (input->pb[q] == b);
  } else if (l[0] == PRESOLVER_OPERAND_TEMPORARY) {
    operand p = l[1];
    temporary t = l[2];
    return (input->pb[p] == b) && (input->tb[t] == b);
  } else if (l[0] == PRESOLVER_ACTIVENESS) {
    operation o = l[1];
    return (input->oblock[o] == b);
  } else if (l[0] == PRESOLVER_OPERATION) {
    operation o = l[1];
    return (input->oblock[o] == b);
  } else if (l[0] == PRESOLVER_OVERLAPPING_OPERANDS) {
    operand p = l[1];
    operand q = l[2];
    return (input->pb[p] == b) && (input->pb[q] == b);
  } else if (l[0] == PRESOLVER_OVERLAPPING_TEMPORARIES) {
    temporary t = l[1];
    temporary u = l[2];
    return (input->tb[t] == b) && (input->tb[u] == b);
  } else if (l[0] == PRESOLVER_CALLER_SAVED_TEMPORARY) {
    temporary t = l[1];
    return (input->tb[t] == b);
  } else if (l[0] == PRESOLVER_NO_OPERATION) {
    operation o = l[1];
    return (input->oblock[o] == b);
  } else if (l[0] == PRESOLVER_OPERAND_CLASS) {
    operand p = l[1];
    return (input->pb[p] == b);
  } else {
    GECODE_NEVER;
  }
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

bool in_block(PresolverPrecedence & p, block b, const Parameters * input) {
  if (input->oblock[p.i] != b) return false;
  if (input->oblock[p.j] != b) return false;
  return in_block(p.d, b, input);
}

bool in_block(PresolverBefore & bf, block b, const Parameters * input) {
  if (input->pb[bf.p] != b) return false;
  if (input->pb[bf.q] != b) return false;
  return in_block(bf.d, b, input);
}
