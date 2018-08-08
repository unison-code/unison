/*
 *  Main authors:
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
 *    Mats Carlsson <mats.carlsson@ri.se>
 *
 *  Contributing authors:
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


#include <iostream>
#include <fstream>
#include <cstring>
#include <string>
#include <cmath>

#ifdef GRAPHICS
#include <QtGui>
#include <QtScript/QScriptEngine>
#endif

#include <gecode/search.hh>
#include <gecode/driver.hh>

#ifndef GRAPHICS
#include "third-party/jsoncpp/json/value.h"
#include "third-party/jsoncpp/json/reader.h"
#endif

#include "common/definitions.hpp"
#include "models/parameters.hpp"
#include "presolver/presolver-options.hpp"
#include "presolver/presolve.hpp"

using namespace Gecode;
using namespace std;

string write_json_vector_int(string label, vector<int> v)
{
  stringstream ss;
  string sep = "[";

  ss << "\"";
  ss << label;
  ss << "\": ";
  for (int x : v) {
    ss << sep; ss << x; sep = ",";
  }
  ss << "]\n";
  return ss.str();
}

string produce_json(Parameters &input, int presolver_time)
{
  stringstream json;
  json << "{"
       << emit_json_line("B", input.B)
       << emit_json_line("O", input.O)
       << emit_json_line("P", input.P)
       << emit_json_line("T", input.T)
       << emit_json_line("block", input.oblock)
       << emit_json_line("operands", input.operands)
       << emit_json_line("temps", input.temps)
       << emit_json_line("use", input.use)
       << emit_json_line("adjacent", input.adjacent)
       << emit_json_line("preassign", input.preassign)
       << emit_json_line("width", input.width)
       << emit_json_line("freq", input.freq)
       << emit_json_line("minlive", input.minlive)
       << emit_json_line("dep", input.dep)
       << emit_json_line("preschedule", input.prescheduled)
       << emit_json_line("I", input.I)
       << emit_json_line("R", input.R)
       << emit_json_line("dist", input.dist)
       << emit_json_line("class", input.rclass)
       << emit_json_line("atoms", input.atoms)
       << emit_json_line("instructions", input.instructions)
       << emit_json_line("lat", input.lat)
       << emit_json_line("bypass", input.bypass)
       << emit_json_line("cap", input.cap)
       << emit_json_line("con", input.con)
       << emit_json_line("dur", input.dur)
       << emit_json_line("off", input.off)
       << emit_json_line("aligned", input.aligned)
       << emit_json_line("adist", input.adist)
       << emit_json_line("packed", input.packed)
       << emit_json_line("exrelated", input.exrelated)
       << emit_json_line("table", input.table)
       << emit_json_line("activators", input.activators)
       << emit_json_line("E", input.E)
       << emit_json_line("optimize_dynamic", input.optimize_dynamic)
       << emit_json_line("optimize_resource", input.optimize_resource)
       << emit_json_line("maxf", input.maxf)
       << emit_json_line("tmp", input.tmp)
       << emit_json_line("definer", input.definer)
       << emit_json_line("operation", input.oper)
       << emit_json_line("def_opr", input.def_opr)
       << emit_json_line("congr", input.congr)
       << emit_json_line("ops", input.ops)
       << emit_json_line("RS", input.RS)
       << emit_json_line("maxc", input.maxc)
       << emit_json_line("type", input.type)
       << emit_json_line("copyrel", input.copyrel)
       << emit_json_line("interchangeable", input.interchangeable)
       << emit_json_line("cfg", input.cfg)
       << emit_json_line("clusters", input.clusters)
       << emit_json_line("space", input.space)
       << emit_json_line("range", input.range)
       << emit_json_line("home", input.home)
       << emit_json_line("infinite", input.infinite)
       << emit_json_line("bounded", input.bounded)
       << emit_json_line("insname", input.insname)
       << emit_json_line("atomname", input.atomname)
       << emit_json_line("classname", input.classname)
       << emit_json_line("spacename", input.spacename)
       << emit_json_line("callersaved", input.callersaved)
       << emit_json_line("calleesaved", input.calleesaved)
       << emit_json_line("optional_min", input.optional_min)
       << emit_json_line("active_tables", input.active_tables)
       << emit_json_line("tmp_tables", input.tmp_tables)
       << emit_json_line("nogoods", input.nogoods)
       << emit_json_line("nogoods2", input.nogoods2)
       << emit_json_line("precedences", input.precedences)
       << emit_json_line("precedences2", input.precedences2)
       << emit_json_line("before", input.before)
       << emit_json_line("before2", input.before2)
       << emit_json_line("across", input.across)
       << emit_json_line("set_across", input.set_across)
       << emit_json_line("domops", input.domops)
       << emit_json_line("last_use", input.last_use)
       << emit_json_line("infassign", input.infassign)
       << emit_json_line("domuses", input.domuses)
       << emit_json_line("precs", input.precs)
       << emit_json_line("assignhints", input.assignhints)
       << emit_json_line("dominates", input.dominates)
       << emit_json_line("difftemps", input.difftemps)
       << emit_json_line("diffregs", input.diffregs)
       << emit_json_line("calleesaved_spill", input.calleesaved_spill)
       << emit_json_line("strictly_congr", input.strictly_congr)
       << emit_json_line("predecessors", input.predecessors)
       << emit_json_line("successors", input.successors)
       << emit_json_line("value_precede_chains", input.value_precede_chains)
       << emit_json_line("quasi_adjacent", input.quasi_adjacent)
       << emit_json_line("long_latency_index", input.long_latency_index)
       << emit_json_line("long_latency_def_use", input.long_latency_def_use)
       << emit_json_line("subsumed_resources", input.subsumed_resources)
       << emit_json_line("temp_domain", input.temp_domain)
       << emit_json_line("freq_scale", input.freq_scale)
       << emit_json_line_last("presolver_time", presolver_time)
       << "}\n";
  return json.str();
}

/*** .dzn generation ***/

#define ZEROBASED1 1
#define ZEROBASED2 2
#define MINUSONEBASED1 4

class FDSet {
public:
  FDSet() : vec({}) {}
  FDSet(vector<int> vec0) : vec(vec0) {sort(vec.begin(), vec.end());}
  FDSet(vector<int> vec0, int inc) : vec(vec0) {sort(vec.begin(), vec.end());
                                                for (unsigned int ii=0; ii<vec.size(); ii++) vec[ii]+=inc;}
  FDSet(int a, int b) : vec({}) {for (int x=a; x<=b; x++) vec.push_back(x);}
  vector<int> vec;
  bool operator<(const FDSet& s) const {
    return (vec < s.vec);
  }
  bool operator==(const FDSet& s) const {
    return (vec == s.vec);
  }
  bool operator!=(const FDSet& s) const {
    return (vec != s.vec);
  }
  friend ostream& operator<<(ostream& os, const FDSet& fd);
};

ostream& operator<<(ostream& os, const FDSet& fd) {
  if (fd.vec.empty()) {
    os << "{}";
  } else {
    int n = -1;
    int a=0, b=0;
    for (int x : fd.vec) {
      if (n == -1) {
	a = b = x;
	n = 0;
      } else if (x == b+1) {
	b++;
      } else {
	if (n++)
	  os << " union ";
	os << a << ".." << b;
	a = b = x;
      }
    }
    if (n > 0)
      os << " union ";
    if (n >= 0)
      os << a << ".." << b;
  }
  return os;
}

class Quad {
public:
  Quad() : id(AND_EXPR), arg1(0), arg2(0), arg3(0), children({}) {}
  UnisonConstraintExprId id;
  int arg1, arg2, arg3;
  vector<int> children;
  bool operator<(const Quad & e) const {
    if(id != e.id) return id < e.id;
    else if(arg1 != e.arg1) return arg1 < e.arg1;
    else if(arg2 != e.arg2) return arg2 < e.arg2;
    else if(arg3 != e.arg3) return arg3 < e.arg3;
    else return children < e.children;
  }
  bool operator==(const Quad& e) const {
    return (id == e.id) && (arg1 == e.arg1) && (arg2 == e.arg2) && (arg3 == e.arg3) && (children == e.children);
  }
  bool operator!=(const Quad& e) const {
    return (id != e.id) || (arg1 != e.arg1) || (arg2 != e.arg2) || (arg3 != e.arg3) || (children != e.children);
  }
};

void toFDSet(vector<vector<int>>& xs, vector<FDSet>& ys) {
  for (vector<int>& x : xs)
    ys.push_back(FDSet(x));
}

void toFDSetInc(vector<vector<int>>& xs, vector<FDSet>& ys) {
  for (vector<int>& x : xs)
    ys.push_back(FDSet(x, 1));
}

int dznEncodeQuad(Quad& quad, map<Quad,int>& quadMap, vector<Quad>& quadList) {
  if (quadMap.count(quad)) {
    return quadMap[quad];
  } else {
    int i = quadList.size();
    quadList.push_back(quad);
    quadMap[quad] = i+1;
    return i+1;
  }
}

int dznEncodeFDSet(FDSet& set, map<FDSet,int>& setMap, vector<FDSet>& setList) {
  if (setMap.count(set)) {
    return setMap[set];
  } else {
    int i = setList.size();
    setList.push_back(set);
    setMap[set] = i+1;
    return i+1;
  }
}

Quad dznQuad(UnisonConstraintExpr& e, map<Quad,int>& quadMap, vector<Quad>& quadList) {
  switch (e.id) {
  case OR_EXPR:
  case AND_EXPR:
    {
      vector<Quad> quads;
      vector<int> encoded;
      for (UnisonConstraintExpr& a : e.children) {
	Quad quad = dznQuad(a, quadMap, quadList);
	quads.push_back(quad);
	encoded.push_back(dznEncodeQuad(quad, quadMap, quadList));
      }
      if (quads.size()==1) {
	return quads[0];
      } else {
	Quad quad;
	quad.id = e.id;
	sort(encoded.begin(), encoded.end());
	quad.children.insert(quad.children.end(), encoded.begin(), encoded.end());
	return quad;
      }
    }
  case XOR_EXPR:
    {
      vector<Quad> quads;
      vector<int> encoded;
      for (UnisonConstraintExpr& a : e.children) {
	Quad quad = dznQuad(a, quadMap, quadList);
	quads.push_back(quad);
	encoded.push_back(dznEncodeQuad(quad, quadMap, quadList));
      }
      Quad quad;
      quad.id = e.id;
      sort(encoded.begin(), encoded.end());
      quad.arg1 = encoded[0];
      quad.arg2 = encoded[1];
      return quad;
    }
  case IMPLIES_EXPR:
    if (e.children[0].id==OR_EXPR) {
      UnisonConstraintExpr y = e.children[1];
      UnisonConstraintExpr f;
      f.id = AND_EXPR;
      for (UnisonConstraintExpr& a : e.children[0].children) {
	UnisonConstraintExpr conjunct;
	conjunct.id = e.id;
	conjunct.children.push_back(a);
	conjunct.children.push_back(y);
	f.children.push_back(conjunct);
      }
      return dznQuad(f, quadMap, quadList);
    } else if (e.children[0].id==AND_EXPR) {
      UnisonConstraintExpr y = e.children[1];
      UnisonConstraintExpr f;
      f.id = OR_EXPR;
      f.children.push_back(y);
      for (UnisonConstraintExpr& a : e.children[0].children) {
	UnisonConstraintExpr disjunct;
	if (a.id == NOT_EXPR) {
	  disjunct = a.children[0];
	} else {
	  disjunct.id = NOT_EXPR;
	  disjunct.children.push_back(a);
	}
	f.children.push_back(disjunct);
      }
      return dznQuad(f, quadMap, quadList);
    } else {
      vector<Quad> quads;
      vector<int> encoded;
      for (UnisonConstraintExpr& a : e.children) {
	Quad quad = dznQuad(a, quadMap, quadList);
	quads.push_back(quad);
	encoded.push_back(dznEncodeQuad(quad, quadMap, quadList));
      }
      Quad quad;
      quad.id = e.id;
      quad.arg1 = encoded[0];
      quad.arg2 = encoded[1];
      return quad;
    }
  case NOT_EXPR:
    {
      Quad q = dznQuad(e.children[0], quadMap, quadList);
      int enc = dznEncodeQuad(q, quadMap, quadList);
      Quad quad;
      quad.id = e.id;
      quad.arg1 = enc;
      return quad;
    }
  default:
    {
      Quad quad;
      quad.id = e.id;
      quad.arg1 = e.data[0];
      if (e.data.size() > 1)
	quad.arg2 = e.data[1];
      if (e.data.size() > 2)
	quad.arg3 = e.data[2];
      return quad;
    }
  }
}

int dznEncode(UnisonConstraintExpr& e, map<Quad,int>& quadMap, vector<Quad>& quadList) {
  Quad quad = dznQuad(e, quadMap, quadList);
  return dznEncodeQuad(quad, quadMap, quadList);
}

void computeWCET(Parameters &input, vector<vector<int>>& wcet) {
  for (int o : input.O) {
    if (input.type[o] == OUT) {
      wcet.push_back({o, 0});
    } else {
      block b = input.oblock[o];
      int maxminlive = 0;
      
      for (temporary t : input.tmp[b])
	if (input.def_opr[t] == o && input.minlive[t] > maxminlive)
	  maxminlive = input.minlive[t];
      for (unsigned int ii = 0; ii < input.instructions[o].size(); ii++) {
	instruction i = input.instructions[o][ii];
	vector<int> opdurs;
	int maxdeflat = 0;
	int maxduroff = 0;

	for (resource r : input.R)
	  if (input.con[i][r]) {
	    int duroff = input.dur[i][r] + input.off[i][r];
	    if (maxduroff < duroff)
	      maxduroff = duroff;
	  }
	opdurs.push_back(maxduroff);

	for (unsigned pi = 0; pi < input.operands[o].size(); pi++) {
	  operand p = input.operands[o][pi];
	  if (!input.use[p]) {
	    int l1 = input.lat[o][ii][pi];
	    for (operand q : input.users[input.single_temp[p]]) {
	      operation o2 = input.oper[q];
	      for (unsigned qi = 0; qi < input.operands[o2].size(); qi++) {
		if (input.operands[o2][qi]==q)
		  for (unsigned int jj = 0; jj < input.instructions[o2].size(); jj++) {
		    int l2 = input.lat[o2][jj][qi];
		    if (maxdeflat < l1+l2)
		      maxdeflat = l1+l2;
		  }
	      }
	    }
	  }	    
	}
	opdurs.push_back(maxdeflat);

	int maxdist = 0;
	for (unsigned int e = 0; e < input.dep[b].size(); e++)
	  if ((o == input.dep[b][e][0]) && (input.dist[b][e][ii] > maxdist))
	    maxdist = input.dist[b][e][ii];
	opdurs.push_back(maxdist);
	opdurs.push_back(maxminlive);
	wcet.push_back({o, max_of(opdurs)});
      }
    }
  }
}

string emit_dzn(const bool b, int opt) {
  (void)opt;
  stringstream s;
  s << (b ? "true" : "false");
  return s.str();
}

string emit_dzn(const int i, int opt) {
  (void)opt;
  stringstream s;
  s << i;
  return s.str();
}

string emit_dzn(const FDSet fd, int opt) {
  (void)opt;
  stringstream s;
  s << fd;
  return s.str();
}

string emit_dzn(const vector<bool> xs, int opt) {
  stringstream s;
  int beg1 = 1;
  int dim1 = xs.size();
  if (opt & ZEROBASED1)
    beg1 = 0;
  else if (opt & MINUSONEBASED1)
    beg1 = -1;
  if (beg1 != 1)
    s << "array1d(" << beg1 << ".." << dim1+beg1-1 << ", ";
  s << "[";
  int r = 0;
  for (bool x : xs) {
    if (r++)
      s << ", ";
    s << (x ? "true" : "false");
  }
  s << "]";
  if (beg1 != 1)
    s << ")";
  return s.str();
}

string emit_dzn(const vector<int> xs, int opt) {
  stringstream s;
  int beg1 = 1;
  int dim1 = xs.size();
  if (opt & ZEROBASED1)
    beg1 = 0;
  else if (opt & MINUSONEBASED1)
    beg1 = -1;
  if (beg1 != 1)
    s << "array1d(" << beg1 << ".." << dim1+beg1-1 << ", ";
  s << "[";
  int r = 0;
  for (int x : xs) {
    if (r++)
      s << ", ";
    s << x;
  }
  s << "]";
  if (beg1 != 1)
    s << ")";
  return s.str();
}

string emit_dzn(const vector<FDSet> xs, int opt) {
  stringstream s;
  int beg1 = 1;
  int dim1 = xs.size();
  if (opt & ZEROBASED1)
    beg1 = 0;
  else if (opt & MINUSONEBASED1)
    beg1 = -1;
  if (beg1 != 1)
    s << "array1d(" << beg1 << ".." << dim1+beg1-1 << ", ";
  s << "[";
  int r = 0;
  for (FDSet x : xs) {
    if (r++)
      s << ", ";
    s << x;
  }
  s << "]";
  if (beg1 != 1)
    s << ")";
  return s.str();
}

string emit_dzn(const vector<vector<int>> xss, int opt) {
  stringstream s;
  int beg1 = 1;
  int beg2 = 1;
  int dim1 = xss.size();
  int dim2 = dim1==0 ? 0 : xss[0].size();
  if (opt & ZEROBASED1)
    beg1 = 0;
  if (opt & ZEROBASED2)
    beg2 = 0;
  int r = 0;
  s << "array2d(" << beg1 << ".." << dim1+beg1-1 << ", " << beg2 << ".." << dim2+beg2-1 << ", [";
  for (vector<int> xs : xss) {
    for (int x : xs) {
      if (r++)
	s << ", ";
      s << x;
    }
  }
  s << "])";
  return s.str();
}


string emit_dzn(const vector<vector<FDSet>> xss, int opt) {
  stringstream s;
  int beg1 = 1;
  int beg2 = 1;
  int dim1 = xss.size();
  int dim2 = dim1==0 ? 0 : xss[0].size();
  if (opt & ZEROBASED1)
    beg1 = 0;
  if (opt & ZEROBASED2)
    beg2 = 0;
  int r = 0;
  s << "array2d(" << beg1 << ".." << dim1+beg1-1 << ", " << beg2 << ".." << dim2+beg2-1 << ", [";
  for (vector<FDSet> xs : xss) {
    for (FDSet x : xs) {
      if (r++)
	s << ", ";
      s << x;
    }
  }
  s << "])";
  return s.str();
}

template <typename T>
string emit_dzn_line(string e, const T o) {
  return e + " = " + emit_dzn(o, 0) + ";\n";
}

template <typename T>
string emit_dzn_line(string e, const T o, int opt) {
  return e + " = " + emit_dzn(o, opt) + ";\n";
}


bool at_least_one_table(vector<vector<int>> tuples) {
  vector<int> tuple = tuples[0];
  unsigned int n = tuple.size();
  unsigned int m = tuples.size();
  return (tuple.back() && 1U<<n == m+1);
}


string produce_dzn(Parameters &input) {
  map<FDSet,int> setMap;
  map<Quad,int> quadMap;
  map<vector<int>,vector<int>> actMap;
  stringstream dzn;
  vector<FDSet> across_regs, across_items;
  vector<FDSet> activator_insns, activator_ops;
  vector<FDSet> atom_regs;
  vector<FDSet> bb_operands, bb_ops, bb_subsumed, bb_temps;
  vector<FDSet> calleesaved_spill;
  vector<FDSet> congr;
  vector<FDSet> diffreg;
  vector<FDSet> difftemp;
  vector<FDSet> dominate_instructions, dominate_temps;
  vector<FDSet> emptyset;
  vector<FDSet> expr_children;
  vector<FDSet> exrelated_rows;
  vector<FDSet> op_instructions, op_operands;
  vector<FDSet> operand_temps;
  vector<FDSet> predecessors_preds, successors_succs;
  vector<FDSet> range;
  vector<FDSet> related_temps;
  vector<FDSet> relation_ops, relation_range, relation_temps;
  vector<FDSet> setList;
  vector<FDSet> setacross_regs, setacross_tempsets;
  vector<FDSet> strictly_congr;
  vector<FDSet> table_exists_ops;
  vector<FDSet> table_iffall_ops;
  vector<FDSet> temp_domain;
  vector<FDSet> temp_uses;
  vector<FDSet> value_precede_regs;
  vector<Quad> quadList;
  vector<bool> last_use;
  vector<bool> mand;
  vector<int> across_op, across_item_temp, across_item_cond;
  vector<int> bb_order;
  vector<int> before_pred, before_succ, before_cond; 
  vector<int> dominate_ed, dominate_ing;
  vector<int> expr_op, expr_arg1, expr_arg2, expr_arg3;
  vector<int> exrelated_p, exrelated_q;
  vector<int> infinite;
  vector<int> ints;
  vector<int> maxatom;
  vector<int> nogood, precedence, adhoc;
  vector<int> operand_definer;
  vector<int> predecessors_succ, predecessors_lat, successors_pred, successors_lat;
  vector<int> preschedule_op, preschedule_cycle;
  vector<int> relation_ntuples;
  vector<int> setacross_op;
  vector<int> space;
  vector<int> value_precede_min, value_precede_max, value_precede_temps;
  vector<pair<int,int>> len_bb(input.maxc.size());
  vector<vector<FDSet>> domop(2);
  vector<vector<FDSet>> long_latency_index;
  vector<vector<int>> adjacent(2);
  vector<vector<int>> aligned(4);
  vector<vector<int>> bypass_table;
  vector<vector<int>> con_transpose(input.cap.size());
  vector<vector<int>> cs_spill_t(2);
  vector<vector<int>> domuse(3);
  vector<vector<int>> dur_transpose(input.cap.size());
  vector<vector<int>> exrelated_ext;
  vector<vector<int>> flat_operands;
  vector<vector<int>> lat_table;
  vector<vector<int>> long_latency_def_use(2);
  vector<vector<int>> off_transpose(input.cap.size());
  vector<vector<int>> operand_atom;
  vector<vector<int>> preassign(2);
  vector<vector<int>> quasi_adjacent(2);
  vector<vector<int>> wcet;

  for (vector<int>& os : input.ops) {
    vector<int> ps;
    for (int o : os)
      ps.insert(ps.end(), input.operands[o].begin(), input.operands[o].end());
    flat_operands.push_back(ps);
  }
  toFDSet(input.ops, bb_ops);
  toFDSet(flat_operands, bb_operands);
  toFDSet(input.tmp, bb_temps);
  toFDSetInc(input.subsumed_resources, bb_subsumed);
  for (unsigned int i=0; i<input.ops.size(); i++)
    len_bb[i] = make_pair(input.ops[i].front() - input.ops[i].back(), i+1);
  sort(len_bb.begin(), len_bb.end());
  for (pair<int,int>& kb : len_bb)
    bb_order.push_back(kb.second);
  toFDSet(input.operands, op_operands);
  toFDSet(input.instructions, op_instructions);
  for (vector<int>& is : input.instructions)
    mand.push_back(is[0] > 0);
  toFDSet(input.atoms, atom_regs);
  emptyset.push_back(FDSet());
  atom_regs.insert(atom_regs.begin(), emptyset.begin(), emptyset.end());
  for (auto& c : input.congr)
    if (c.size() > 1)
      congr.push_back(FDSet(c));
  for (auto& c : input.strictly_congr)
    if (c.size() > 1)
      strictly_congr.push_back(FDSet(c));
  for (vector<int>& is : input.range)
    range.push_back(FDSet(is[0], is[1]));
  for (vector<vector<int>>& row : input.long_latency_index) {
    vector<FDSet> long_latency_row;
    toFDSet(row, long_latency_row);
    long_latency_index.push_back(long_latency_row);
  }
  toFDSet(input.temps, operand_temps);
  map<int,vector<int>> canonMap;
  set<int> optTemps;
  for (int p : input.P)
    if (!input.use[p] && input.temps[p][0]==-1)
	optTemps.insert(input.temps[p][1]);
  for (int p : input.P) {
    vector<int> ts = input.temps[p];
    int canon = ts[0];
    if (canon>=0 && input.use[p] && !optTemps.count(canon))
      canonMap[canon].insert(canonMap[canon].end(), ts.begin(), ts.end());
    last_use.push_back(ord_contains(input.last_use, p));
  }
  for (auto& kv : canonMap) {
    sort(kv.second.begin(), kv.second.end());
    kv.second.erase(unique(kv.second.begin(), kv.second.end()), kv.second.end());
    if (kv.second.size() > 1)
      related_temps.push_back(FDSet(kv.second));
  }	
  toFDSet(input.users, temp_uses);
  toFDSet(input.difftemps, difftemp);
  toFDSet(input.diffregs, diffreg);
  toFDSet(input.temp_domain, temp_domain);
  for (vector<int>& is : input.calleesaved_spill) {
    cs_spill_t[0].push_back(is[0]);
    cs_spill_t[1].push_back(is[1]);
    calleesaved_spill.push_back(FDSet(is));
  }    
  for (vector<int>& as : input.atoms)
    maxatom.push_back(as.back());
  for (unsigned int i=0; i<input.infinite.size(); i++)
    if (input.infinite[i]) {
      int a = input.range[i][0];
      int b = input.range[i][1];
      for (int j=a; j<=b; j++)
	infinite.push_back(j);
    }
  for (vector<int>& cs : input.con)
    for (unsigned int i=0; i<cs.size(); i++)
      con_transpose[i].push_back(cs[i]);
  for (vector<int>& cs : input.dur)
    for (unsigned int i=0; i<cs.size(); i++)
      dur_transpose[i].push_back(cs[i]);
  for (vector<int>& cs : input.off)
    for (unsigned int i=0; i<cs.size(); i++)
      off_transpose[i].push_back(cs[i]);
  for (vector<int>& pr : input.preassign)
    for (unsigned int i=0; i<2; i++)
      preassign[i].push_back(pr[i]);
  for (vector<int>& pr : input.aligned)
    for (unsigned int i=0; i<4; i++)
      aligned[i].push_back(pr[i]);
  for (vector<int>& pr : input.adjacent)
    for (unsigned int i=0; i<2; i++)
      adjacent[i].push_back(pr[i]);
  for (vector<int>& pr : input.quasi_adjacent)
    for (unsigned int i=0; i<2; i++)
      quasi_adjacent[i].push_back(pr[i]);
  for (vector<int>& pr : input.long_latency_def_use)
    for (unsigned int i=0; i<2; i++)
      long_latency_def_use[i].push_back(pr[i]);
  for (unsigned int o=0; o<input.operands.size(); o++)
    for (int p : input.operands[o]) {
      (void)p;
      operand_definer.push_back(o);
    }
  for (unsigned int o=0; o<input.operands.size(); o++) {
    vector<int> opnds = input.operands[o];
    vector<int> insns = input.instructions[o];
    for (unsigned int j=0; j<opnds.size(); j++) {
      vector<int> row;
      unsigned int ii = 0;
      for (int ins : input.I)
	if (ii < insns.size() && ins == insns[ii])
	  row.push_back(input.rclass[o][ii++][j]);
	else
	  row.push_back(-1);
      operand_atom.push_back(row);
    }
  }
  for (PresolverBeforeJSON& bef : input.before) {
    before_pred.push_back(bef.p);
    before_succ.push_back(bef.q);
    before_cond.push_back(dznEncode(bef.e, quadMap, quadList));
  }
  for (UnisonConstraintExpr& e : input.nogoods)
    nogood.push_back(dznEncode(e, quadMap, quadList));
  for (PresolverAcrossJSON& a : input.across) {
    unsigned int ii = 1;
    across_op.push_back(a.o);
    across_regs.push_back(FDSet(a.ras));
    vector<int> items;
    for (PresolverAcrossItemJSON& aitem : a.as) {
      items.push_back(ii++);
      across_item_temp.push_back(aitem.t);
      across_item_cond.push_back(dznEncode(aitem.e, quadMap, quadList));
    }
    across_items.push_back(FDSet(items));
  }
  for (PresolverSetAcross& sa : input.set_across) {
    setacross_op.push_back(sa.o);
    setacross_regs.push_back(FDSet(sa.ras));
    vector<int> tempsets;
    for (vector<int>& tset : sa.tsets) {
      FDSet fd = FDSet(tset);
      tempsets.push_back(dznEncodeFDSet(fd, setMap, setList));
    }
    setacross_tempsets.push_back(FDSet(tempsets));
  }
  for (UnisonConstraintExpr& e : input.precedences)
    precedence.push_back(dznEncode(e, quadMap, quadList));
  for (UnisonConstraintExpr& e : input.E)
    adhoc.push_back(dznEncode(e, quadMap, quadList));
  for (vector<vector<int>>& pr : input.domops)
    for (unsigned int i=0; i<2; i++)
      domop[i].push_back(FDSet(pr[i]));
  for (vector<int>& pr : input.domuses)
    for (unsigned int i=0; i<3; i++)
      domuse[i].push_back(pr[i]);
  space.push_back(-1);
  space.insert(space.end(), input.space.begin(), input.space.end());
  for (PresolverDominates& pd : input.dominates) {
    dominate_ing.push_back(pd.o1);
    dominate_ed.push_back(pd.o2);
    dominate_instructions.push_back(FDSet(pd.ins));
    dominate_temps.push_back(FDSet(pd.temps));
  }
  for (const PresolverActiveTable& AT : input.active_tables) {
    if (AT.tuples.size() == 2 &&
	AT.tuples[0].size() == 2 &&
	AT.tuples[0][0] == AT.tuples[0][1] &&
	AT.tuples[1][0] == AT.tuples[1][1]) {
      table_iffall_ops.push_back(FDSet(AT.os));
    } else if (at_least_one_table(AT.tuples)) {
      table_exists_ops.push_back(FDSet(AT.os));
    } else {
      relation_ops.push_back(FDSet(AT.os));
      relation_temps.push_back(FDSet());
      relation_ntuples.push_back(AT.tuples.size());
      int a = ints.size();
      for (auto& tuple : AT.tuples)
	ints.insert(ints.end(), tuple.begin(), tuple.end());
      int b = ints.size();
      relation_range.push_back(FDSet(a+1, b));
    }
  }
  for (const PresolverCopyTmpTable& TT : input.tmp_tables) {
    relation_ops.push_back(FDSet(TT.os));
    relation_temps.push_back(FDSet(TT.ps));
    relation_ntuples.push_back(TT.tuples.size());
    int a = ints.size();
    for (auto& tuple : TT.tuples)
      ints.insert(ints.end(), tuple.begin(), tuple.end());
    int b = ints.size();
    relation_range.push_back(FDSet(a+1, b));
  }
  for (unsigned int ii=0; ii<input.activators.size(); ii++)
    if (!input.activators[ii].empty())
      actMap[input.activators[ii]].push_back(ii);
  for (auto& kv : actMap) {
    activator_insns.push_back(FDSet(kv.first));
    activator_ops.push_back(FDSet(kv.second));
  }
  for (PresolverPred& is : input.predecessors) {
    predecessors_preds.push_back(FDSet(is.p));
    predecessors_succ.push_back(is.q);
    predecessors_lat.push_back(is.d);
  }
  for (PresolverSucc& is : input.successors) {
    successors_pred.push_back(is.p);
    successors_succs.push_back(FDSet(is.q));
    successors_lat.push_back(is.d);
  }
  for (PresolverValuePrecedeChain& item : input.value_precede_chains) {
    unsigned int sofar = value_precede_temps.size();
    value_precede_temps.insert(value_precede_temps.end(), item.ts.begin(), item.ts.end());
    for (vector<int>& rs : item.rss) {
      value_precede_min.push_back(sofar+1);
      value_precede_max.push_back(value_precede_temps.size());
      value_precede_regs.push_back(FDSet(rs));
    }
  }
  int minl = 9999;
  int maxl = 0;
  for (int o : input.O)
    for (unsigned int ii = 0; ii < input.instructions[o].size(); ii++)
      for (unsigned int pp = 0; pp < input.operands[o].size(); pp++) {
	int l = input.lat[o][ii][pp];
	lat_table.push_back({o, input.instructions[o][ii], input.operands[o][pp], l});
	minl = min(minl, l);
	maxl = max(maxl, l);
      }
  for (vector<int>& is : input.prescheduled) {
    preschedule_op.push_back(is[0]);
    preschedule_cycle.push_back(is[1]);
  }
  for (vector<int>& is : input.exrelated) {
    exrelated_p.push_back(is[0]);
    exrelated_q.push_back(is[1]);
  }
  for (vector<vector<int>>& item : input.table) {
    unsigned int sofar = exrelated_ext.size();
    exrelated_ext.insert(exrelated_ext.end(), item.begin(), item.end());
    exrelated_rows.push_back(FDSet(sofar+1, exrelated_ext.size()));
  }
  for (int o : input.O)
    for (unsigned int ii = 0; ii < input.instructions[o].size(); ii++)
      for (unsigned int pp = 0; pp < input.operands[o].size(); pp++)
	if (input.bypass[o][ii][pp])
	  bypass_table.push_back({o, input.instructions[o][ii], input.operands[o][pp]});
  computeWCET(input, wcet);
  for (Quad& quad : quadList) {
    expr_op.push_back(quad.id);
    expr_arg1.push_back(quad.arg1);
    expr_arg2.push_back(quad.arg2);
    expr_arg3.push_back(quad.arg3);
    expr_children.push_back(FDSet(quad.children));
  }

  dzn  << emit_dzn_line("MAXF", input.maxf[0])
       << emit_dzn_line("MAXO", input.O.back())
       << emit_dzn_line("MAXP", input.P.back())
       << emit_dzn_line("MAXT", input.T.back())
       << emit_dzn_line("MAXI", input.I.back())
       << emit_dzn_line("MAXC", max_of(input.maxc))
       << emit_dzn_line("MAXR", max_of(maxatom))
       << emit_dzn_line("optimize_cycles", (input.optimize_resource.front() == -1))
       << emit_dzn_line("bb_ops", bb_ops)
       << emit_dzn_line("bb_operands", bb_operands)
       << emit_dzn_line("bb_temps", bb_temps)
       << emit_dzn_line("bb_subsumed", bb_subsumed)
       << emit_dzn_line("bb_frequency", input.freq)
       << emit_dzn_line("bb_maxcycle", input.maxc)
       << emit_dzn_line("bb_optional_min", input.optional_min)
       << emit_dzn_line("bb_order", bb_order)
       << emit_dzn_line("op_operands", op_operands, ZEROBASED1)
       << emit_dzn_line("op_instructions", op_instructions, ZEROBASED1)
       << emit_dzn_line("op_type", input.type, ZEROBASED1)
       << emit_dzn_line("op_mand", mand, ZEROBASED1)
       << emit_dzn_line("atom_regs", atom_regs, MINUSONEBASED1)
       << emit_dzn_line("calleesaved", FDSet(input.calleesaved))
       << emit_dzn_line("callersaved", FDSet(input.callersaved))
       << emit_dzn_line("infinite", FDSet(infinite))
       << emit_dzn_line("range", range)
       << emit_dzn_line("bounded", input.bounded)
       << emit_dzn_line("res_cap", input.cap)
       << emit_dzn_line("res_con", con_transpose, ZEROBASED2)
       << emit_dzn_line("res_dur", dur_transpose, ZEROBASED2)
       << emit_dzn_line("res_off", off_transpose, ZEROBASED2)
       << emit_dzn_line("congr", congr)
       << emit_dzn_line("strictly_congr", strictly_congr)
       << emit_dzn_line("preassign_operand", preassign[0])
       << emit_dzn_line("preassign_reg", preassign[1])
       << emit_dzn_line("aligned_def", aligned[0])
       << emit_dzn_line("aligned_use", aligned[1])
       << emit_dzn_line("aligned_defi", aligned[2])
       << emit_dzn_line("aligned_usei", aligned[3])
       << emit_dzn_line("aligned_dist", input.adist)
       << emit_dzn_line("adj_from", adjacent[0])
       << emit_dzn_line("adj_to", adjacent[1])
       << emit_dzn_line("quasi_adj_from", quasi_adjacent[0])
       << emit_dzn_line("quasi_adj_to", quasi_adjacent[1])
       << emit_dzn_line("long_latency_index", long_latency_index)
       << emit_dzn_line("long_latency_def", long_latency_def_use[0], ZEROBASED1)
       << emit_dzn_line("long_latency_use", long_latency_def_use[1], ZEROBASED1)
       << emit_dzn_line("operand_definer", operand_definer, ZEROBASED1)
       << emit_dzn_line("operand_use", input.use, ZEROBASED1)
       << emit_dzn_line("operand_lastuse", last_use, ZEROBASED1)
       << emit_dzn_line("operand_temps", operand_temps, ZEROBASED1)
       << emit_dzn_line("operand_atom", operand_atom, ZEROBASED1 + ZEROBASED2)
       << emit_dzn_line("related_temps", related_temps)
       << emit_dzn_line("temp_definer", input.def_opr, ZEROBASED1)
       << emit_dzn_line("temp_def", input.definer, ZEROBASED1)
       << emit_dzn_line("temp_width", input.width, ZEROBASED1)
       << emit_dzn_line("temp_minlive", input.minlive, ZEROBASED1)
       << emit_dzn_line("temp_uses", temp_uses, ZEROBASED1)
       << emit_dzn_line("packed_pq", input.packed)
       << emit_dzn_line("before_pred", before_pred)
       << emit_dzn_line("before_succ", before_succ)
       << emit_dzn_line("before_cond", before_cond)
       << emit_dzn_line("nogood", nogood)
       << emit_dzn_line("across_op", across_op)
       << emit_dzn_line("across_regs", across_regs)
       << emit_dzn_line("across_items", across_items)
       << emit_dzn_line("across_item_temp", across_item_temp)
       << emit_dzn_line("across_item_cond", across_item_cond)
       << emit_dzn_line("setacross_op", setacross_op)
       << emit_dzn_line("setacross_regs", setacross_regs)
       << emit_dzn_line("setacross_tempsets", setacross_tempsets)
       << emit_dzn_line("difftemp", difftemp)
       << emit_dzn_line("diffreg", diffreg)
       << emit_dzn_line("domop_operands", domop[0])
       << emit_dzn_line("domop_temps", domop[1])
       << emit_dzn_line("domuse_p", domuse[0])
       << emit_dzn_line("domuse_q", domuse[1])
       << emit_dzn_line("domuse_r", domuse[2])
       << emit_dzn_line("infassign", input.infassign)
       << emit_dzn_line("space", space, MINUSONEBASED1)
       << emit_dzn_line("dominate_ing", dominate_ing)
       << emit_dzn_line("dominate_ed", dominate_ed)
       << emit_dzn_line("dominate_instructions", dominate_instructions)
       << emit_dzn_line("dominate_temps", dominate_temps)
       << emit_dzn_line("precedence", precedence)
       << emit_dzn_line("table_exists_ops", table_exists_ops)
       << emit_dzn_line("table_iffall_ops", table_iffall_ops)
       << emit_dzn_line("relation_ops", relation_ops)
       << emit_dzn_line("relation_temps", relation_temps)
       << emit_dzn_line("relation_ntuples", relation_ntuples)
       << emit_dzn_line("relation_range", relation_range)
       << emit_dzn_line("ints", ints)
       << emit_dzn_line("calleesaved_spill", calleesaved_spill)
       << emit_dzn_line("cs_spill_transpose", cs_spill_t)
       << emit_dzn_line("activator_insns", activator_insns)
       << emit_dzn_line("activator_ops", activator_ops)
       << emit_dzn_line("predecessors_preds", predecessors_preds)
       << emit_dzn_line("predecessors_succ", predecessors_succ)
       << emit_dzn_line("predecessors_lat", predecessors_lat)
       << emit_dzn_line("successors_pred", successors_pred)
       << emit_dzn_line("successors_succs", successors_succs)
       << emit_dzn_line("successors_lat", successors_lat)
       << emit_dzn_line("value_precede_min", value_precede_min)
       << emit_dzn_line("value_precede_max", value_precede_max)
       << emit_dzn_line("value_precede_regs", value_precede_regs)
       << emit_dzn_line("value_precede_temps", value_precede_temps)
       << emit_dzn_line("lat_table", lat_table)
       << emit_dzn_line("MINL", minl)
       << emit_dzn_line("MAXL", maxl)
       << emit_dzn_line("preschedule_op", preschedule_op)
       << emit_dzn_line("preschedule_cycle", preschedule_cycle)
       << emit_dzn_line("exrelated_p", exrelated_p)
       << emit_dzn_line("exrelated_q", exrelated_q)
       << emit_dzn_line("exrelated_rows", exrelated_rows)
       << emit_dzn_line("exrelated_ext", exrelated_ext)
       << emit_dzn_line("bypass_table", bypass_table)
       << emit_dzn_line("adhoc", adhoc)
       << emit_dzn_line("temp_domain", temp_domain, ZEROBASED1)
       << emit_dzn_line("wcet", wcet)
       << emit_dzn_line("expr_op", expr_op)
       << emit_dzn_line("expr_arg1", expr_arg1)
       << emit_dzn_line("expr_arg2", expr_arg2)
       << emit_dzn_line("expr_arg3", expr_arg3)
       << emit_dzn_line("expr_children", expr_children)
       << emit_dzn_line("sets", setList);
  
  return dzn.str();
}

int main(int argc, char* argv[]) {

  int argc0 = argc;
  vector<string> argv0;
  for (int i = 0; i < argc; i++) argv0.push_back(argv[i]);

  PresolverOptions options;
  options.parse(argc, argv);

  if (argc < 2) {
    options.help();
    cout << endl;
    exit(EXIT_FAILURE);
  }

  if (strlen(options.instance()) == 0) {
    cerr << "Null input file, original program arguments:" << endl;
    for (int i = 0; i < argc0; i++) {
      cerr << "arg" << i << ": " << argv0[i] << endl;
    }
    cerr << "Remaining program arguments:" << endl;
    for (int i = 0; i < argc; i++) {
      cerr << "arg" << i << ": " << argv[i] << endl;
    }
    exit(EXIT_FAILURE);
  }

#ifdef GRAPHICS
  QApplication *app = new QApplication(argc, argv, false);
#endif

  string name(options.instance());
  string prefix = name.substr(0,name.find(".json"))
                      .substr(0,name.find(".ext"));
  ifstream fin;
  ofstream fout;
  fin.open(name.c_str(), ios::in);
  if (fin.fail()) {
    cerr << "Failed to open " << name << ": " << strerror(errno) << endl;
#ifdef GRAPHICS
    cerr << "Working directory: "
         << QDir::currentPath().toStdString() << endl;
#endif
    exit(EXIT_FAILURE);
  }
  string json_input ((std::istreambuf_iterator<char>(fin)),
                     (std::istreambuf_iterator<char>()));
  fin.close();
  if (fin.fail()) {
    cerr << "Failed to close " << name << ": " << strerror(errno) << endl;
    exit(EXIT_FAILURE);
  }

#ifdef GRAPHICS
  QScriptValue root;
  QScriptEngine engine;
  root = engine.evaluate("(" + QString::fromStdString(json_input) + ")");
  if (engine.hasUncaughtException()) {
    QScriptValue val = engine.uncaughtException();
    if (val.isError()) {
      cerr << "Failed to parse " << name << ": "
           << val.toString().toStdString() << " at line "
           << engine.uncaughtExceptionLineNumber() << endl
           << "Backtrace: "
           << engine.uncaughtExceptionBacktrace().join("\n").toStdString()
           << endl;
    }
    exit(EXIT_FAILURE);
  }
  app->exit();
  delete app;
#else
  Json::Value root;
  Json::CharReaderBuilder reader;
  std::stringstream json_input_stream;
  json_input_stream << json_input;
  std::string errs;
  if (!Json::parseFromStream(reader, json_input_stream, &root, &errs)) {
    cerr << "Failed to parse " << name << endl << errs;
    exit(EXIT_FAILURE);
  }
#endif

  Parameters input(root);

  Support::Timer t_presolver;
  t_presolver.start();
  presolve(input, options);
  double t_pre_ms = t_presolver.stop();
  if (options.output_file().empty()) {
    cout << produce_json(input, t_pre_ms);
  } else {
    fout.open(options.output_file());
    fout << produce_json(input, t_pre_ms);
    fout.close();
  }
  if (!options.dzn_file().empty()) {
    fout.open(options.dzn_file());
    fout << produce_dzn(input);
    fout.close();
  }
  if (options.verbose())
    cerr << "presolved in " << t_pre_ms << " ms" << endl;

  exit(EXIT_SUCCESS);

}
