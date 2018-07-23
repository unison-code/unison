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

string produce_dzn(Parameters &input)
{
  stringstream dzn;
  vector<int> maxatom;
  vector<vector<int>> operands(input.maxc.length());
  vector<vector<int>> temps(input.maxc.length());
  vector<pair<int,int>> len_bb(input.maxc.length());
  vector<int> bb_order;
  vector<int> mand;
  vector<vector<int>> atom_regs;
  vector<int> infinite;
  vector<vector<int>> con_transpose(input.cap.length());
  vector<vector<int>> res_transpose(input.cap.length());
  vector<vector<int>> off_transpose(input.cap.length());
  vector<vector<int>> preassign(2);
  vector<vector<int>> aligned(4);
  vector<vector<int>> adjacent(2);
  vector<vector<int>> quasi_adjacent(2);
  vector<vector<int>> long_data_def_use(2);
  vector<int> operand_definer;
  vector<vector<int>> operand_atom;
  vector<vector<int>> temp_uses(input.T.length());
  vector<int> before_pred, before_succ, before_cond; 
  vector<int> across_op, across_item_temp, across_item_cond;
  vector<vector<int>> across_regs, across_items;
  vector<int> setacross_op;
  vector<vector<int>> setacross_regs, setacross_tempsets;
  vector<vector<int>> domop(2);
  vector<vector<int>> domuse(3);
  vector<int> dominate_ing;
  vector<int> dominate_ed;
  vector<vector<int>> dominate_instructions;
  vector<vector<int>> dominate_temps;
  vector<vector<int>> table_exists_ops;
  vector<vector<int>> table_iffall_ops;
  vector<vector<int>> relation_ops;
  vector<vector<int>> relation_temps;
  vector<int> relation_ntuples;
  vector<vector<int>> relation_range;



  

  for (int as : input.atoms)
    maxatom.push_back(as.back());
  for (int p : input.P)
    operands[input.block[input.oper[p]]].push_back(p);
  for (int t : input.T)
    temps[input.block[input.def_opr[t]]].push_back(t);
  for (unsigned int i=0; i<input.ops.size(); i++)
    len_bb[i] = make_pair(input.ops[i].front() - input.ops[i].back(), i+1);
  sort(len_bb.begin(), len_bb.end());
  for (pair<int,int>& kb : len_bb)
    bb_order.push_back(kb.second());
  for (vector<int>& is : input.instructions)
    mand.push_back(is[0] > 0);
  atom_regs.push_back({});
  for (vector<int>& rs : input.atoms)
    atom_regs.push_back(rs);
  for (unsigned int i=0; i<input.infinite.size(); i++)
    if (input.infinite[i]) {
      int a = input.range[i][0];
      int b = input.range[i][1];
      for (unsigned int j=a; j<=b; j++)
	infinite.push_back(j);
    }
  for (vector<int>& cs : input.con)
    for (unsigned int i=0; i<cs.size(); i++)
      con_transpose[i].push_back(cs[i]);
  for (vector<int>& cs : input.res)
    for (unsigned int i=0; i<cs.size(); i++)
      res_transpose[i].push_back(cs[i]);
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
  for (vector<int>& pr : input.long_data_def_use)
    for (unsigned int i=0; i<2; i++)
      long_data_def_use[i].push_back(pr[i]);
  for (unsigned int o=0; o<input.operands.length(); o++)
    for (p : input.operands[o])
      operand_definer.push_back(o);
  for (unsigned int o=0; o<input.operands.length(); o++) {
    vector<int> opnds = input.operands[o];
    vector<int> insns = input.instructions[o];
    for (unsigned int j=0; o<opnds.length(); j++) {
      vector<int> row;
      int ii = 0;
      for (int ins : input.O)
	if (ins == insns[ii])
	  row.push_back(input.rclass[o][ii++][j]);
	else
	  row.push_back(-1);
      operand_atom.push_back(row);
    }
  for (unsigned int p=0; p<input.temps.length(); p++)
    if (input.use[p])
      for (int t : input.temps[p])
	if (t >= 0)
	  temp_uses[t].push_back(p);
  for (PresolverBeforeJSON& bef : input.before) {
    before_pred.push_back(bef.p);
    before_succ.push_back(bef.q);
    before_cond.push_back(bef.e);	// FIXME
  }
  for (PresolverAcrossJSON& a : input.across) {
    int ii = 1;
    across_op.push_back(a.op);
    across_regs.push_back(a.ras);
    vector<int> items;
    for (PresolverAcrossItemJSON& aitem : a.as) {
      items.push_back(ii++);
      across_item_temp.push_back(aitem.t);
      across_item_cond.push_back(aitem.e); // FIXME
    }
    across_items.push_back(items);
  }
  for (PresolverSetAcross& sa : input.set_across) {
    setacross_op.push_back(sa.o);
    setacross_regs.push_back(sa.ras);
    setacross_tempsets.push_back(sa.tsets);
  }
  for (vector<int>& pr : input.domop)
    for (unsigned int i=0; i<2; i++)
      domop[i].push_back(pr[i]);
  for (vector<int>& pr : input.domuses)
    for (unsigned int i=0; i<3; i++)
      domuse[i].push_back(pr[i]);
  space.push_back(-1);
  for (int s : input.space)
    space.push_back(s);
  for (PresolverDominates& pd : input.dominates) {
    dominate_ing.push_back(pd.o1);
    dominate_ed.push_back(pd.o2);
    dominate_instructions.push_back(pd.ins);
    dominate_temps.push_back(pd.temps);
  }
  for (const PresolverActiveTable& AT : input.active_tables) {
    if (AT.tuples.length() == 2 &&
	AT.tuples[0] == {0,0} &&
	AT.tuples[1] == {1,1}) {
      table_iffall_ops.push_back(AT.os);
    } else if (at_least_one_table(AT.tuples)) {
      table_exists_ops.push_back(AT.os);
    } else {
      relation_os.push_back(AT.os);
      relation_ps.push_back({});
      relation_ntuples.push_back(AT.tuples.length());
      relation_range.push_back(AT.tuples); // TODO
    }
  }
  for (const PresolverCopyTmpTable& TT : input.tmp_tables) {
    relation_os.push_back(TT.os);
    relation_ps.push_back(TT.ps);
    relation_tuples.push_back(TT.tuples);
  }
  
  
  
  dzn  << emit_dzn_int("MAXF", input.maxf)
       << emit_dzn_int("MAXO", input.O.back())
       << emit_dzn_int("MAXP", input.P.back())
       << emit_dzn_int("MAXT", input.T.back())
       << emit_dzn_int("MAXI", input.I.back())
       << emit_dzn_int("MAXC", input.maxc.back())
       << emit_dzn_int("MAXR", max_of(maxatom))
       << emit_dzn_bool("optimize_cycles", (input.optimize_resource.front() == -1))
       << emit_dzn_array1d("bb_ops", input.ops)
       << emit_dzn_array1d("bb_operands", operands)
       << emit_dzn_array1d("bb_temps", temps)
       << emit_dzn_array1d("bb_subsumed", input.subsumed_resources)
       << emit_dzn_array1d("bb_freq", input.freq)
       << emit_dzn_array1d("bb_maxcycle", input.maxc)
       << emit_dzn_array1d("bb_optional_min", input.optional_min)
       << emit_dzn_array1d("bb_order", bb_order)
       << emit_dzn_array1d("op_operands", input.operands, ZEROBASED1)
       << emit_dzn_array1d("op_instructions", input.instructions, ZEROBASED1)
       << emit_dzn_array1d("op_type", input.type, ZEROBASED1)
       << emit_dzn_array1d("op_mand", mand, ZEROBASED1)
       << emit_dzn_array1d("atom_regs", atom_regs, MINUSONEBASED1)
       << emit_dzn_array1d("calleesaved", input.calleesaved)
       << emit_dzn_array1d("callersaved", input.callersaved)
       << emit_dzn_array1d("infinite", infinite)
       << emit_dzn_array1d("range", input.range)
       << emit_dzn_array1d("res_cap", input.cap)
       << emit_dzn_array2d("res_con", con_transpose, ZEROBASED2)
       << emit_dzn_array2d("res_dur", dur_transpose, ZEROBASED2)
       << emit_dzn_array2d("res_off", off_transpose, ZEROBASED2)
       << emit_dzn_array1d("congr", input.congr)
       << emit_dzn_array1d("strictly_congr", input.strictly_congr)
       << emit_dzn_array1d("preassign_operand", preassign[0])
       << emit_dzn_array1d("preassign_reg", preassign[1])
       << emit_dzn_array1d("aligned_def", aligned[0])
       << emit_dzn_array1d("aligned_use", aligned[1])
       << emit_dzn_array1d("aligned_defi", aligned[2])
       << emit_dzn_array1d("aligned_usei", aligned[3])
       << emit_dzn_array1d("aligned_dist", input.adist)
       << emit_dzn_array1d("adj_from", adjacent[0])
       << emit_dzn_array1d("adj_to", adjacent[1])
       << emit_dzn_array1d("quasi_adj_from", quasi_adjacent[0])
       << emit_dzn_array1d("quasi_adj_to", quasi_adjacent[1])
       << emit_dzn_array2d("long_latency_index", input.long_latency_index)
       << emit_dzn_array1d("long_data_def", long_data_def_use[0], ZEROBASED1)
       << emit_dzn_array1d("long_data_use", long_data_def_use[1], ZEROBASED1)
       << emit_dzn_array1d("operand_definer", operand_definer, ZEROBASED1)
       << emit_dzn_array1d("operand_use", input.use, ZEROBASED1)
       << emit_dzn_array1d("operand_lastuse", input.last_use, ZEROBASED1)
       << emit_dzn_array1d("operand_temps", input.temps, ZEROBASED1)
       << emit_dzn_array2d("operand_atom", operand_atom, ZEROBASED1 + ZEROBASED2)
       << emit_dzn_array1d("related_temps", input.related_temps)
       << emit_dzn_array1d("temp_definer", input.def_opr, ZEROBASED1)
       << emit_dzn_array1d("temp_def", input.definer, ZEROBASED1)
       << emit_dzn_array1d("temp_width", input.width, ZEROBASED1)
       << emit_dzn_array1d("temp_minlive", input.minlive, ZEROBASED1)
       << emit_dzn_array1d("temp_uses", input.minlive, ZEROBASED1)
       << emit_dzn_array2d("packed_pq", input.packed)
       << emit_dzn_array1d("before_pred", before[0])
       << emit_dzn_array1d("before_succ", before[1])
       << emit_dzn_array1d("before_cond", before[2])
       << emit_dzn_array1d("nogood", input.nogoods) // FIXME
       << emit_dzn_array1d("across_op", across_op)
       << emit_dzn_array1d("across_regs", across_regs)
       << emit_dzn_array1d("across_items", across_items)
       << emit_dzn_array1d("across_item_temp", across_item_temp)
       << emit_dzn_array1d("across_item_cond", across_item_cond)
       << emit_dzn_array1d("setacross_op", setacross_op)
       << emit_dzn_array1d("setacross_regs", setacross_regs)
       << emit_dzn_array1d("setacross_tempsets", setacross_tempsets)
       << emit_dzn_array1d("difftemp", input.difftemp)
       << emit_dzn_array1d("diffreg", input.diffreg)
       << emit_dzn_array1d("domop_operands", domop[0])
       << emit_dzn_array1d("domop_temps", domop[1])
       << emit_dzn_array1d("domuse_p", domuse[0])
       << emit_dzn_array1d("domuse_q", domuse[1])
       << emit_dzn_array1d("domuse_r", domuse[2])
       << emit_dzn_array2d("infassign", input.infassign)
       << emit_dzn_array1d("space", space, MINUSONEBASED1)
       << emit_dzn_array1d("dominate_ing", dominate[0])
       << emit_dzn_array1d("dominate_ed", dominate[1])
       << emit_dzn_array1d("dominate_instructions", dominate[2])
       << emit_dzn_array1d("dominate_temps", dominate[3])
       << emit_dzn_array1d("precedence", input.precedence) // FIXME
       << emit_dzn_array1d("table_exists_ops", table_exists_ops)
       << emit_dzn_array1d("table_iffall_ops", table_iffall_ops)
       << emit_dzn_array1d("relation_ops", relation_ops)
       << emit_dzn_array1d("relation_temps", relation_temps)
       << emit_dzn_array1d("relation_ntuples", relation_ntuples)
       << emit_dzn_array1d("relation_range", relation_range)




<< emit_dzn_line("B", input.B)
       << emit_dzn_line("O", input.O)
       << emit_dzn_line("P", input.P)
       << emit_dzn_line("T", input.T)
       << emit_dzn_line("block", input.oblock)
       << emit_dzn_line("operands", input.operands)
       << emit_dzn_line("temps", input.temps)
       << emit_dzn_line("use", input.use)
       << emit_dzn_line("adjacent", input.adjacent)
       << emit_dzn_line("preassign", input.preassign)
       << emit_dzn_line("width", input.width)
       << emit_dzn_line("freq", input.freq)
       << emit_dzn_line("minlive", input.minlive)
       << emit_dzn_line("dep", input.dep)
       << emit_dzn_line("preschedule", input.prescheduled)
       << emit_dzn_line("I", input.I)
       << emit_dzn_line("R", input.R)
       << emit_dzn_line("dist", input.dist)
       << emit_dzn_line("class", input.rclass)
       << emit_dzn_line("atoms", input.atoms)
       << emit_dzn_line("instructions", input.instructions)
       << emit_dzn_line("lat", input.lat)
       << emit_dzn_line("bypass", input.bypass)
       << emit_dzn_line("cap", input.cap)
       << emit_dzn_line("con", input.con)
       << emit_dzn_line("dur", input.dur)
       << emit_dzn_line("off", input.off)
       << emit_dzn_line("aligned", input.aligned)
       << emit_dzn_line("adist", input.adist)
       << emit_dzn_line("packed", input.packed)
       << emit_dzn_line("exrelated", input.exrelated)
       << emit_dzn_line("table", input.table)
       << emit_dzn_line("activators", input.activators)
       << emit_dzn_line("E", input.E)
       << emit_dzn_line("optimize_dynamic", input.optimize_dynamic)
       << emit_dzn_line("optimize_resource", input.optimize_resource)
       << emit_dzn_line("tmp", input.tmp)
       << emit_dzn_line("definer", input.definer)
       << emit_dzn_line("operation", input.oper)
       << emit_dzn_line("def_opr", input.def_opr)
       << emit_dzn_line("congr", input.congr)
       << emit_dzn_line("ops", input.ops)
       << emit_dzn_line("RS", input.RS)
       << emit_dzn_line("maxc", input.maxc)
       << emit_dzn_line("type", input.type)
       << emit_dzn_line("copyrel", input.copyrel)
       << emit_dzn_line("interchangeable", input.interchangeable)
       << emit_dzn_line("cfg", input.cfg)
       << emit_dzn_line("clusters", input.clusters)
       << emit_dzn_line("space", input.space)
       << emit_dzn_line("range", input.range)
       << emit_dzn_line("home", input.home)
       << emit_dzn_line("infinite", input.infinite)
       << emit_dzn_line("bounded", input.bounded)
       << emit_dzn_line("insname", input.insname)
       << emit_dzn_line("atomname", input.atomname)
       << emit_dzn_line("classname", input.classname)
       << emit_dzn_line("spacename", input.spacename)
       << emit_dzn_line("callersaved", input.callersaved)
       << emit_dzn_line("calleesaved", input.calleesaved)
       << emit_dzn_line("optional_min", input.optional_min)
       << emit_dzn_line("active_tables", input.active_tables)
       << emit_dzn_line("tmp_tables", input.tmp_tables)
       << emit_dzn_line("nogoods", input.nogoods)
       << emit_dzn_line("nogoods2", input.nogoods2)
       << emit_dzn_line("precedences", input.precedences)
       << emit_dzn_line("precedences2", input.precedences2)
       << emit_dzn_line("before", input.before)
       << emit_dzn_line("before2", input.before2)
       << emit_dzn_line("across", input.across)
       << emit_dzn_line("set_across", input.set_across)
       << emit_dzn_line("domops", input.domops)
       << emit_dzn_line("last_use", input.last_use)
       << emit_dzn_line("infassign", input.infassign)
       << emit_dzn_line("domuses", input.domuses)
       << emit_dzn_line("precs", input.precs)
       << emit_dzn_line("assignhints", input.assignhints)
       << emit_dzn_line("dominates", input.dominates)
       << emit_dzn_line("difftemps", input.difftemps)
       << emit_dzn_line("diffregs", input.diffregs)
       << emit_dzn_line("calleesaved_spill", input.calleesaved_spill)
       << emit_dzn_line("strictly_congr", input.strictly_congr)
       << emit_dzn_line("predecessors", input.predecessors)
       << emit_dzn_line("successors", input.successors)
       << emit_dzn_line("value_precede_chains", input.value_precede_chains)
       << emit_dzn_line("quasi_adjacent", input.quasi_adjacent)
       << emit_dzn_line("long_latency_index", input.long_latency_index)
       << emit_dzn_line("long_latency_def_use", input.long_latency_def_use)
       << emit_dzn_line("subsumed_resources", input.subsumed_resources)
       << emit_dzn_line("temp_domain", input.temp_domain)
       << emit_dzn_line("freq_scale", input.freq_scale)
  return json.str();
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
