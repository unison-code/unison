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


#include "commonprocedures.hpp"

void IterationState::next(ModelOptions * options) {
  if (cf || !options->global_connection_iterations()) {
    a += options->step_aggressiveness();
    cf = false;
  } else {
    cf = true;
  }
}

bool IterationState::stop(ModelOptions * options) {
  return a > options->final_aggressiveness();
}

ostream& operator<<(ostream& os, const IterationState & state) {
  os << state.a << (state.cf ? "(c)" : "");
  return os;
}

Search::Stop * new_stop(double limit, ModelOptions * options) {
  if (options->limit_unit() == "time")
    return new Search::TimeStop(limit);
  else if (options->limit_unit() == "fails")
    return new Search::FailStop(limit);
  else GECODE_NEVER;
  return NULL;
}

vector<block>
sorted_blocks(Parameters * input, map<block, SolverResult> & latest_result) {
  vector<block> blocks(input->B);
  std::stable_sort(blocks.begin(), blocks.end(),
                   [&]
                   (const block b1,
                    const block b2) -> bool
                   {
                     return make_tuple(score(latest_result[b1]),
                                       input->freq[b1]) >
                            make_tuple(score(latest_result[b2]),
                                       input->freq[b2]);
                   });
  return blocks;
}

int score(SolverResult r) {
  switch(r) {
  case OPTIMAL_SOLUTION:
  case CACHED_SOLUTION:
    return 0;
  case UNKNOWN:
    return 1;
  case SOME_SOLUTION:
    return 2;
  case LIMIT:
    return 3;
  case UNSATISFIABLE:
    return 4;
  case SHAVING_FAILURE:
    return 5;
  default:
    GECODE_NEVER;
  }
}

vector<InstructionAssignment> shave_instructions(Model * base,
                                                 Search::Stop * stop,
                                                 Search::Statistics & stats) {
  Search::Options dummyOpts;
  vector<InstructionAssignment> forbidden;
  for (operation o : base->O()) {
    for (IntVarValues iit(base->i(o)); iit(); ++iit) {
      int ii = iit.val();
      Model * g = (Model*) base->clone();
      g->assign_instruction(o, ii);
      if (g->status() == SS_FAILED) forbidden.push_back(make_pair(o, ii));
      stats.fail++;
      delete g;
      if (stop->stop(stats, dummyOpts)) break;
    }
    if (stop->stop(stats, dummyOpts)) break;
  }
  return forbidden;
}

bool infinite_register_atom(Parameters & input, register_atom ra) {
  for (register_space rs : input.RS)
    if (input.infinite[rs] &&
	ra >= input.range[rs][0] &&
	ra <= input.range[rs][1])
      return true;
  return false;
}


vector<PresolverValuePrecedeChain>
value_precede_chains(Parameters & input, Model * m, bool global, block b) {
  vector<PresolverValuePrecedeChain> chains;
  map<vector<temporary>,vector<vector<register_atom>>> Ts2Rss;
  map<register_atom,vector<register_class>> R2Cs;
  vector<temporary> PreT;
  vector<operand> PreP;
  vector<block> B(global ? input.B : vector<block>({b}));
  vector<temporary> T(global ? input.T : input.tmp[b]);
  int maxw = 1;

  // find largest relevant width
  for (temporary t : T)
    maxw = max(input.width[t],maxw);

  // collect register classes and their sizes
  map<register_class,int> C2W;
  for (block b : B) {
    for (operation o : input.ops[b]) {
      for (unsigned int ii = 0; ii < input.instructions[o].size(); ii++) {
        for (unsigned pi = 0; pi < input.operands[o].size(); pi++) {
          operand p = input.operands[o][pi];
	  if (input.instructions[o][ii] != NULL_INSTRUCTION) {
	    register_class rc = input.rclass[o][ii][pi];
	    C2W[rc] = input.operand_width[p];
	  }
	}
      }
    }
  }
  for(const pair<register_class,int>& CW : C2W) {
    register_class C = CW.first;
    int w = CW.second;
    vector<register_atom> ras = input.atoms[C];
    if (!infinite_register_atom(input, max_of(ras)))
      for (register_atom r : ras)
	for (int d=0; d<w; d++)
	  R2Cs[r+d].push_back(C);
  }
  register_class C = input.atoms.size();

  // more register classes from preassignments
  map<operation,vector<vector<int>>> defer;
  for(const vector<int>& pr : input.preassign) {
    operand p = pr[0];
    register_atom r = pr[1];
    if (global || input.pb[p] == b) {
      operation o = input.oper[p];
      temporary t = input.temps[p][0];
      vector<operand> us = input.users[t];
      int ty = input.type[o];
      PreP.push_back(p);
      if (!input.use[p])
	PreT.push_back(t);
      if (!input.use[p] &&
	  us.size() == 1 &&
	  input.type[input.oper[us[0]]] == KILL) {
	defer[o].push_back(pr);
      } else if (binary_search(input.calleesaved.begin(), input.calleesaved.end(), r) &&
		 (ty == IN || ty == OUT)) {
	defer[o].push_back(pr);
      } else {
	int w = input.operand_width[p];
	for (int d=0; d<w; d++)
	  R2Cs[r+d].push_back(C);
	C++;
      }
    }
  }

  // deferred caller-saved that are dead upon funcall, grouped per call
  // deferred callee-saved preassignments in (in), (out)
  for (const pair<operation,vector<vector<int>>>& percall : defer) {
    for (const vector<int>& pr : percall.second) {
      operand p = pr[0];
      register_atom r = pr[1];
      int w = input.operand_width[p];
      for (int d=0; d<w; d++)
	R2Cs[r+d].push_back(C);
    }
    C++;
  }

  // prevent redefined temporaries from occurring in symmetry chains
  for (block b : B)
    for (operation o : input.ops[b])
      for (const pair<operand,operand>& pp : input.redefined[o])
	for (temporary t : input.temps[pp.second])
	  if (t != NULL_TEMPORARY)
	    PreT.push_back(t);

  // prevent (pack) 2nd operand temporaries from occurring in symmetry chains
  for (block b : B)
    for (vector<operand> ps : input.bpacked[b])
      for (temporary t : input.temps[ps[1]])
	if (t != NULL_TEMPORARY)
	  PreT.push_back(t);

  sort(PreT.begin(), PreT.end()); // will search
  sort(PreP.begin(), PreP.end()); // will search

  // more register classes from fixed (in), (out), except preassigned
  if (!global) {
    operation inop = input.ops[b][0];
    operation outop = max_of(input.ops[b]);
    for (operand p : input.operands[inop]) {
      if (!m->ry(p).assigned())
	return chains;
    }
    for (operand p : input.operands[outop]) {
      if (!m->ry(p).assigned())
	return chains;
    }
    for (operand p : input.operands[inop]) {
      if (m->ry(p).assigned() &&
	  !infinite_register_atom(input, m->ry(p).val()) &&
          !binary_search(PreP.begin(), PreP.end(), p)) {
	int w = input.operand_width[p];
	for (int d=0; d<w; d++)
	  R2Cs[m->ry(p).val()+d].push_back(C);
	C++;
      }
    }
    for (operand p : input.operands[outop]) {
      if (m->ry(p).assigned() &&
	  !infinite_register_atom(input, m->ry(p).val()) &&
          !binary_search(PreP.begin(), PreP.end(), p)) {
	int w = input.operand_width[p];
	for (int d=0; d<w; d++)
	  R2Cs[m->ry(p).val()+d].push_back(C);
	C++;
      }
    }
  }
  
  for (int curw = 1; curw <= maxw; curw = 2*curw) {
    // merge unaligned (wrt. curw) R2Cs items into aligned ones
    vector<register_atom> odd;
    for(const pair<register_atom,vector<register_class>>& RCs : R2Cs)
      if (RCs.first % curw > 0)
	odd.push_back(RCs.first);
    for(register_atom a : odd) {
      register_atom e = a - (a % curw);
      vector<register_class> U;
      set_union(R2Cs[e].begin(), R2Cs[e].end(), R2Cs[a].begin(), R2Cs[a].end(), back_inserter(U));
      R2Cs[e] = U;
      R2Cs.erase(a);
    }
    
    map<vector<register_class>,vector<register_atom>> Cs2Rs;
    
    // regroup by set of register class
    for(const pair<register_atom,vector<register_class>>& RCs : R2Cs)
      Cs2Rs[RCs.second].push_back(RCs.first);

    // build the chains
    for(const pair<vector<register_class>,vector<register_atom>>& CsRs : Cs2Rs) {
      vector<register_class> Cs = CsRs.first;
      vector<register_atom> Rs = CsRs.second;
      if ((int)Rs.size()>curw) {
	vector<temporary> Ts;
	for(temporary t : T) {
	  int ty = input.type[input.def_opr[t]];
	  if (input.width[t]==curw &&
	      (ty == LINEAR || ty == COPY) &&
	      !binary_search(PreT.begin(), PreT.end(), t)) {
	    operand p = input.definer[t];
	    operation o = input.oper[p];
	    unsigned pi = p - input.operands[o][0];
	    for (unsigned int ii = 0; ii < input.instructions[o].size(); ii++) {
	      register_class rc = input.rclass[o][ii][pi];
	      if (binary_search(Cs.begin(), Cs.end(), rc)) {
		Ts.push_back(t);
		goto nextt;
	      }
	    }
	  }
	nextt: ;
	}
	if (!Ts.empty())
	  Ts2Rss[Ts].push_back(Rs);
      }
    }
  }

  for(const pair<vector<temporary>,vector<vector<register_atom>>>& TsRs : Ts2Rss) {
    PresolverValuePrecedeChain vpc;
    for (temporary t : TsRs.first) {
      // FIXME: this check is added temporarily, needs investigation
      if (!contains(input.callee_saved_loads, input.def_opr[t])) {
        vpc.ts.push_back(t);
      }
    }
    vpc.rss = TsRs.second;
    sort(vpc.rss.begin(), vpc.rss.end()); // canonicalize
    chains.push_back(vpc);
    // cerr << "* VPC block " << b << " = " << show(vpc) << endl;
  }
  return chains;
}
