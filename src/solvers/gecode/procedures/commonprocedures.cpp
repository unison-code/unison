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

Gecode::SpaceStatus status_lb(GlobalModel * base) {
  SpaceStatus ss = base->status();
  emit_lower_bound(base);
  return ss;
}

void emit_lower_bound(const GlobalModel * base, bool proven) {
  if (base->options->lower_bound_file().empty()) return;
  vector<int> lbs;
  for (unsigned int n = 0; n < base->input->N; n++) {
    lbs.push_back(proven ? -1 : base->cost()[n].min());
  }
  ofstream fout;
  fout.open(base->options->lower_bound_file());
  fout << "{\"lower_bound\":" << show(lbs, ", ", "", "[]") << "}" << endl;
  fout.close();
}

LocalModel * make_local(const GlobalModel * gs, block b) {
  return make_local(gs, b, gs->ipl);
}

LocalModel * make_local(const GlobalModel * gs, block b, IntPropLevel p_ipl) {
  return new LocalModel(gs->input, gs->options, p_ipl, gs, b);
}

bool infinite_register_atom(Parameters & input, register_atom ra) {
  for (register_space rs : input.RS)
    if (input.infinite[rs] &&
	ra >= input.range[rs][0] &&
	ra <= input.range[rs][1])
      return true;
  return false;
}

// Idea: values v1, v2 are symmetric iff they are _covered_ by identical sets of register classes + preassignments
// Value v is covered by register class rc of width w iff (e <= v < e+w) where e is in rc
// Value v is preassignment p:ra of width w iff (ra <= v < ra+w)
// Preassigned (kill) operands and pressigned callee-saved (in)/(out) operands are clumped per operation
// All other preassignments are counted individually
// Temps defined preassigned are not subject to symmetry breaking
// Temps defined by reassignment, in (pack), or in exrelated operands, are not subject to symmetry breaking
// Temps defined by callee-saved-load are not subject to symmetry breaking, maybe because they get reloaded in reverse order
// Temps not defined by (linear) or (copy) are not subject to symmetry breaking
vector<PresolverValuePrecedeChain>
value_precede_chains(Parameters & input, Model * m, bool global, block b) {
  vector<PresolverValuePrecedeChain> chains;
  map<vector<temporary>,vector<vector<register_atom>>> SymChainsOfTemps;
  map<register_atom,vector<register_class>> CoveringClassesOfReg;
  set<temporary> PreT;
  set<operand> PreP;
  vector<block> B(global ? input.B : vector<block>({b}));
  vector<temporary> T(global ? input.T : input.tmp[b]);

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
	  CoveringClassesOfReg[r+d].push_back(C);
  }
  register_class C = input.atoms.size();

  // more register classes from preassignments
  map<operation,vector<vector<int>>> defer;
  for(const vector<int>& pr : input.preassign) {
    operand p = pr[0];
    register_atom r = pr[1];
    if (global || input.pb[p] == b) {
      temporary t = input.temps[p][0];
      if (t != NULL_TEMPORARY) {
	vector<operand> us = input.users[t];
	operation o = input.oper[p];
	int ty = input.type[o];
	PreP.insert(p);
	if (!input.use[p])
	  PreT.insert(t);
	if (!input.use[p] &&
	    us.size() == 1 &&
	    input.type[input.oper[us[0]]] == KILL) {
	  defer[o].push_back(pr);
	} else if (binary_search(input.calleesaved.begin(), input.calleesaved.end(), r) &&
		   (ty == IN || ty == OUT)) {
	  defer[o].push_back(pr);
	} else {
	  int w = input.operand_width[p];
	  C2W[C] = w;
	  for (int d=0; d<w; d++)
	    CoveringClassesOfReg[r+d].push_back(C);
	  C++;
	}
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
      C2W[C] = w;
      for (int d=0; d<w; d++)
	CoveringClassesOfReg[r+d].push_back(C);
    }
    C++;
  }

  // prevent redefined temporaries from occurring in symmetry chains
  for (block b : B)
    for (operation o : input.ops[b])
      for (const pair<operand,operand>& pp : input.redefined[o])
	for (temporary t : input.temps[pp.second])
	  if (t != NULL_TEMPORARY)
	    PreT.insert(t);

  // prevent (pack) operand temporaries from occurring in symmetry chains
  for (block b : B)
    for (vector<operand> ps : input.bpacked[b])
      for (operand p : ps)
	for (temporary t : input.temps[p])
	  if (t != NULL_TEMPORARY)
	    PreT.insert(t);

  // prevent ext. related operand temporaries from occurring in symmetry chains
  for (vector<operand> ps : input.exrelated)
    for (operand p : ps)
      for (temporary t : input.temps[p])
        if (t != NULL_TEMPORARY)
          PreT.insert(t);

  // FIXME: this check is added temporarily, needs investigation
  // prevent temps defined by callee-saved-load from occurring in symmetry chains
  for (operation o : input.callee_saved_loads)
    if (global || input.oblock[o]==b)
      for (operand p : input.operands[o])
	if (!input.use[p])
	  PreT.insert(input.single_temp[p]);

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
      int r = m->ry(p).val();
      if (r >= 0 &&
	  !infinite_register_atom(input, m->ry(p).val()) && !PreP.count(p)) {
	int w = input.operand_width[p];
	C2W[C] = w;
	for (int d=0; d<w; d++)
	  CoveringClassesOfReg[r+d].push_back(C);
	C++;
      }
    }
    for (operand p : input.operands[outop]) {
      int r = m->ry(p).val();
      if (r >= 0 &&
	  !infinite_register_atom(input, m->ry(p).val()) && !PreP.count(p)) {
	int w = input.operand_width[p];
	C2W[C] = w;
	for (int d=0; d<w; d++)
	  CoveringClassesOfReg[r+d].push_back(C);
	C++;
      }
    }
  }

  map<vector<register_class>,vector<register_atom>> RegsOfClasses;

  // regroup by set of covering register class
  for(const pair<register_atom,vector<register_class>>& RCs : CoveringClassesOfReg)
    RegsOfClasses[RCs.second].push_back(RCs.first);

  // build the chains
  for(const pair<vector<register_class>,vector<register_atom>>& CsRs : RegsOfClasses) {
    vector<register_class> Cs = CsRs.first;
    vector<register_atom> Rs = CsRs.second;
    if ((int)Rs.size()>1) {
      vector<temporary> Ts;
      for(temporary t : T) {
	int ty = input.type[input.def_opr[t]];
	if ((ty == LINEAR || ty == COPY) && !PreT.count(t)) {
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
	SymChainsOfTemps[Ts].push_back(Rs);
    }
  }

  for(const pair<vector<temporary>,vector<vector<register_atom>>>& TsRs : SymChainsOfTemps) {
    PresolverValuePrecedeChain vpc;
    map<int,vector<temporary>> w2ts;
    for (temporary t : TsRs.first) // sort temps by decreasing width
      w2ts[-input.width[t]].push_back(t);
    for (auto wts : w2ts)
      vpc.ts.insert(vpc.ts.end(), wts.second.begin(), wts.second.end());
    vpc.rss = TsRs.second;
    sort(vpc.rss.begin(), vpc.rss.end()); // canonicalize
    // cerr << "SYMMETRY " << show(vpc) << endl;
    chains.push_back(vpc);
  }
  return chains;
}
