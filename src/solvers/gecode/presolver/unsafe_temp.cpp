/*
 *  Main authors:
 *    Noric Couderc <noric@sics.se>
 *
 *  Contributing authors:
 *    Mats Carlsson <mats.carlsson@ri.se>
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
#include "./unsafe_temp.hpp"

using namespace std;

bool temp_is_unsafe(const Parameters& input, const temporary t) {
  // operand that can define temporary t
  operand d = temp_def(input, t);
  // operands that can use temporary t
  vector<operand> U = temp_uses(input, t);
  // operation that defines temporary t
  operation o = temp_oper(input, t);

  int otype = input.type[o];
  if (otype != IN  && otype != DEFINE && otype != FUN) {
    int ld = std::numeric_limits<int>::max();
    int lu = std::numeric_limits<int>::max();

    // For each instruction that can implement operation o
    for(unsigned int i = 0; i < oper_insns(input, o).size(); ++i) {
      // Latency of operand d, in operation o, if it' implemented by
      // instruction i
      if(oper_insns(input, o)[i] != NULL_INSTRUCTION) {
	unsigned int di = index_of(oper_opnds(input, o), d);
	ld = std::min(ld, input.lat[o][i][di]);
      }
    } // I get minimum latency of the definer of temporary
    // I assume that the operation does not have ONLY null
    // instructions

    // For operand that can use t
    for(operand u : U) {
      // For the operation that might use t
      operation o1 = opnd_oper(input, u);
      // For each instruction that can implement operation o1
      for(unsigned int i = 0; i < oper_insns(input, o1).size(); ++i) {
	if(oper_insns(input, o1)[i] != NULL_INSTRUCTION) {
	  unsigned int ui = index_of(oper_opnds(input, o1), u);
	  lu = std::min(lu, input.lat[o1][i][ui]);
	}
      }
    } // I get minimum latency of all the users of the temporary

    // If the minimum live range (which is min 1 cycle) of the
    // temporary is bigger than ld + lu
    if (input.minlive[t] > ld + lu)
      return true;
  }
  return false;
}

static bool temp_users_predef(Parameters& input, temporary t, set<int>& rdom) {
  for (operand p : input.users[t]) {
    int r = input.p_preassign[p];
    if (r < 0)
      return false;
    else
      rdom.insert(r);
  }
  return true;
}

#if 0

void temp_domain(Parameters& input) {
  vector<temporary> deferred_in, deferred_combine;
  // phase 1: all except those defined by congruent (in) operands
  for (temporary t : input.T) {
    operand p = input.definer[t];
    bool optional = (input.temps[p][0] == NULL_TEMPORARY);
    int r = input.p_preassign[p];
    operation o = input.def_opr[t];
    set<int> rdom;
    if (optional)
      rdom.insert(-1);
    if (r >= 0) {
      input.temp_domain[t].push_back(r);
    } else if (input.type[o] == IN) {
      deferred_in.push_back(t);
    } else if (input.type[o] == COMBINE) {
      deferred_combine.push_back(t);
    } else if (temp_users_predef(input, t, rdom)) {
      input.temp_domain[t].insert(input.temp_domain[t].end(), rdom.begin(), rdom.end());
    } else {
      rdom.clear();
      if (optional)
	rdom.insert(-1);
      unsigned int nbinsn = input.instructions[o].size();
      unsigned int opndno = 0;
      for (operand p0 : input.operands[o])
	if (p0 == p)
	  break;
        else
	  opndno++;      
      for (unsigned int j=0; j<nbinsn; j++) {
	if (input.instructions[o][j] != NULL_INSTRUCTION) {
	  register_class rc = input.rclass[o][j][opndno];
          register_space rs = input.space[rc];
	  pair<temporary, register_space> trs = make_pair(t, rs);
	  if (input.infinite[rs] &&
              input.infinite_atom_range.count(trs)) {
            register_atom fra = input.infinite_atom_range[trs][0],
	                  lra = input.infinite_atom_range[trs][1];
	    unsigned int w = input.width[t];
	    for (int a = fra; a <= lra; a += w)
	      rdom.insert(a);
	  } else {
	    rdom.insert(input.atoms[rc].begin(), input.atoms[rc].end());
	  }
	}
      }
      input.temp_domain[t].insert(input.temp_domain[t].end(), rdom.begin(), rdom.end());
    }
  }
  // phase 2: congruent (in) operands
  map<operand,vector<operand>> in2outs;
  for (const vector<operand>& adj : input.adjacent) {
    operand outp = adj[0];
    operand inp = adj[1];
    in2outs[inp].push_back(outp);
  }
  vector<pair<temporary, temporary> > E;
  for (temporary t : deferred_in) {
    operand inp = input.definer[t];
    for (operand outp : in2outs[inp])
      for (temporary u : input.temps[outp])
	if (u != NULL_TEMPORARY)
	  E.push_back(make_pair(t,u));
  }
  for (temporary t : deferred_combine) {
    operation o = input.def_opr[t];
    for (temporary u : input.temps[input.operands[o][0]])
      if (u != NULL_TEMPORARY)
	E.push_back(make_pair(t,u));
  }
  Digraph G(E);
  deferred_in.insert(deferred_in.end(), deferred_combine.begin(), deferred_combine.end());
  for (temporary t : deferred_in) {
    operand p = input.definer[t];
    set<int> rdom;
    for (temporary u : G.reachables(t))
      rdom.insert(input.temp_domain[u].begin(), input.temp_domain[u].end());
    if (input.temps[p][0] == NULL_TEMPORARY)
      rdom.insert(-1);
    else
      rdom.erase(-1);
    input.temp_domain[t].insert(input.temp_domain[t].end(), rdom.begin(), rdom.end());
  }
  for (temporary t : deferred_combine) {
    operation o = input.def_opr[t];
    operand p = input.operands[o][1];
    if (input.temps[p].size() == 1) {
      temporary buddy = input.temps[p][0];
      int w = input.width[buddy];
      input.temp_domain[buddy].clear();
      for (int x : input.temp_domain[t])
	input.temp_domain[buddy].push_back(x + w);
    }
  }
  // phase 3: poor man's alldiff on every (in)
  for (block b : input.B) {
    vector<int> clash;
    for (operand p : input.operands[input.in[b]]) {
      temporary t = input.temps[p][0];
      if (t != NULL_TEMPORARY) {
	vector<temporary> tdt = input.temp_domain[t];
	if (tdt.size() == 1) {
	  clash.push_back(tdt[0]);
	}
      }
    }
    for (operand p : input.operands[input.in[b]]) {
      temporary t = input.temps[p][0];
      if (t != NULL_TEMPORARY) {
	vector<temporary> tdt = input.temp_domain[t];
	if (tdt.size() > 1) {
	  for (int x : clash)
	    vector_erase(tdt, x);
	  input.temp_domain[t] = tdt;
	}
      }
    }
  }  
  // phase 4: in theory, solver's propagation should compute the same thing
  {
    ModelOptions moptions;
    RelaxedModel * base = new RelaxedModel(&input, &moptions, IPL_DOM);
    // base->post_standalone_constraints();
    if (base->status() != SS_FAILED) { // FIXME: should be assertion
      for (temporary t : input.T) {
	vector<int> rdom;
	populate_r_domain(base, t, rdom);
	if (input.temp_domain[t] != rdom) {
	  cerr << "* D(r[" << t << "]) = " << show(input.temp_domain[t]) << " by analysis" << endl;
	  cerr << "* D(r[" << t << "]) = " << show(rdom) << " by propagation" << endl;
	}
      }
    }
    delete base;
  }
}

#else

void temp_domain(Parameters& input) {
  ModelOptions moptions;
  RelaxedModel * base = new RelaxedModel(&input, &moptions, IPL_DOM);
  // if CompleteModel: base->post_standalone_constraints();
  assert(base->status() != SS_FAILED);
  for (temporary t : input.T)
    populate_r_domain(base, t, input.temp_domain[t]);
  delete base;
}

#endif
