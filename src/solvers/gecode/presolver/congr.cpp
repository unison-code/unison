/*
 *  Main authors:
 *    Mats Carlsson <mats.carlsson@ri.se>
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
 *
 *  Contributing authors:
 *    Noric Couderc <noric@sics.se>
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
#include "./congr.hpp"

using namespace std;

void gen_congr(Parameters& input) {
    Disjoint_set<operand> sets;

    // For all operand p
    for(operand p : input.P) {
      // MakeDSet(p)
      sets.make_set(p);
    }

    // For all operands p,q
    for(operand p : input.P) {
      for(operand q : input.P) {
	if(p < q) {
	  vector<temporary> pts = input.temps[p];
	  vector<temporary> qts = input.temps[q];
	  // if |OpndTemps(p)|=1 and OpndTemps(p)=OpndTemps(q) then merge
	  if (pts.size()==1 && qts.size()==1 && pts[0]==qts[0]) {
	    sets.make_union(p, q);
	  // else if p and q are preassigned to the same register then merge
	  } else {
	    if(input.p_preassign[p]>=0 &&
	       input.p_preassign[p]==input.p_preassign[q])
	      sets.make_union(p, q);
	  }
	}
      }
    }

    // For all <p,i,q,j> in JSON.aligned, d in JSON.adist, in parallel
    // such that operation(p) = i and operation(p) = j for sure
    for(unsigned int a=0; a<input.aligned.size(); a++) {
      vector<int> tuple = input.aligned[a];
      operand p = tuple[0],
              q = tuple[2];
      operation op = input.oper[p],
                oq = input.oper[q];
      instruction i = tuple[1],
                  j = tuple[3];
      int ald = input.adist[a];
      if (ald==0 &&
          input.instructions[op].size() == 1 &&
          input.instructions[op][0] == i &&
          input.instructions[oq].size() == 1 &&
          input.instructions[oq][0] == j)
	// If d=0, operation(p) = i, and operation(p) = j then merge
	sets.make_union(p, q);
    }

    // For all <p,q> in JSON.adjacent where p and q are mandatory, merge
    for(const vector<operand>& adj : input.adjacent) {
      operand p = adj[0];
      operand q = adj[1];
      if (input.temps[p][0] != NULL_TEMPORARY &&
	  input.temps[q][0] != NULL_TEMPORARY) {
	sets.make_union(p, q);
      }
    }

    map<operand,vector<operand>> key2class, min2class;
    for(operand p : input.P)
      key2class[sets.find(p)].push_back(p);
    for(const pair<operand,vector<operand>>& kc : key2class)
      min2class[kc.second[0]] = kc.second;
    for(const pair<operand,vector<operand>>& mc : min2class)
      input.strictly_congr.push_back(mc.second);
}


vector<vector<temporary>> gen_copyrel_star(const Parameters& input) {
  vector<temporary> T;
  Disjoint_set<temporary> sets;
  for(const vector<operand>& C : input.copyrel) {
    temporary prev = NULL_TEMPORARY;
    for(operand p : C) {
      if(!input.use[p]) {
	temporary t = first_temp_but_null(input, p);
	T.push_back(t);
	sets.make_set(t);
	if(prev==NULL_TEMPORARY)
	  prev = t;
	else
	  sets.make_union(prev, t);
      }
    }
  }
  for(const vector<operand>& C : input.strictly_congr) {
    temporary prev = NULL_TEMPORARY;
    for(operand p : C) {
      temporary t = first_temp_but_null(input, p);
      if(prev==NULL_TEMPORARY)
	prev = t;
      else
	sets.make_union(prev, t);
    }
  }

  multimap<temporary, temporary> M;
  // M <- emptyset
  // For all t in T
  vector<temporary> keys; // Not part of pseudocode, for later
  for(temporary t : T) {
    // AddToMap(FindDSet(t) -> t, M)
    temporary key = sets.find(t);
    M.insert(make_pair(key, t));

    // Not part of original algorithm, for later
    vector_insert(keys, key);
  }

  // return { C | _ -> C in M }
  // Means that one just drops the keys of M.
  vector<vector<temporary>> C(keys.size());
  for(const pair<temporary,temporary>& p : M) {
    unsigned int k_index = index_of(keys, p.first);
    C.at(k_index).push_back(p.second);
  }
  vector<vector<temporary>> D;
  for(vector<temporary> Ci : C) {
    sort(Ci.begin(), Ci.end());
    D.push_back(Ci);
  }
  return D;
}

void gen_calleesaved_spill(Parameters& input) {
  vector<vector<temporary> > copyrel_star = gen_copyrel_star(input);
  for(temporary t : callee_saved_temps(input)) {
    vector<operation> Si;
    for(const vector<temporary>& related : copyrel_star) {
      if(related.size()>=2 && related[0]==t) {
	temporary t1 = related[1];
	operation oi = input.oper[input.definer[t1]];
	if(input.type[oi] == COPY)
	  Si.push_back(oi);
	for(unsigned int j=2; j<related.size(); j++) {
	  temporary tj = related[j];
	  operation oj = input.oper[input.definer[tj]];
	  if(input.type[oj] == COPY)
	    Si.push_back(oj);
	}
	break;
      }
    }
    if (!Si.empty())
      input.calleesaved_spill.push_back(Si);
  }
}

vector<temporary> callee_saved_temps(const Parameters& input) {
  vector<temporary> Temps;
  if (input.calleesaved.empty() || input.callersaved.empty()) return Temps;

  set<register_atom> safe;
  for(register_atom r : input.calleesaved) safe.insert(r);
  for(const vector<int>& PR : input.preassign) {
    operation op = opnd_oper(input, PR[0]);
    if (input.type[op] != IN && input.type[op] != OUT) {
      temporary t = first_temp(input, PR[0]);
      int w = input.width[t];
      for(register_atom r = PR[1]; r<PR[1]+w; r++) safe.erase(r);
    }
  }
  for(const vector<int>& PR : input.preassign) {
    if(opnd_oper(input, PR[0]) == 0) {
      temporary T = first_temp(input, PR[0]);
      int w = input.width[T];
      for(register_atom r = PR[1]; r<PR[1]+w; r++) {
	if(!safe.count(r)) goto next_PR;
      }
      Temps.push_back(T);
    }
  next_PR: ;
  }
  return Temps;
}
