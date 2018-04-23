/*
 *  Main authors:
 *    Mikael Almgren <mialmg@kth.se>
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
 *    Mats Carlsson <mats.carlsson@ri.se>
 *
 *  This file is part of Unison, see http://unison-code.github.io
 *
 *  Copyright (c) 2015-2016, Mikael Almgren
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

#include "presolverprocedures.hpp"

#if 0
// Return if e is member in V.
bool member(const std::vector<int>& V, int e);

template<typename T> bool member(const std::vector<T>& V, const T& e){
  for(T v : V) {
    if(v == e) return true;
  }
  return false;
}

// Inserts e into v, if not already in v.
// v is a set stored in a vector
template<typename T>
void vector_insert(std::vector<T>& v, const T& e){
  typename std::vector<T>::iterator it = std::lower_bound(v.begin(), v.end(), e);
  if(it == v.end() || e < *it){
    v.insert(it, e);
  }
}
#endif

TmpTableResult get_labelings(Parameters& input, RelaxedModel * base,
			     vector<operation> O, vector<operand> P,
			     int timeout) {
  TmpTableResult result;
  int nbsol = 0;		// runaway prevention

  // Set timeout
  Search::Stop * relaxedStop = new Search::TimeStop(timeout);
  Search::Options opt;
  opt.clone = false;		// Christian's advice
  opt.stop = relaxedStop;

  RelaxedModel * r_base = (RelaxedModel *) base->clone();
  r_base->post_active_operation_branching(O);
  r_base->post_operand_temporary_branching(P);

  DFS<RelaxedModel> e(r_base, opt);

  // Find labellings
  RelaxedModel * r;
  while((r = e.next()) && nbsol<1000) {
    vector<int> a_lab, t_lab;

    if(exists_i_labeling(r, O)) {
      for(operation o : O)
	a_lab.push_back(r->a(o).val());

      for(operand p : P) {
	vector<temporary> temps = input.temps[p];
	t_lab.push_back(temps[r->y(p).val()]);
      }
      result.labelings[a_lab].insert(t_lab);
      nbsol++;
    }
    delete r;
  }

  delete relaxedStop;

  // Set status messages
  if (e.stopped() || nbsol>=1000) {
    result.timeout_status = RELAXED_TIMEOUT;
  } else {
    result.timeout_status = RELAXED_NO_TIMEOUT;
    result.solution_status = RELAXED_SOLUTION_FOUND;
  }

  return result;
}

ActiveTableResult get_labelings(RelaxedModel * base,
				vector<operation> O, vector<operand> P,
				int timeout) {
  ActiveTableResult result;

  // Set timeout
  Search::Stop * relaxedStop = new Search::TimeStop(timeout);
  Search::Options opt;
  opt.clone = false;		// Christian's advice
  opt.stop = relaxedStop;

  RelaxedModel * r_base = (RelaxedModel *) base->clone();
  r_base->post_active_operation_branching(O);

  DFS<RelaxedModel> e(r_base, opt);

  (void)P;

  // Find labellings
  while(RelaxedModel * r = e.next()) {
    vector<int> lab;

    if(exists_i_labeling(r, O)) {
      for(operation o : O)
	lab.push_back(r->a(o).val());

      result.labelings.push_back(lab);
    }
    delete r;
  }

  delete relaxedStop;

  // Set status messages
  if (e.stopped())
    result.timeout_status = RELAXED_TIMEOUT;

  else {
    result.timeout_status = RELAXED_NO_TIMEOUT;
    result.solution_status = RELAXED_SOLUTION_FOUND;
  }

  return result;
}

bool exists_i_labeling(RelaxedModel * base, vector<operation> O) {
  RelaxedModel * r_base = (RelaxedModel *) base->clone();

  r_base->post_instruction_operation_branching(O);

  Search::Options opt;
  opt.clone = false;		// Christian's advice
  DFS<RelaxedModel> e(r_base, opt);

  if(RelaxedModel * r = e.next()) {
    delete r;
    return true;
  }

  else
    return false;
}

bool propagate(Parameters& input) {
  ModelOptions options;
  CompleteModel * m = new CompleteModel(&input, &options, IPL_DOM);
  m->post_standalone_constraints();
  Gecode::SpaceStatus ss = m->status();
  assert(ss != SS_FAILED); // At this point the problem should be solvable
  m->post_upper_bound(input.maxf);
  Gecode::SpaceStatus ss1 = m->status();
  if (ss1 == SS_FAILED) {
    return false;
  } else  {
    assert(ss1 != SS_FAILED);
    for (operand p : input.P) {
      vector<temporary> temps;
      for (IntVarValues yi(m->y(p)); yi(); ++yi) {
        temporary t = input.temps[p][yi.val()];
        temps.push_back(t);
      }
      input.temps[p] = temps;
      if (temps.size() == 1 && temps[0] == -1) {
        disconnect_operand(input, p);
      }
    }
    input.compute_derived();
    return true;
  }
}

void disconnect_operand(Parameters& input, operand p) {
  if (!input.global_operand[p]) {
    // Make the operation of p inactive
    operation o = input.oper[p];
    input.instructions[o] = {NULL_INSTRUCTION};
    for (operand q : input.operands[o]) {
      input.temps[q] = {NULL_TEMPORARY};
    }
    input.rclass[o] = {input.rclass[o][0]};
    input.lat[o] = {input.lat[o][0]};
    // TODO: update 'dist'
  }
}

void populate_r_domain(Model * m, temporary t, vector<int>& domain) {
  domain.clear();
  for (IntVarValues rr(m->r(t)); rr(); ++rr)
    domain.push_back(rr.val());
}
