/*
 *  Main authors:
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
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


#include "completemodel.hpp"

vector<temporary> & CompleteModel::T() const { return input->T; }

vector<operation> & CompleteModel::O() const { return input->O; }

vector<operand> & CompleteModel::P() const { return input->P; }

BoolVar CompleteModel::x(operand p) const {
  if (input->global_optional[p]) {
    return v_x[input->global_optional_index[p]];
  } else {
    return a(input->oper[p]);
  }
}

BoolVar CompleteModel::u(operand p, temporary t) const {
  unsigned int ti = input->temporary_index[p][t];
  return v_u[input->fu[p] + ti];
}

IntVar CompleteModel::lat(operand p, temporary t) const {
  unsigned int ti = input->temporary_index[p][t];
  return v_lat[input->fu[p] + ti];
}

BoolVar CompleteModel::p(operation o1, operation o2) {
  if (options->disable_precedence_variables()) {
    return var(c(o1) < c(o2));
  } else {
    block b = input->oblock[o1];
    unsigned int o1i = input->mandatory_index[o1],
                 o2i = input->mandatory_index[o2];
    return v_p[input->accman[b] + o1i * input->mandatory[b].size() + o2i];
  }
}

IntVar CompleteModel::s(operand p) const {
  assert(input->global_operand[p]);
  return v_s[input->global_index[p]];
}

CompleteModel::CompleteModel(Parameters * p_input, ModelOptions * p_options,
                         IntPropLevel p_ipl) :
  Model(p_input, p_options, p_ipl)
{
  v_r   = int_var_array(T().size(), -1, input->RA.size() - 1);
  v_i   = int_var_array(O().size(), 0, input->I.size() - 1);
  v_c   = int_var_array(O().size(), 0, max_of(input->maxc));
  if (!P().empty()) {
    v_y = int_var_array(P().size(), 0, input->T.size() - 1);
  }
  v_x   = bool_var_array(sum_of(input->n_global_optionals), 0, 1);
  v_ry  = int_var_array(P().size(), -1, input->RA.size() - 1);
  v_a   = bool_var_array(O().size(), 0, 1);
  v_ls  = int_var_array(T().size(), 0, max_of(input->maxc));
  v_ld  = int_var_array(T().size(), 0, max_of(input->maxc));
  v_le  = int_var_array(T().size(), 0,
                        max_of(input->maxc) + maybe_max_of(0, input->minlive));
  v_al  = bool_var_array(T().size() * input->RS.size(), 0, 1);
  v_u   = bool_var_array(input->nu, 0, 1);
  v_us  = int_var_array(T().size(), 0, O().size());
  if (!P().empty()) {
    v_lt = int_var_array(P().size(), input->min_lat, input->max_lat);
  }
  if (input->nu > 0) {
    v_lat = int_var_array(input->nu, -input->max_lat, input->max_lat * 3);
  }
  if (!options->disable_precedence_variables()) {
    v_p   = bool_var_array(input->accman[input->B.size()], 0, 1);
  }
  if (!T().empty()) {
    v_users = set_var_array(T().size(), IntSet::empty,
                            IntSet(min_of(input->P), max_of(input->P)));
  }
  if (!P().empty()) {
    v_s = int_var_array(sum_of(input->n_global),
                        -input->max_lat, input->max_lat);
  }
  v_gf = int_var_array(input->N, 0, Int::Limits::max);
  v_f  = int_var_array(input->B.size() * input->N, 0, Int::Limits::max);
}

CompleteModel::CompleteModel(CompleteModel& cg) :
  Model(cg)
{
  v_gf.update(*this, cg.v_gf);
  v_f.update(*this, cg.v_f);
}

CompleteModel* CompleteModel::copy(void) {
  return new CompleteModel(*this);
}

IntVarArgs CompleteModel::cost() const {
  return gf();
}

void CompleteModel::post_decision_variable_domain_definitions(void) {
  for (block b : input->B)
    Model::post_decision_variable_domain_definitions(b);
}

void CompleteModel::post_secondary_variable_definitions(void) {
  for (block b : input->B)
    Model::post_secondary_variable_definitions(b);
}

void CompleteModel::post_basic_model_constraints(void) {

  for (block b : input->B) Model::post_basic_model_constraints(b);
  post_global_operand_connection_constraints();
  post_congruence_constraints();
  post_activation_constraints();
  post_slack_balancing_constraints();

}

void CompleteModel::post_global_operand_connection_constraints(void) {

  // Global operands are connected iff any of their successors is connected:

  for (auto pqs : input->succ) {
    operand p = pqs.first;
    BoolVarArgs xqs;
    for (operand q : pqs.second) xqs << x(q);
    constraint(x(p) == (sum(xqs) > 0));
  }
  // [MC]
  for (vector<operand> adj : input->quasi_adjacent) {
    operand p = adj[0], q = adj[1];
    constraint(x(q) >> x(p));
  }
}

void CompleteModel::post_congruence_constraints(void) {

  // Connected adjacent operands are assigned to the same register:

  for (vector<operand> adj : input->adjacent) {
    operand p = adj[0], q = adj[1];
    constraint(ry(q) == ite(x(q), ry(p), NULL_REGISTER));
  }

}

void CompleteModel::post_activation_constraints(void) {

  // An operation is active if any of its activator instructions is selected:

  for (activation_class ac : input->AC) {
    BoolVarArgs is;
    for (instruction i1 : input->activation_class_instructions[ac])
      for (operation o : input->O)
        for (unsigned int ii = 0; ii < input->instructions[o].size(); ii++)
          if (i1 == input->instructions[o][ii]) is << var(i(o) == ii);
    BoolVarArgs as;
    for (operation o : input->activation_class_operations[ac]) as << a(o);
    rel(*this, as, IRT_EQ, ipl);
    constraint((sum(is) > 0) >> a(input->activation_class_representative[ac]));
  }

}

void CompleteModel::post_slack_balancing_constraints(void) {

  // The slack of adjacent operands is balanced:

  for (vector<operand> adj : input->adjacent) {
    operand p = adj[0], q = adj[1];
    constraint((s(p) + s(q)) == 0);
  }

}

void CompleteModel::post_improved_model_constraints(void) {
  for (block b : input->B)
    Model::post_improved_model_constraints(b);
  post_slack_functional_constraints();
}

void CompleteModel::post_slack_functional_constraints(void) {

  int maxc = max_of(input->maxc);
  int maxl = input->max_lat;
  for (vector<vector<int>>& ix : input->long_latency_index) {
    vector<operand> inps = ix[0];
    vector<operand> outps = ix[2];
    vector<int> inix = ix[1];
    vector<int> outix = ix[3];
    IntVarArgs inubs, outubs;
    
    for (unsigned int ii : inix) {
      vector<operand>du = input->long_latency_def_use[ii];
      operand p = du[0];
      operand q = du[1];
      temporary t = input->single_temp[p];
      inubs << var(ite(u(q, t), c(input->oper[q]) - lt(q) - slack(q) - lt(p), maxl));
    }
    for (unsigned int ii : outix) {
      vector<operand>du = input->long_latency_def_use[ii];
      operand p = du[0];
      operand q = du[1];
      temporary t = input->single_temp[p];
      outubs << var(ite(u(q, t), ld(t) - lt(q) - slack(p) - lt(p), maxl));
    }
    IntVar outlb(*this, -maxc, maxc);
    IntVar outub(*this, -maxc, maxc);
    if(!inix.empty())
      constraint(outlb == -min(inubs));
    else
      constraint(outlb == -maxl);
    if(!outix.empty())
      constraint(outub == min(outubs));
    else
      constraint(outub == maxl);
    constraint(outlb <= outub);
    constraint(s(outps[0]) == min(outub,max(outlb,0)));
    for (operand p : outps)
      if (p > outps[0])
	constraint(s(p) == s(outps[0]));
  }
  
}

void CompleteModel::post_presolver_constraints(void) {
  for (block b : input->B)
    Model::post_presolver_constraints(b);
  if (!options->disable_presolver_constraints()) {
    // cross-block synchronisation of spill/unspill ops
    for (const vector<operation>& os : input->calleesaved_spill) {
      operation spillop = os[0];
      for (operation o : os) {
        if (o != spillop) {
          constraint(a(o) == a(spillop)); // cross-block constraint
        }
      }
    }

    // cross-block nogoods
    for (UnisonConstraintExpr ng : input->gnogoods) {
      constraint(!adhoc_constraint_var(ng));
    }

    // cross-block active tables
    for (PresolverActiveTable table : input->gactive_tables) {
      if (table.tuples.empty()) continue;
      BoolVarArgs as;
      for (operation o : table.os) as << a(o);
      TupleSet ts(table.tuples[0].size());
      for (vector<int> tuple : table.tuples) ts.add(IntArgs(tuple));
      ts.finalize();
      extensional(*this, as, ts);
    }
  }
}

void CompleteModel::post_global_cost_definition(void) {

  // The objective is to minimize the cost of each block possibly weighted by
  // the estimated execution frequency:

  for (unsigned int n = 0; n < input->N; n++) {
    IntVarArgs fs;
    for (block b : input->B) fs << f(b, n);
    if (input->optimize_dynamic[n])
      linear(*this, IntArgs(input->freq), fs, IRT_EQ, gf()[n]);
    else
      linear(*this, fs, IRT_EQ, gf()[n]);
  }
}

void CompleteModel::post_cost_definition(void) {
  for (block b : input->B)
    Model::post_cost_definition(b);
}

void CompleteModel::post_upper_bound(vector<int> maxcost) {
  rel(*this, cost(), IRT_LQ, maxcost);
}

void CompleteModel::post_lower_bound(vector<int> mincost) {
  rel(*this, cost(), IRT_GQ, mincost);
}

void CompleteModel::post_standalone_constraints(void) {

  // Individual domains of problem variables
  post_decision_variable_domain_definitions();

  // Secondary variable definitions
  post_secondary_variable_definitions();

  // Basic model
  post_basic_model_constraints();

  // Improved model
  post_improved_model_constraints();

  // Presolved model
  post_presolver_constraints();

  // Global cost
  post_global_cost_definition();

  // Cost of each block
  post_cost_definition();

}

void CompleteModel::print(ostream & pOs) const {
  pOs << "global cost: " << gf() << endl << endl;
  for (block b : input->B) Model::print(pOs, b);
  pOs << endl;
}

// Returns a JSON string with the solution of the given model, assuming all
// variables are assigned.

string CompleteModel::solution_to_json() const {

  std::stringstream pOs;

  vector<int> rs;
  for (temporary t1 : input->T)
    rs.push_back(l(t1).val() ? r(t1).val() : -1);

  vector<instruction> is;
  for (operation o : input->O)
    is.push_back(input->instructions[o][i(o).val()]);

  vector<int> cs;
  for (operation o : input->O)
    cs.push_back(a(o).val() ? c(o).val() : -1);

  vector<temporary> ys;
  for (operand p : input->P)
    ys.push_back(input->temps[p][y(p).val()]);

  pOs << "{";
  pOs << "\"registers\":" << to_json(rs) << ",";
  pOs << "\"instructions\":" << to_json(is) << ",";
  pOs << "\"cycles\":" << to_json(cs) << ",";
  pOs << "\"temporaries\":" << to_json(ys);
  pOs << "}";

  return pOs.str();

}
