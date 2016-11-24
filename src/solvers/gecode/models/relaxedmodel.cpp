/*
 *  Main authors:
 *    Mikael Almgren <mialmg@kth.se>
 *    Roberto Castaneda Lozano <rcas@sics.se>
 *    Mats Carlsson <matsc@sics.se>
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

#include "relaxedmodel.hpp"

/*********************************************************************************
 * Model
 ********************************************************************************/
vector<temporary> & RelaxedModel::T() const { return input->T; }

vector<operation> & RelaxedModel::O() const { return input->O; }

vector<operand> & RelaxedModel::P() const { return input->P; }

BoolVar RelaxedModel::x(operand p) const {
  if (input->global_optional[p]) {
    return v_x[input->global_optional_index[p]];
  } else {
    return a(input->oper[p]);
  }
}

BoolVar RelaxedModel::u(operand p, temporary t) const {
  assert(input->use[p]);
  unsigned int ti = input->temporary_index[p][t];
  return v_u[input->fu[p] + ti];
}

IntVar RelaxedModel::lat(operand p, temporary t) const {
  assert(input->use[p]);
  unsigned int ti = input->temporary_index[p][t];
  return v_lat[input->fu[p] + ti];
}

BoolVar RelaxedModel::p(operation, operation) {
  GECODE_NEVER;
}

IntVar RelaxedModel::s(operand) const {
  GECODE_NEVER;
}

// Based on the function from model.cpp. This version ignores
// PRESOLVER_OVERLAPPING_OPERANDS and PRESOLVER_OVERLAPPING_TEMPORARIES
// by just returning a BoolVar in 0..1
BoolVar RelaxedModel::relaxed_presolver_conj_var(presolver_conj c) {
  BoolVar conj(*this, 0, 1);
  BoolVarArgs lits;

  for(presolver_lit l : c)
    lits << relaxed_presolver_lit_var(l);

  rel(*this, BOT_AND, lits, conj);
  return conj;
}

BoolVar RelaxedModel::relaxed_presolver_lit_var(presolver_lit l) {
  if (l[0] == PRESOLVER_EQUAL_TEMPORARIES) {
    operand p = l[1];
    operand q = l[2];
    return var(y(p) == y(q));
  } else if (l[0] == PRESOLVER_OPERAND_TEMPORARY) {
    operand p = l[1];
    temporary t = l[2];
    return u(p, t);
  } else if (l[0] == PRESOLVER_ACTIVENESS) {
    operation o = l[1];
    return a(o);
  } else if (l[0] == PRESOLVER_OPERATION) {
    operation o = l[1];
    instruction in = l[2];
    unsigned int ii = find_index(input->instructions[o], in);
    return var(i(o) == ii);
  } else if (l[0] == PRESOLVER_CALLER_SAVED_TEMPORARY) {
    temporary t = l[1];
    IntArgs cs(input->callersaved);
    BoolVar tcs(*this, 0, 1);
    // TODO: this is correct, but should consider also temporaries wider than 1
    dom(*this, r(t), IntSet(cs), tcs);
    return tcs;
  } else if (l[0] == PRESOLVER_NO_OPERATION) {
    operation o = l[1];
    instruction in = l[2];
    unsigned int ii = find_index(input->instructions[o], in);
    return var(i(o) != ii);
  } else if (l[0] == PRESOLVER_OPERAND_CLASS) {
    operand p = l[1];
    register_class c = l[2];
    IntArgs cs(input->atoms[c]);
    BoolVar toc(*this, 0, 1);
    dom(*this, ry(p), IntSet(cs), toc);
    return toc;
  } else {
    return BoolVar(*this, 0, 1);
  }
}

RelaxedModel::RelaxedModel(Parameters * p_input, ModelOptions * p_options,
                         IntPropLevel p_ipl) :
  Model(p_input, p_options, p_ipl)
{
  // Variables
  v_r   = int_var_array(T().size(), -1, input->RA.size() - 1);
  v_i   = int_var_array(O().size(), 0, input->I.size() - 1);
  v_y   = int_var_array(P().size(), 0, input->T.size() - 1);
  v_x   = bool_var_array(sum_of(input->n_global_optionals), 0, 1);
  v_ry  = int_var_array(P().size(), -1, input->RA.size() - 1);
  v_a   = bool_var_array(O().size(), 0, 1);
  v_al  = bool_var_array(T().size() * input->RS.size(), 0, 1);
  v_u   = bool_var_array(input->nu, 0, 1);
  v_us  = int_var_array(T().size(), 0, O().size());

  v_gf   = IntVar(*this, 0, Int::Limits::max);
  v_f    = int_var_array(input->B.size(), 0, Int::Limits::max);

  // Constraint posters
  post_relaxed_decision_variable_domain_definitions();
  post_relaxed_secondary_variable_definitions();
  post_core_constraints();
  post_instruction_constraints();
}

RelaxedModel::RelaxedModel(bool share, RelaxedModel& cg) :
  Model(share, cg){}

RelaxedModel* RelaxedModel::copy(bool share) {
  return new RelaxedModel(share, *this);
}

// Not used but needed since inherited from Model
IntVar RelaxedModel::cost(void) const { return gf(); }

void RelaxedModel::post_relaxed_decision_variable_domain_definitions(void) {
  for(block b : input->B) {
    Model::post_instruction_domains(b);
    Model::post_temporary_domains(b);
  }
}

void RelaxedModel::post_relaxed_secondary_variable_definitions(void) {
  for(block b : input->B) {
    Model::post_operand_register_definition(b);
    Model::post_allocation_definition(b);
    Model::post_use_temporary_definition(b);
    Model::post_temporary_uses_definition(b);
  }
}

void RelaxedModel::post_core_constraints(void) {
  for(block b : input->B) {
    Model::post_null_register_constraints(b);
    Model::post_connected_users_constraints(b);
    Model::post_effective_copy_constraints(b);
    // Model::post_local_congruence_constraints(b); // [MC]
    Model::post_alignment_constraints(b);
    Model::post_preassignment_constraints(b);
    Model::post_temporary_symmetry_breaking_constraints(b);
  }

  // CompleteModel::post_global_operand_connection_constraints(); // [MC]
  // Global operands are connected iff any of their successors is connected:
  for (auto pqs : input->succ) {
    operand p = pqs.first;
    BoolVarArgs xqs;
    for (operand q : pqs.second) xqs << x(q);
    constraint(x(p) == (sum(xqs) > 0));
  }
  for (vector<operand> adj : input->quasi_adjacent) {
    operand p = adj[0], q = adj[1];
    constraint(x(q) >> x(p));
  }
  // CompleteModel::post_congruence_constraints();		       // [MC]
  // Connected adjacent operands are assigned to the same register:
  for (vector<operand> adj : input->adjacent) {
    operand p = adj[0], q = adj[1];
    constraint(ry(q) == ite(x(q), ry(p), NULL_REGISTER));
  }
  // CompleteModel::post_activation_constraints(); // [MC]
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

  post_relaxed_nogood_constraints();

  for(block b : input->B) {
    Model::post_dominates_constraints(b);
    Model::post_difftemps_constraints(b);
    Model::post_diffregs_constraints(b);
  }
}

void RelaxedModel::post_instruction_constraints(void) {
  for(block b : input->B) {
    Model::post_active_instructions_constraints(b);
    Model::post_mandatory_reuse_constraints(b);
    Model::post_register_class_constraints(b);
  }
}
/*********************************************************************************
 * Core constraints
 ********************************************************************************/

void RelaxedModel::post_relaxed_nogood_constraints(void) {
  for(presolver_conj nogood : input->nogoods)
    if (!nogood.empty())
      constraint(!relaxed_presolver_conj_var(nogood));
}

/*********************************************************************************
 * Branching
 ********************************************************************************/
void RelaxedModel::post_active_operation_branching(vector<operation> O) {
  BoolVarArgs as;
  for(operation o : O)
    as << a(o);
  branch(*this, as, INT_VAR_NONE(), INT_VAL_MIN());
}

void RelaxedModel::post_operand_temporary_branching(vector<operand> P) {
  IntVarArgs ts;
  for(operand p : P)
    ts << y(p);
  branch(*this, ts, INT_VAR_DEGREE_MAX(),INT_VAL_MED());
}

void RelaxedModel::post_instruction_operation_branching(vector<operation> O) {
  IntVarArgs is;
  for(operation o : O)
    is << i(o);
  branch(*this, is, INT_VAR_DEGREE_MAX(), INT_VAL_MED());
}
