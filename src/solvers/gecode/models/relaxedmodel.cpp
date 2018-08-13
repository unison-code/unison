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
BoolVar RelaxedModel::relaxed_adhoc_constraint_var(UnisonConstraintExpr & e) {
  BoolVar v(*this, 0, 1);
  switch (e.id) {
  case OR_EXPR:
  case AND_EXPR:
    {
      BoolVarArgs vs;
      for (UnisonConstraintExpr e0 : e.children)
        vs << relaxed_adhoc_constraint_var(e0);
      rel(*this, e.id == OR_EXPR ? BOT_OR : BOT_AND, vs, v, ipl);
    }
    return v;
  case XOR_EXPR:
  case IMPLIES_EXPR:
    rel(*this,
        relaxed_adhoc_constraint_var(e.children[0]),
        e.id == XOR_EXPR ? BOT_XOR : BOT_IMP,
        relaxed_adhoc_constraint_var(e.children[1]),
        v,
        ipl);
    return v;
  case NOT_EXPR:
    rel(*this, relaxed_adhoc_constraint_var(e.children[0]), IRT_NQ, v);
    return v;
  case ACTIVE_EXPR:
    return a(e.data[0]);
  case CONNECTS_EXPR:
    return u(e.data[0], e.data[1]);
  case IMPLEMENTS_EXPR:
    return imp(e.data[0], e.data[1]);
  case SHARE_EXPR:
    // This is fine because the temps of one will always be a prefix of the
    // temps of the other
    return var(y(e.data[0]) == y(e.data[1]));
  case DISTANCE_EXPR:
  case OPERAND_OVERLAP_EXPR:
  case TEMPORARY_OVERLAP_EXPR:
    return BoolVar(*this, 0, 1);
  case CALLER_SAVED_EXPR:
    {
      IntArgs cs(input->callersaved);
      // TODO: this is correct, but should include temporaries wider than 1
      dom(*this, r(e.data[0]), IntSet(cs), v);
    }
    return v;
  case ALLOCATED_EXPR:
    {
      IntArgs cs(input->atoms[e.data[1]]);
      dom(*this, ry(e.data[0]), IntSet(cs), v);
    }
    return v;
  case ALIGNED_EXPR:
    return var(ry(e.data[1]) == (ry(e.data[0]) + e.data[2]));
  default:
    GECODE_NEVER;
  }
}


RelaxedModel::RelaxedModel(Parameters * p_input, ModelOptions * p_options,
                         IntPropLevel p_ipl) :
  Model(p_input, p_options, p_ipl)
{
  // Variables
  v_r   = int_var_array(T().size(), -1, input->RA.size() - 1);
  v_i   = int_var_array(O().size(), 0, input->I.size() - 1);
  if (!P().empty()) {
    v_y   = int_var_array(P().size(), 0, input->T.size() - 1);
  }
  v_x   = bool_var_array(sum_of(input->n_global_optionals), 0, 1);
  v_ry  = int_var_array(P().size(), -1, input->RA.size() - 1);
  v_a   = bool_var_array(O().size(), 0, 1);
  v_al  = bool_var_array(T().size() * input->RS.size(), 0, 1);
  v_u   = bool_var_array(input->nu, 0, 1);
  v_us  = int_var_array(T().size(), 0, O().size());

  // Constraint posters
  post_relaxed_decision_variable_domain_definitions();
  post_relaxed_secondary_variable_definitions();
  post_core_constraints();
  post_instruction_constraints();
}

RelaxedModel::RelaxedModel(RelaxedModel& cg) :
  Model(cg){}

RelaxedModel* RelaxedModel::copy(void) {
  return new RelaxedModel(*this);
}

void RelaxedModel::post_relaxed_decision_variable_domain_definitions(void) {
  for(block b : input->B) {
    Model::post_instruction_domains(b);
    Model::post_temporary_domains(b);
  }
}

void RelaxedModel::post_relaxed_secondary_variable_definitions(void) {
  for(block b : input->B) {
    Model::post_operand_register_definition(b);
    Model::post_connected_operand_definition(b);
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
    Model::post_packing_constraints(b);
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
  for(UnisonConstraintExpr nogood : input->nogoods)
    constraint(!relaxed_adhoc_constraint_var(nogood));
}

/*********************************************************************************
 * Branching
 ********************************************************************************/
void RelaxedModel::post_active_operation_branching(vector<operation> O) {
  BoolVarArgs as;
  for(operation o : O)
    as << a(o);
  branch(*this, as, BOOL_VAR_AFC_MAX(), BOOL_VAL_MIN());
}

void RelaxedModel::post_operand_temporary_branching(vector<operand> P) {
  IntVarArgs ts;
  for(operand p : P)
    ts << y(p);
  branch(*this, ts, INT_VAR_AFC_MAX(),INT_VALUES_MIN());
}

void RelaxedModel::post_instruction_operation_branching(vector<operation> O) {
  IntVarArgs is;
  for(operation o : O)
    is << i(o);
  branch(*this, is, INT_VAR_AFC_MAX(), INT_VALUES_MIN());
}
