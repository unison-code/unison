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


#include "localmodel.hpp"

vector<temporary> & LocalModel::T() const { return input->tmp[b]; }

vector<operation> & LocalModel::O() const { return input->ops[b]; }

vector<operand> & LocalModel::P() const { return input->ope[b]; }

BoolVar LocalModel::x(operand p) const {
  if (input->global_optional[p]) {
    return v_x[input->global_optional_index[p] -
               input->first_global_optional_index[b]];
  } else {
    return a(input->oper[p]);
  }
}

BoolVar LocalModel::u(operand p, temporary t) const {
  unsigned int ti = input->temporary_index[p][t];
  return v_u[input->fu[p] - input->bfu[b] + ti];
}

IntVar LocalModel::lat(operand p, temporary t) const {
  unsigned int ti = input->temporary_index[p][t];
  return v_lat[input->fu[p] - input->bfu[b] + ti];
}

BoolVar LocalModel::p(operation o1, operation o2) {
  if (options->disable_precedence_variables()) {
    return var(c(o1) < c(o2));
  } else {
    unsigned int o1i = input->mandatory_index[o1],
                 o2i = input->mandatory_index[o2];
    return v_p[o1i * input->mandatory[b].size() + o2i];
  }
}

IntVar LocalModel::s(operand p) const {
  assert(input->global_operand[p]);
  return v_s[input->global_index[p] - input->first_global_index[b]];
}

LocalModel::LocalModel(Parameters * p_input, ModelOptions * p_options,
                       IntPropLevel p_ipl,
                       const GlobalModel * gs, block p_b) :
  Model(p_input, p_options, p_ipl),
  b(p_b)
{
  v_r   = int_var_array(T().size(), -1, input->RA.size() - 1);
  v_i   = int_var_array(O().size(), 0, input->I.size() - 1);
  v_c   = int_var_array(O().size(), 0, input->maxc[b]);
  if (!P().empty()) {
    v_y  = int_var_array(P().size(), 0, input->T.size() - 1);
  }
  v_x   = bool_var_array(input->n_global_optionals[b], 0, 1);
  v_ry  = int_var_array(P().size(), -1, input->RA.size() - 1);
  v_a   = bool_var_array(O().size(), 0, 1);
  v_ls  = int_var_array(T().size(), 0, input->maxc[b]);
  v_ld  = int_var_array(T().size(), 0, input->maxc[b]);
  v_le  = int_var_array(T().size(), 0,
                        input->maxc[b] + maybe_max_of(0, input->minlive));
  v_al  = bool_var_array(T().size() * input->RS.size(), 0, 1);
  v_u   = bool_var_array(input->bnu[b], 0, 1);
  v_us  = int_var_array(T().size(), 0, O().size());
  if (!P().empty()) {
    v_lt = int_var_array(P().size(), input->min_lat, input->max_lat);
  }
  if (input->bnu[b] > 0) {
    v_lat = int_var_array(input->bnu[b], -input->max_lat, input->max_lat * 3);
  }
  if (!T().empty()) {
    v_users = set_var_array(T().size(), IntSet::empty,
                            IntSet(min_of(input->ope[b]),
                                   max_of(input->ope[b])));
  }
  if (!P().empty()) {
    v_s = int_var_array(input->n_global[b], -input->max_lat, input->max_lat);
  }
  v_f = int_var_array(input->N, 0, Int::Limits::max);
  if (!options->disable_precedence_variables()) {
    v_p = bool_var_array(input->mandatory[b].size() *
                         input->mandatory[b].size(), 0, 1);
  }

  c_activity = IntAction(*this, v_c, 0.99);

  // Apply solution from the global model
  apply_solution(gs);

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

  // Local cost
  post_cost_definition();

  // Symmetry breaking (depends on the result of propagating all other
  // constraints)
  if (!options->disable_improving()) {
    Gecode::SpaceStatus ss = status();
    if (ss == SS_FAILED) return;
    if (!options->disable_operand_symmetry_breaking_constraints())
      post_operand_symmetry_breaking_constraints(b);
    ss = status();
    if (ss == SS_FAILED) return;
    if (!options->disable_register_symmetry_breaking_constraints())
      post_register_symmetry_breaking_constraints(b);
  }

}

LocalModel::LocalModel(LocalModel& cg) :
  Model(cg),
  b(cg.b),
  c_activity(cg.c_activity)
{
  v_f.update(*this, cg.v_f);
}

LocalModel* LocalModel::copy(void) {
  return new LocalModel(*this);
}

void LocalModel::post_decision_variable_domain_definitions(void) {
  Model::post_decision_variable_domain_definitions(b);
}

void LocalModel::post_secondary_variable_definitions(void) {
  Model::post_secondary_variable_definitions(b);
}

void LocalModel::post_basic_model_constraints(void) {
  Model::post_basic_model_constraints(b);
}



void LocalModel::post_improved_model_constraints(void) {
  Model::post_improved_model_constraints(b);
}

void LocalModel::post_presolver_constraints(void) {
  Model::post_presolver_constraints(b);
}

void LocalModel::post_before_scheduling_constraints_in_space(Space& s) {
  LocalModel& m = static_cast<LocalModel&>(s);
  m.post_before_scheduling_constraints(m.b);
}

void LocalModel::post_cost_definition(void) {
  Model::post_cost_definition(b);
}

void LocalModel::post_operand_symmetry_breaking_constraints(block b) {

  // An operand can only be connected to a certain temporary when all its
  // interchangeable operands with lower index within the same operation
  // are connected to a lower temporary:

  for (operation o : input->ops[b]) {
    map<set<temporary>, set<operand> > interchangeable;
    for (operand p : input->operands[o])
      if (input->use[p]) {
       set<temporary> ts;
        for (IntVarValues ti(y(p)); ti(); ++ti)
          ts.insert(input->temps[p][ti.val()]);
        interchangeable[ts].insert(p);
      }
    for (map<set<temporary>, set<operand> >::iterator it =
           interchangeable.begin(); it != interchangeable.end(); ++it)
      if (it->first.size() > 1 && it->second.size() > 1) {
        IntVarArgs yps;
        for (operand p : it->second) yps << y(p);
        rel(*this, yps, IRT_LQ, ipl);
      }
  }

}


void LocalModel::post_register_symmetry_breaking_constraints(block b) {
  vector<PresolverValuePrecedeChain> chains = value_precede_chains(*input, this, false, b);
  for (const PresolverValuePrecedeChain& vpc : chains) {
    IntVarArgs rts;
    for (temporary t : vpc.ts) {
      rts << r(t);
      for (int w = 1; w < input->width[t]; w++) {
	IntVar rtw(*this, r(t).min() + w, r(t).max() + w);
	constraint(rtw == r(t) + w);
	assert_bounded(rtw);
	rts << rtw;
      }
    }
    for (vector<register_atom> rs : vpc.rss) {
      IntArgs ras(rs.begin(), rs.end());
      precede(*this, rts, ras);
      assert(status() != SS_FAILED);
    }
  }
}


void LocalModel::constrain_cost(IntRelType irt, int cost) {
  rel(*this, f(b, 0), irt, cost, ipl);
}

void LocalModel::post_branchers(char search) {
  switch (search) {
  case AGGRESSIVE_SEARCH:
    post_aggressive_branchers();
    break;
  case TRIVIAL_SEARCH:
    post_trivial_branchers();
    break;
  case MINIMUM_COST_SEARCH:
    post_minimum_cost_branchers();
    break;
  case FAIL_FIRST_SEARCH:
    post_fail_first_branchers();
    break;
  case CONSERVATIVE_SEARCH:
    post_conservative_branchers();
    break;
  default:
    GECODE_NEVER;
  }
}

void LocalModel::post_aggressive_branchers(void) {

  post_routing_branchers(true);

  branch(*this, &LocalModel::post_before_scheduling_constraints_in_space);

  branch_on_pressure_scheduling(*this, v_c);

  branch(*this, v_r, INT_VAR_SIZE_MIN(), INT_VAL_MIN(),
         &assignable, &print_register_decision);

}

void LocalModel::post_trivial_branchers(void) {

  branch(*this, v_a, BOOL_VAR_ACTION_MAX(c_activity), BOOL_VAL_MIN(),
         NULL, &print_inactive_decision);

  branch(*this, v_i, INT_VAR_NONE(), INT_VAL_MIN(),
         NULL, &print_instruction_decision);

  IntVarArgs ts;
  for (operand p : input->groupcopyrel[b]) ts << y(p);
  branch(*this, ts, INT_VAR_NONE(), INT_VAL_MIN(),
         NULL, &print_temporary_decision);

  branch(*this, &LocalModel::post_before_scheduling_constraints_in_space);

  branch(*this, v_c, INT_VAR_MIN_MIN(), INT_VAL_MIN(),
         &schedulable, &print_cycle_decision);

  branch(*this, v_r, INT_VAR_SIZE_MIN(), INT_VAL_MIN(), &assignable,
         &print_register_decision);

}

void LocalModel::post_minimum_cost_branchers(void) {

  branch(*this, cost(), INT_VAR_NONE(), INT_VAL_MIN(),
         NULL, &print_cost_decision);

  branch(*this, v_a, BOOL_VAR_ACTION_MAX(c_activity), BOOL_VAL_MIN(),
         NULL, &print_inactive_decision);

  branch(*this, v_i, INT_VAR_NONE(), INT_VAL_MIN(),
         NULL, &print_instruction_decision);

  IntVarArgs ts;
  for (operand p : input->groupcopyrel[b]) ts << y(p);
  branch(*this, ts, INT_VAR_NONE(), INT_VAL_MIN(),
         NULL, &print_temporary_decision);

  branch(*this, &LocalModel::post_before_scheduling_constraints_in_space);

  branch(*this, v_c, INT_VAR_MIN_MIN(), INT_VAL_MIN(),
         &schedulable, &print_cycle_decision);

  branch(*this, v_r, INT_VAR_SIZE_MIN(), INT_VAL_MIN(), &assignable,
         &print_register_decision);

}

void LocalModel::post_fail_first_branchers(void) {

  branch(*this, v_a, BOOL_VAR_ACTION_MAX(c_activity), BOOL_VAL_MIN(),
         NULL, &print_inactive_decision);

  branch(*this, v_i, INT_VAR_NONE(), INT_VAL_MAX(),
         NULL, &print_instruction_decision);

  IntVarArgs ts;
  for (operand p : input->groupcopyrel[b]) ts << y(p);
  branch(*this, ts, INT_VAR_NONE(), INT_VAL_MIN(),
         NULL, &print_temporary_decision);

  branch(*this, &LocalModel::post_before_scheduling_constraints_in_space);

  branch(*this, v_c, INT_VAR_SIZE_MIN(), INT_VAL_MIN(),
         &schedulable, &print_cycle_decision);

  branch(*this, v_r, INT_VAR_SIZE_MIN(), INT_VAL_MIN(), &assignable,
         &print_register_decision);

}

void LocalModel::post_conservative_branchers(void) {

  post_routing_branchers(false);

  branch(*this, &LocalModel::post_before_scheduling_constraints_in_space);

  branch_on_pressure_scheduling(*this, v_c);

  branch(*this, v_r, INT_VAR_SIZE_MIN(), INT_VAL_MIN(), &assignable,
         &print_register_decision);

}

void LocalModel::post_routing_branchers(bool aggressive) {

  for (vector<operand> ps : input->copyrel) {

    if (input->pb[ps[0]] != b) continue;

    set<operation> is0;
    for (operand p : ps) {
      operation o = input->oper[p];
      if (is_optional(o)) is0.insert(o);
    }
    vector<operation> os(is0.begin(), is0.end());

    BoolVarArgs as;
    IntVarArgs is;
    for (operation o : os) {
      as << a(o);
      is << i(o);
    }

    IntVarArgs ts;
    for (operand p : ps) ts << y(p);

    branch_on_routing(*this, as, is, ts, aggressive, os, ps);

  }

  // Select instructions for natural operations when necessary

  branch(*this, v_i, INT_VAR_NONE(), INT_VAL_MIN(),
         NULL, &print_instruction_decision);

}

bool LocalModel::master(const MetaInfo& mi) {
  assert(mi.type() == MetaInfo::PORTFOLIO);
  return true;
}

bool LocalModel::slave(const MetaInfo& mi) {
  assert(mi.type() == MetaInfo::PORTFOLIO);
  string portfolio = options->local_portfolio();
  assert(mi.asset() < portfolio.size());
  char search = portfolio[mi.asset()];
  post_branchers(search);
  return true;
}

IntVarArgs LocalModel::cost() const {
  return v_f;
}

void LocalModel::print(ostream & pOs) const {

  pOs << "estimated cost: " << f(b, 0);

  pOs << endl << endl;

  Model::print(pOs, b);

  pOs << endl << endl;

}

void LocalModel::apply_solution(const GlobalModel * gs) {

  for (operation o : input->ops[b]) {
    copy_domain(*this, gs->i(o), i(o));
    copy_domain(*this, gs->c(o), c(o));
    copy_domain(*this, gs->a(o), a(o));
  }

  for (operand p : input->ope[b]) {
    copy_domain(*this, gs->ry(p), ry(p));
    copy_domain(*this, gs->y(p), y(p));
    if (input->global_operand[p]) {
      copy_domain(*this, gs->s(p), s(p));
    }
  }

  for (temporary t : input->tmp[b]) {
    copy_domain(*this, gs->r(t), r(t));
    copy_domain(*this, gs->l(t), l(t));
    copy_domain(*this, gs->ls(t), ls(t));
    copy_domain(*this, gs->ld(t), ld(t));
    copy_domain(*this, gs->le(t), le(t));
  }

  for (unsigned int n = 0; n < input->N; n++) {
    copy_domain(*this, gs->f(b, n), f(b, n));
  }

}

bool LocalModel::equal_to(const LocalModel * ls) const {
  for (operation o : {input->in[b], input->out[b]})
    for (operand p : input->operands[o]) {
      if (ry(p).val() != ls->ry(p).val()) return false;
      if (input->global_operand[p]) {
        if (s(p).val() != ls->s(p).val()) return false;
      }
    }
  for (activation_class ac : input->AC)
    for (operation o : input->activation_class_operations[ac])
      if (contains(input->ops[b], o))
        if (a(o).val() != ls->a(o).val()) return false;
  return true;
}

string LocalModel::emit_json() const {
  vector<vector<int> > a1, i1, l1, r1, y1, c1, ls1, le1;
  for (operation o : input->ops[b]) {
    a1.push_back({a(o).min(), a(o).max()});
    i1.push_back({i(o).min(), i(o).max()});
    c1.push_back({c(o).min(), c(o).max()});
  }
  for (operand p : input->ope[b]) {
    y1.push_back({y(p).min(), y(p).max()});
  }
  for (temporary t : input->tmp[b]) {
    l1.push_back({l(t).min(), l(t).max()});
    r1.push_back({r(t).min(), r(t).max()});
    ls1.push_back({ls(t).min(), ls(t).max()});
    le1.push_back({le(t).min(), le(t).max()});
  }
  stringstream json;
  json << emit_json_line("a", a1);
  json << emit_json_line("i", i1);
  json << emit_json_line("l", l1);
  json << emit_json_line("r", r1);
  json << emit_json_line("y", y1);
  json << emit_json_line("c", c1);
  json << emit_json_line("ls", ls1);
  json << emit_json_line("le", le1);
  return json.str();
}
