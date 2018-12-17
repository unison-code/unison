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


#include "model.hpp"

IntVarArray Model::int_var_array(int n, int min, int max) {
  n_int_vars += n;
  return IntVarArray(*this, n, min, max);
}

BoolVarArray Model::bool_var_array(int n, int min, int max) {
  n_bool_vars += n;
  return BoolVarArray(*this, n, min, max);
}

SetVarArray
Model::set_var_array(int n, const IntSet & glb, const IntSet & lub) {
  n_set_vars += n;
  return SetVarArray(*this, n, glb, lub);
}

BoolVar Model::adhoc_constraint_var(UnisonConstraintExpr & e) {
  BoolVar v(*this, 0, 1);
  switch (e.id) {
  case OR_EXPR:
  case AND_EXPR:
    {
      BoolVarArgs vs;
      for (UnisonConstraintExpr e0 : e.children)
        vs << adhoc_constraint_var(e0);
      rel(*this, e.id == OR_EXPR ? BOT_OR : BOT_AND, vs, v, ipl);
    }
    return v;
  case XOR_EXPR:
  case IMPLIES_EXPR:
    rel(*this,
        adhoc_constraint_var(e.children[0]),
        e.id == XOR_EXPR ? BOT_XOR : BOT_IMP,
        adhoc_constraint_var(e.children[1]),
        v,
        ipl);
    return v;
  case NOT_EXPR:
    rel(*this, adhoc_constraint_var(e.children[0]), IRT_NQ, v);
    return v;
  case ACTIVE_EXPR:
    return a(e.data[0]);
  case CONNECTS_EXPR:
    return u(e.data[0], e.data[1]);
  case IMPLEMENTS_EXPR:
    return imp(e.data[0], e.data[1]);
  case DISTANCE_EXPR:
    return var(c(e.data[1]) >= (c(e.data[0]) + e.data[2]));
  case SHARE_EXPR:
    // This is fine because the temps of one will always be a prefix of the
    // temps of the other
    return var(y(e.data[0]) == y(e.data[1]));
  case OPERAND_OVERLAP_EXPR:
    return var((pls(e.data[0]) < ple(e.data[1])) &&
               (pls(e.data[1]) < ple(e.data[0])));
  case TEMPORARY_OVERLAP_EXPR:
    return var((ls(e.data[0]) < le(e.data[1])) &&
               (ls(e.data[1]) < le(e.data[0])));
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

IntVar Model::slack(operand p) {
  return input->global_operand[p] ? s(p) : var(0);
}

void Model::distinct(Home home, const IntVarArgs & x, const BoolVarArgs & m,
                     IntPropLevel ipl) {

  int min = Gecode::Int::Limits::max,
      max = Gecode::Int::Limits::min;
  for (int i = 0; i < x.size(); i++) {
    if (x[i].min() < min) min = x[i].min();
    if (x[i].max() > max) max = x[i].max();
  }

  int upper_distance = abs(Gecode::Int::Limits::max - max),
      lower_distance = abs(Gecode::Int::Limits::min - min);
  assert(std::max(upper_distance, lower_distance) >= x.size());

  bool upwards;
  int step;
  if (upper_distance > lower_distance) {
    upwards = true;
    step  = 1;
  } else {
    upwards = false;
    step  = -1;
  }

  int id = upwards ? max + 1 : min - 1;
  IntVarArgs cx;
  for (int i = 0; i < x.size(); i++) {
    IntVar cxi(*this, upwards ? x[i].min() : id, upwards ? id : x[i].max());
    IntVar unique(*this, id, id);
    ite(*this, m[i], x[i], unique, cxi, ipl);
    cx << cxi;
    id += step;
  }

  Gecode::distinct(home, cx, ipl);

}

BoolVar Model::imp(operation o, instruction i0) {
    for (unsigned int ii = 0; ii < input->instructions[o].size(); ii++) {
      if (input->instructions[o][ii] == i0) return var(i(o) == ii);
    }
    GECODE_NEVER;
  }

IntVar Model::pls(operand p) {
  block b = input->pb[p];
  IntVarArgs plss;
  for (temporary t1 : input->temps[p])
    plss << (t1 == NULL_TEMPORARY ? var(input->maxc[b]) : ls(t1));
  return var(element(plss, y(p)));
}

IntVar Model::ple(operand p) {
  IntVarArgs ples;
  for (temporary t1 : input->temps[p])
    ples << (t1 == NULL_TEMPORARY ? zero : le(t1));
  return var(element(ples, y(p)));
}

int Model::objective_domain(void) {
  int d = 0;
  for (block b : input->B)
    d += (input->freq[b] * input->maxc[b]);
  assert(d >= 0 && d <= Int::Limits::max);
  return d;
}

BoolVar Model::partially_in_register_space(temporary t, register_space rs) {
  register_atom fa = input->range[rs][0],
                la = input->range[rs][1];
  // TODO: domain propagation (based on the domain of the r variables) would
  // discard many impossible allocations
  return var((r(t) <= la) && (r(t) + input->width[t] > fa));
}

BoolVar Model::totally_in_register_space(temporary t, register_space rs) {
  register_atom fa = input->range[rs][0],
                la = input->range[rs][1];
  return var((r(t) >= fa) && (r(t) + input->width[t] <= la));
}

bool Model::is_inactive(operation o) const {
  return a(o).assigned() && !a(o).val();
}

bool Model::is_optional(operation o) const {
  return input->instructions[o][0] == NULL_INSTRUCTION;
}

bool Model::is_dead(temporary t) const {
  return l(t).assigned() && !l(t).val();
}

bool Model::is_disconnected(operand p) const {
  return x(p).assigned() && !x(p).val();
}

bool Model::must_connect(operand p) const {
  return (input->temps[p][0] != NULL_TEMPORARY);
}

bool Model::multiple_non_null_temps(operand p) const {
  if (input->temps[p].size() == 1 ||
      (input->temps[p].size() == 2 && !must_connect(p)))
    return false;
  else
    return true;
}

operand Model::src(operation o) const {
  for (operand p : input->operands[o])
    if (input->use[p]) return p;
  GECODE_NEVER;
}

operand Model::dst(operation o) const {
  for (operand p : input->operands[o])
    if (!input->use[p]) return p;
  GECODE_NEVER;
}

operand Model::opposite(operand p) const {
  operation o = input->oper[p];
  return input->use[p] ? dst(o) : src(o);
}

set<operand> Model::single_class(vector<operand> ps) const {
  // Single-class operands do not transitively share any temporary
  set<operand> sps(ps.begin(), ps.end());
  for (unsigned int i = 0; i < ps.size(); i++)
    for (unsigned int j = i + 1; j < ps.size(); j++) {
      operand p = ps[i], q = ps[j];
      if (!disjoint_sets(input->real_temps[p], input->real_temps[q]) ||
          half_congruent(p, q)) {
        sps.erase(p);
        sps.erase(q);
      }
    }
  return sps;
}

bool Model::half_congruent(operand p, operand q) const {
  for (operand p1 : input->related_operands[p])
    for (operand q1 : input->related_operands[q]) {
      operation o1 = input->oper[p1], o2 = input->oper[q1];
      if (o1 == o2)
        if (input->type[o1] == LOW ||
            input->type[o1] == HIGH ||
            input->type[o1] == SPLIT2 ||
            input->type[o1] == SPLIT4 ||
            (input->type[o1] == COMBINE && input->use[p1] != input->use[q1]))
          return true;
    }
  return false;
}

void Model::disjoint_operand_registers(vector<operand> ps) {

  if (ps.size() < 2) return;

  set<operand> sps = single_class(ps);

  BoolVarArgs xs;
  IntVarArgs rs;
  for (operand p : sps) {
    for (int w = 0; w < input->operand_width[p]; w++) {
      xs << x(p);
      // FIXME: "rs << var(ry(p) + w)" sometimes returns an unbounded variable
      // during presolving!
      IntVar rypw(*this, ry(p).min() + w, ry(p).max() + w);
      constraint(rypw == ry(p) + w);
      assert_bounded(rypw);
      rs << rypw;
    }
  }

  // Post global distinct for all single-class operands
  distinct(*this, rs, xs, ipl);

  // Post pair-wise disjoint constraints for all the other pairs
  for (unsigned int i = 0; i < ps.size(); i++)
    for (unsigned int j = i + 1; j < ps.size(); j++) {
      operand p = ps[i], q = ps[j];
      int wp = input->operand_width[p], wq = input->operand_width[q];
      // Otherwise they are handled by distinct / nooverlap above
      if (!(sps.count(p) && sps.count(q)))
        // Otherwise they are allow to overlap
        if (disjoint_sets(input->real_temps[p], input->real_temps[q]) &&
            !half_congruent(p, q)) {
          if (wp == wq) // Same size: enough with inequality
            constraint((x(p) && x(q)) >> (ry(p) != ry(q)));
          else // More general form of non-overlap
            constraint((x(p) && x(q)) >>
                       ((ry(p) >= ry(q) + wq) ^ (ry(q) >= ry(p) + wp)));
        }
    }

}

double Model::
saturation_likelihood(block b, pair<int,int> C, RangeListIter & A) const {

  int Asize = range_size(A);
  double max_likelihood = 0.0;

  map<temporary, double> in_A;

  int totalw = 0;

  for (temporary t : input->tmp[b]) if (!is_dead(t)) {
      int w = input->width[t];
      IntVarRanges tregs(r(t));
      Region r1;
      RangeListIter tAtoms = extend(r1, tregs, w);
      Inter<RangeListIter, RangeListIter> At(A, tAtoms);
      double t_in_A = (double)range_size(At) / (double)range_size(tAtoms);
      if (t_in_A > numeric_limits<double>::epsilon()) in_A[t] = t_in_A;
      totalw += w;
    }

  // If the width of all temps is smaller than A then no saturation is possible
  if (totalw < Asize) return 0.0;

  map<int, vector<pair<int, double> > > events;
  for (std::pair<const temporary, const double> tp : in_A) {
    temporary t = tp.first;
    events[ls(t).min()].push_back(make_pair(input->width[t], in_A[t]));
    events[le(t).max()].push_back(make_pair(-input->width[t], in_A[t]));
  }

  // Total atoms that are surely allocated to A
  double must_pressure = 0.0,
    // Total atoms that may be allocated to A
    may_pressure = 0.0,
    // Total atoms possibly allocated to A weigthed by likelihood
    weigthed_pressure = 0.0;

  // For each event in C
  for (auto step : events) {
    int c = step.first;
    if (c < C.first) continue;
    else if (c > C.second) break;
    for (pair<int, double> event : step.second) {
      int w = event.first;
      double t_in_A = event.second;
      if (t_in_A >= 0.99) { // ts must be allocated to A
        must_pressure += w;
        weigthed_pressure += w;
        if (must_pressure >= Asize) return 1.0;
      } else { // ts may be allocated to A (lik. t_in_A)
        may_pressure += w;
        weigthed_pressure += t_in_A * w;
      }
    }

    double cycle_likelihood;
    if (must_pressure + may_pressure < Asize) cycle_likelihood = 0.0;
    else {
      cycle_likelihood = weigthed_pressure / (double)Asize;
      if (cycle_likelihood > 0.99) return 1.0;
    }
    if (cycle_likelihood > max_likelihood) max_likelihood = cycle_likelihood;

  }

  return max_likelihood;
}

double Model::pressure_balance(operation o) const {
  double in_pressure = 0.0, out_pressure = 0.0;
  for (unsigned pi = 0; pi < input->operands[o].size(); pi++) {
    operand p = input->operands[o][pi];
    double maxp = 0.0;
    for (IntVarValues ii(i(o)); ii(); ++ii) {
      register_class rc = input->rclass[o][ii.val()][pi];
      double pr = pressure(p, rc);
      if (p > maxp) maxp = pr;
    }
    if (input->use[p])
      in_pressure += maxp;
    else
      out_pressure += maxp;
  }

  return in_pressure - out_pressure;

}

double Model::pressure(operand p, register_class rc) const {
  return (double) input->operand_width[p] / (double) (input->atoms[rc].size());
}

bool Model::may_saturate(block b, register_atom fa, register_atom la) const {
  pair<int,int> C = make_pair(0, c(input->out[b]).max());
  // TODO: there should be a better way
  Singleton rsA(fa, la);
  Region r1;
  NaryUnion A(r1, &rsA, 1);
  return (saturation_likelihood(b, C, A) > 0.01);
}

IntArgs Model::consumption_domain(resource r, vector<operation> & is) const {
  int unit = 0, maxcon = 0;
  for (operation o : is)
    for (instruction i : input->instructions[o]) {
      int con = input->con[i][r];
      unit = gcd(unit, con);
      maxcon += con;
    }
  return IntArgs::create(maxcon / unit + 1, 0, unit);
}


Model::Model(Parameters * p_input, ModelOptions * p_options, IntPropLevel p_ipl) :
  input(p_input),
  options(p_options),
  ipl(p_ipl),
  n_int_vars(0),
  n_bool_vars(0),
  n_set_vars(0),
  zero(*this, 0, 0),
  one(*this, 1, 1) {}

Model::Model(Model& m) :
  IntLexMinimizeSpace(m),
  input(m.input),
  options(m.options),
  ipl(m.ipl)
{
  v_r.update(*this, m.v_r);
  v_i.update(*this, m.v_i);
  v_c.update(*this, m.v_c);
  v_y.update(*this, m.v_y);
  v_x.update(*this, m.v_x);
  v_ry.update(*this, m.v_ry);
  v_a.update(*this, m.v_a);
  v_ls.update(*this, m.v_ls);
  v_ld.update(*this, m.v_ld);
  v_le.update(*this, m.v_le);
  v_al.update(*this, m.v_al);
  v_u.update(*this, m.v_u);
  v_us.update(*this, m.v_us);
  v_lt.update(*this, m.v_lt);
  v_lat.update(*this, m.v_lat);
  v_p.update(*this, m.v_p);
  v_users.update(*this, m.v_users);
  v_s.update(*this, m.v_s);
  zero.update(*this, m.zero);
  one.update(*this, m.one);
}

void Model::post_decision_variable_domain_definitions(block b) {

  post_instruction_domains(b);
  post_issue_cycle_domains(b);
  post_temporary_domains(b);

}

void Model::post_instruction_domains(block b) {

  for (operation o : input->ops[b]) {
    unsigned int ops = input->instructions[o].size();
    constraint(i(o) < ops);
  }

}

void Model::post_issue_cycle_domains(block b) {

  int bmaxc = input->maxc[b];

  for (operation o : input->ops[b]) {
    constraint(c(o) <= bmaxc);
    constraint(c(o) <= c(input->out[b]));
  }

  for (temporary t : input->tmp[b]) {
    constraint(ls(t) <= bmaxc);
    constraint(le(t) <= (bmaxc + input->minlive[t]));
  }

  // in-delimiters always start in cycle 0
  constraint(c(input->in[b]) == 0);

  // the rest of operations start at least in cycle 1
  for (operation o : input->ops[b])
    if (o != input->in[b]) constraint(c(o) >= 1);

}

void Model::post_temporary_domains(block b) {

  for (operation o : input->ops[b])
    for (operand p : input->operands[o]) {
      int max = input->temps[p].size();
      constraint(y(p) < max);
    }

}

void Model::post_secondary_variable_definitions(block b) {

  post_operand_register_definition(b);
  post_live_start_definition(b);
  post_live_duration_definition(b);
  post_live_end_definition(b);
  post_connected_operand_definition(b);
  post_allocation_definition(b);
  post_use_temporary_definition(b);
  post_temporary_uses_definition(b);
  post_operand_latency_definition(b);
  post_temporary_use_latency_definition(b);
  if (!options->disable_precedence_variables()) {
    post_precedence_definition(b);
  }
  post_temporary_users_definition(b);

}

void Model::post_operand_register_definition(block b) {

  // The register of an operand is equal to the register of its selected
  // temporary, if this is not the null temporary

  for (operation o : input->ops[b])
    for (operand p : input->operands[o])
      if (input->use[p]) {
        IntVarArgs rs;
        for (temporary t1 : input->temps[p])
          rs << (t1 == NULL_TEMPORARY ? IntVar(*this, -1, -1) : r(t1));
        constraint(ry(p) == element(rs, y(p)));
      } else {
        constraint(ry(p) == r(input->single_temp[p]));
      }

}

void Model::post_live_start_definition(block b) {

  // The live range of a temporary starts at the issue cycle of its definer:

  for (temporary t : input->tmp[b])
    constraint(ls(t) == c(input->oper[input->definer[t]]));

}

void Model::post_live_duration_definition(block b) {

  // The live range of a temporary t is as long as the distance between its live
  // end and its live start:

  for (temporary t : input->tmp[b])
    constraint(ld(t) == (le(t) - ls(t)));

}

void Model::post_live_end_definition(block b) {

  // The live range of a temporary ends at the last issue cycle of its users:

  for (temporary t : input->tmp[b]) {
    IntVarArgs uc; // Issue cycle of each operation that may use t, if t is used
                   // Minimum live range end,                       otherwise
    for (operand p : input->users[t])
      uc << var(ite(u(p, t), c(input->oper[p]), 0));
    uc << var(ls(t) + input->minlive[t]);
    constraint(le(t) == max(uc));
  }

}

void Model::post_connected_operand_definition(block b) {

  // Operands cannot be connected to null temporaries:

  for (operand p : input->ope[b]) {
    if (must_connect(p))
      constraint(x(p));
    else
      constraint(x(p) == (y(p) > 0));
  }

}

void Model::post_allocation_definition(block b) {

  // al[t][rs] <-> t is allocated to register space rs

  for (temporary t : input->tmp[b])
    for (register_space rs : input->RS)
      constraint(al(rs, t) == partially_in_register_space(t, rs));

}

void Model::post_use_temporary_definition(block b) {

  // u[p][t] <-> operand p is connected to temporary t

  for (operation o : input->ops[b])
    for (operand p : input->operands[o])
      if (input->use[p]) {
        BoolVarArgs us;
        for (temporary t1 : input->temps[p]) us << u(p, t1);
        channel(*this, us, y(p), 0, ipl);
      }

}

void Model::post_temporary_uses_definition(block b) {

  // us[t] == number of times temporary t is used

  for (temporary t : input->tmp[b]) {
    BoolVarArgs uses;
    for (operand p : input->users[t]) uses << u(p, t);
    constraint(us(t) == sum(uses));
  }

}

void Model::post_operand_latency_definition(block b) {

  // lt[p] == latency of operand p

  for (operation o : input->ops[b])
    for (unsigned pi = 0; pi < input->operands[o].size(); pi++) {
      operand p = input->operands[o][pi];
      int samelat = -1;
      for (unsigned int ii = 0; ii < input->instructions[o].size(); ii++) {
        int l = input->lat[o][ii][pi];
    	/*if (input->instructions[o][ii]>0) - can't do that - apparently lt(p) can be used even if i(o) == 0 */ {
    	  if (samelat==l || samelat==-1)
    	    samelat = l;
    	  else
    	    samelat = -2;
    	}
      }
      if (samelat>=0) {
    	constraint(lt(p) == samelat);
      } else {
    	IntArgs lats;
    	for (unsigned int ii = 0; ii < input->instructions[o].size(); ii++)
    	  lats << input->lat[o][ii][pi];
    	constraint(lt(p) == element(lats, i(o)));
      }
    }
}

void Model::post_temporary_use_latency_definition(block b) {

  // lat[p][t] == latency of operand p using temporary t

  for (operation o : input->ops[b])
    for (operand q : input->operands[o])
      if (input->use[q])
        for (temporary t : input->temps[q])
          if (t != NULL_TEMPORARY) {
            operand p = input->definer[t];
            constraint(lat(q, t) == lt(p) + slack(p) + lt(q) + slack(q));
          }

}

void Model::post_precedence_definition(block b) {

  // p[o1][o2] <-> operation o1 precedes operation o2

  for (operation o1 : input->mandatory[b])
    for (operation o2 : input->mandatory[b])
      constraint(p(o1, o2) == (c(o1) < c(o2)));

}

void Model::post_temporary_users_definition(block b) {

  // users[t] == set of use operands connected to t

  for (temporary t : input->tmp[b]) {
    IntArgs ps(input->users[t]);
    constraint(users(t) <= IntSet(ps));
    for (operand p : input->users[t]) {
      constraint(u(p, t) == (users(t) >= singleton(p)));
    }
  }

}

void Model::post_basic_model_constraints(block b) {

  post_connected_users_constraints(b);
  post_active_instructions_constraints(b);
  post_register_class_constraints(b);
  post_disjoint_live_ranges_constraints(b);
  post_preassignment_constraints(b);
  post_alignment_constraints(b);
  post_packing_constraints(b);
  post_extensional_constraints(b);
  post_processor_resources_constraints(b);
  post_initial_precedence_constraints(b);
  post_data_precedences_constraints(b);
  post_fixed_precedences_constraints(b);
  post_prescheduling_constraints(b);
  post_bypassing_constraints(b);
  post_adhoc_constraints(b);

}

void Model::post_connected_users_constraints(block b) {

  // A temporary is live iff it is connected to a user;
  // If it's live, then its register must be among the users' registers,
  // otherwise its register must be -1:

  for (temporary t : input->tmp[b]) {
    constraint(l(t) == (us(t) > 0));
    IntVarArgs ruses;
    for (operand p : input->users[t]) ruses << ry(p);
    operand p = input->definer[t];
    if (!must_connect(p)) ruses << IntVar(*this, -1, -1);
    member(*this, ruses, r(t), ipl);
  }
}

void Model::post_active_instructions_constraints(block b) {

  // Active operations are implemented by non-null instructions:

  for (operation o : input->ops[b])
    if (is_optional(o))
      constraint(a(o) == (i(o) > 0));
    else
      constraint(a(o));

}

void Model::post_register_class_constraints(block b) {

  // The instruction that implements an operation determines the register class
  // to which its operands are allocated:

  for (operation o : input->ops[b])
    for (unsigned pi = 0; pi < input->operands[o].size(); pi++) {
      operand p = input->operands[o][pi];
      IntVarArgs atoms;
      for (unsigned int ii = 0; ii < input->instructions[o].size(); ii++) {
        instruction i = input->instructions[o][ii];
        if (i == NULL_INSTRUCTION) {
          atoms << var(NULL_REGISTER);
        } else {
          register_class rc = input->rclass[o][ii][pi];
          IntSet Drt;
          register_space rs = input->space[rc];
          // If a definition temporary is allocated to an infinite space, its
          // register can be pre-assigned to a narrow range:
          if (!options->disable_presolver_constraints() &&
              !options->disable_infinite_register_dominance_constraints() &&
              !input->use[p] &&
              input->infinite[rs] &&
              input->infinite_atom_range.count(
                make_pair(input->single_temp[p], rs))) {
            temporary t = input->single_temp[p];
            pair<temporary, register_space> trs = make_pair(t, rs);
            register_atom fra = input->infinite_atom_range[trs][0],
                          lra = input->infinite_atom_range[trs][1];
            int n = ((lra - fra) / input->width[t]) + 1;
            Drt = IntSet(IntArgs::create(n, fra, input->width[t]));
          } else {
	    IntSetRanges irs(input->atom_set[rc]);
	    Drt = IntSet(irs);
	  }

          atoms << IntVar(*this, Drt);
        }
      }
      if (input->delimiter[o] && !must_connect(p)) {
        assert(atoms.size() == 1);
        constraint(ry(p) == ite(x(p), atoms[0], NULL_REGISTER));
      } else {
        constraint(ry(p) == element(atoms, i(o)));
      }
    }
}

void Model::post_disjoint_live_ranges_constraints(block b) {

  // Temporaries whose live ranges overlap are assigned to different register
  // atoms:

  IntVarArgs bld, bw, bre,
    br  = temps_to_var_args(v_r, input->tmp[b]),
    bls = temps_to_var_args(v_ls, input->tmp[b]),
    ble = temps_to_var_args(v_le, input->tmp[b]);
  BoolVarArgs bm;

  for (temporary t : input->tmp[b]) {
    bld << ld(t);
    bw  << var(input->width[t]);
    bre << var(r(t) + input->width[t]);
    bm  << var(l(t) && (ld(t) > 0));
  }

  nooverlap(*this, br, bw, bre, bls, bld, ble, bm, ipl);

}

void Model::post_preassignment_constraints(block b) {

  // Certain operands are pre-assigned to registers:

  for (vector<int> pa : input->preassign) {
    operand p = pa[0];
    register_atom a = pa[1];
    if (input->pb[p] == b) constraint(ry(p) == a);
  }

}

void Model::post_alignment_constraints(block b) {

  // Aligned operands are assigned to registers at a given relative distance:

  for (unsigned int ai = 0; ai < input->baligned[b].size(); ai++) {
    operand p = input->baligned[b][ai][0],
            q = input->baligned[b][ai][2];
    operation op = input->oper[p],
              oq = input->oper[q];
    instruction i = input->baligned[b][ai][1],
                j = input->baligned[b][ai][3];
    int adist = input->badist[b][ai];
    if (input->instructions[op].size()==1 && input->instructions[oq].size()==1) {
      constraint(ry(p) == ry(q) + adist);
    } else {
      IntVar ryqa(*this, ry(q).min()+adist, ry(q).max()+adist);
      constraint(ryqa == (ry(q) + adist));
      BoolVar r(*this, 0, 1);
      rel(*this, ry(p), IRT_EQ, ryqa, r, ipl);
      constraint(!imp(op, i) || !imp(oq, j) || r);
    }
  }

}

void Model::post_packing_constraints(block b) {

  // Packed operands are assigned to contiguous, complementary registers:

  for (vector<operand> ps : input->bpacked[b]) {
    operand p = ps[0], q = ps[1];
    int w = input->operand_width[p];

    BoolVarArgs cases;
    IntVarArgs ryps;

    // first case:  bound operand packed in high component
    cases << var(x(p) && ((ry(p) % (w*2)) == 0));
    ryps << var(ry(p) + w);

    // second case: bound operand packed in low component
    cases << var(x(p) && ((ry(p) % (w*2)) != 0));
    ryps << var(ry(p) - w);

    // third case:  bound operand not packed
    cases << var(!x(p));
    IntVar any(*this, Gecode::Int::Limits::min, Gecode::Int::Limits::max);
    ryps << any;

    IntVar idx(*this, 0, 2);
    channel(*this, cases, idx);

    constraint(ry(q) == element(ryps, idx));
  }

}

void Model::post_extensional_constraints(block b) {

  // The registers assigned to some pairs of operands are related extensionally:

  for (unsigned int i = 0; i < input->exrelated.size(); i++) {
    operand p = input->exrelated[i][0],
            q = input->exrelated[i][1];
    if (input->pb[p] == b) {
      IntVarArgs rys;
      rys << ry(p) << ry(q);
      TupleSet ts(input->table[i][0].size());
      for (vector<int> row : input->table[i]) ts.add(IntArgs(row));
      ts.finalize();
      extensional(*this, rys, ts);
    }
  }

}

void Model::post_data_precedences_constraints(block b) {

  // An operation that uses a temporary must be preceded by its definer:

  // Cycle in which t is defined (taking into account its definition latency)
  IntVarArgs ct;
  map<temporary, unsigned int> ctindex;
  unsigned int i = 0;
  for (temporary t : input->tmp[b]) {
    operand p = input->definer[t];
    operation d = input->oper[p];
    ct << var(c(d) + max(input->min_active_lat[p], lt(p)) + slack(p));
    ctindex[t] = i;
    i++;
  }

  for (operation u : input->ops[b])
    if (input->type[u] != KILL)	// handled in post_kill_issue_cycle_constraints()
      for (operand q : input->operands[u])
	if (input->use[q]) {
	  IntVarArgs cs;
	  for (temporary t : input->temps[q])
	    if (t == NULL_TEMPORARY) {
	      IntVarArgs pcs;
	      // TODO: can we use a tighter bound here?
	      for (temporary t1 : input->real_temps[q])
		pcs << var(c(input->def_opr[t1]));
	      cs << var(min(pcs));
	    } else {
	      // Maximum of the definition cycle of t and the definition cycle of
	      // the ultimate source of t
	      IntVarArgs cts;
	      cts << ct[ctindex[t]];
	      cts << ct[ctindex[input->ultimate_source[t]]];
	      cs << var(max(cts));
	    }

	  constraint(c(u) >= element(cs, y(q)) + lt(q) + slack(q));
	}

}

void Model::post_processor_resources_constraints(block b) {

  // The capacity of processor resources cannot be exceeded at any issue cycle:

  vector<resource> essential_r;
  map<resource, vector<UsageTask> > r2tasks;

  for (resource r : input->R)
    if (!contains(input->subsumed_resources[b], r))
      essential_r.push_back(r);

  for (operation o : input->ops[b]) {

    typedef int instruction_index;

    // Map from consumption to tasks for each resource
    vector<map<int, vector<instruction_index> > > rc2tasks;
    map<int, vector<instruction_index> > emptyMap;
    init_vector(rc2tasks, input->R.size(), emptyMap);

    // Complete map with tasks grouped by consumption
    for (unsigned int ii = 0; ii < input->instructions[o].size(); ii++) {
      instruction i = input->instructions[o][ii];
      for (resource r : essential_r) {
        int con = input->con[i][r],
            dur = input->dur[i][r];
        if (con > 0 && dur > 0)
          rc2tasks[r][con].push_back(ii);
      }
    }

    // For each operation, resource and consumption, define a task possibly
    // related to several instructions
    for (resource r : essential_r) {
      for (auto ctts : rc2tasks[r]) {
        IntArgs iis;
        vector<int> durs, offs;
        set<instruction> involved;
        vector<int> involved_durs;
        for (instruction_index ii : ctts.second) {
          iis << ii;
          instruction i = input->instructions[o][ii];
          involved.insert(i);
          int dur = input->dur[i][r];
          involved_durs.push_back(dur);
        }
        for (instruction i : input->instructions[o]) {
          int dur = input->dur[i][r],
              off = input->off[i][r];
          offs.push_back(off);
          if (involved.count(i)) {
            durs.push_back(dur);
          } else { // Optimization: the duration of an instruction that is not
                   // involved is set to the minimum of the involved durations
                   // (for better propagation of the element constraint)
            durs.push_back(min_of(involved_durs));
          }
        }
        UsageTask task;
        IntVar off(*this, min_of(offs), max_of(offs));
        element(*this, IntArgs(offs), i(o), off);
        IntVar tc(*this, min_of(offs), input->maxc[b]);
        constraint(tc == c(o) + off);
        task.c = tc;
        IntVar dur(*this, min_of(durs), max_of(durs));
        element(*this, IntArgs(durs), i(o), dur);
        task.dur = dur;
        task.e = var(tc + task.dur);
        task.con = ctts.first;
        BoolVar opc(*this, 0, 1);
        Gecode::dom(*this, i(o), IntSet(iis), opc);
        task.o = opc;
        r2tasks[r].push_back(task);
      }
    }
  }

  for (resource r : essential_r) {

    IntVarArgs rc; // Start time of each task
    IntVarArgs rdur; // Duration of each task
    IntVarArgs re; // End time of each task
    IntArgs rcon; // Consumption of each task
    BoolVarArgs ro; // Whether each task is scheduled

    for (UsageTask task : r2tasks[r]) {
      rc << task.c;
      rdur << task.dur;
      re << task.e;
      rcon << task.con;
      ro << task.o;
    }

    cumulative(*this, input->cap[r], rc, rdur, re, rcon, ro, IPL_BASIC_ADVANCED);

  }

}

void Model::post_fixed_precedences_constraints(block b) {

  // Control and read-write dependencies yield fixed precedences among
  // operations:

  for (unsigned int e = 0; e < input->dep[b].size(); e++) {

    operation d = input->dep[b][e][0],
              u = input->dep[b][e][1];

    IntArgs dist(input->dist[b][e]);

    constraint((a(d) && a(u)) >> (c(u) >= c(d) + element(dist, i(d))));
  }

}

void Model::post_prescheduling_constraints(block b) {

  // Certain operations are prescheduled before the last operation issue:

  for (vector<int> v : input->prescheduled) {
    operation o = v[0];
    issue_cycle c0 = v[1];

    if (input->oblock[o] != b)
      continue;

    constraint(a(o) >> (c(o) == c0));
    constraint(a(o) == (c0 < c(input->out[b])));
  }

}

void Model::post_bypassing_constraints(block b) {

  // The operation of a bypassing operand is scheduled in parallel with its
  // definer:

  for (operation o : input->ops[b]) {
    for (unsigned pi = 0; pi < input->operands[o].size(); pi++) {
      bool any_bypass = false;
      for (unsigned int ii = 0; ii < input->instructions[o].size(); ii++) {
        if (input->bypass[o][ii][pi]) {
           any_bypass = true;
          break;
        }
      }
      if (!any_bypass) continue;
      IntVarArgs cs;
      for (unsigned int ii = 0; ii < input->instructions[o].size(); ii++) {
        if (input->bypass[o][ii][pi]) {
          operand p = input->operands[o][pi];
          IntVarArgs dcs;
          for (temporary t : input->temps[p]) {
            if (t == NULL_TEMPORARY) {
              dcs << c(o);
            } else {
              dcs << c(input->oper[input->definer[t]]);
            }
          }
          cs << var(element(dcs, y(p)));
        } else {
          cs << c(o);
        }
      }
      constraint(c(o) == element(cs, i(o)));
    }
  }

}

void Model::post_adhoc_constraints(block b) {
  for (UnisonConstraintExpr e : input->E) {
    if (in_block(e, b, input)) {
      constraint(adhoc_constraint_var(e));
    }
  }

}

void Model::post_improved_model_constraints(block b) {

  if (!options->disable_improving()) {
    post_null_register_constraints(b);
    post_effective_copy_constraints(b);
    if (!options->disable_maximum_temporary_usage_constraints())
      post_maximum_temporary_usage_constraints(b);
    post_minimum_temporary_duration_constraints(b);
    post_define_issue_cycle_constraints(b);
    post_kill_issue_cycle_constraints(b);
    post_disjoint_component_operand_constraints(b);
    if (!options->disable_space_capacity_constraints())
      post_space_capacity_constraints(b);
    post_branch_issue_cycle_constraints(b);
    post_active_first_copy_constraints(b);
    post_callee_saved_symmetry_breaking_constraints(b);
    if (!options->disable_precedence_variables()) {
      post_irreflexive_precedence_constraints(b);
      post_transitive_precedence_constraints(b);
    }
    post_cost_domain_constraints(b);
    post_local_congruence_constraints(b);
    post_ultimate_source_constraints(b);
  }

}

void Model::post_null_register_constraints(block b) {

  // Connected operands cannot be assigned to the null register:

  for (operand p : input->ope[b])
    constraint(x(p) == (ry(p) >= 0));

}

void Model::post_effective_copy_constraints(block b) {

  // The source and destination operands of an active copy are assigned to
  // different registers:

  for (operation o : input->ops[b])
    if (input->type[o] == COPY)
      constraint(a(o) == (ry(src(o)) != ry(dst(o))));

}

void Model::post_disjoint_operand_constraints(block b) {

  // Use operands of an operation are assigned to different registers when
  // they do not share temporaries:

  for (operation o : input->ops[b]) {
    vector<operand> ps;
    for (operand p : input->operands[o])
      if (input->use[p]) ps.push_back(p);
    disjoint_operand_registers(ps);
  }

  // Definition operands of an operation are assigned to different registers
  // when they do not share temporaries:

  for (operation o : input->ops[b]) {
    vector<operand> ps;
    for (operand p : input->operands[o])
      if (!input->use[p]) ps.push_back(p);
    disjoint_operand_registers(ps);
  }

}

void Model::post_maximum_temporary_usage_constraints(block b) {

  // The number of times a temporary can be connected to use operands is bound
  // by the number of copy-related operands in non-copy operations:

  for (temporary t : input->tmp[b]) {

    unsigned int n = 0;
    for (operand q : input->ope[b])
      if ((input->use[q]) &&
          (input->type[input->oper[q]] != COPY) &&
          (input->copyreltop[q] == input->copyreltop[input->definer[t]])) n++;

    constraint(us(t) <= n);

  }

}

void Model::post_copy_symmetry_breaking_constraints(block b) {

  // A copy can only be active when all its interchangeable copies with lower
  // index are active:

  for (vector<operation> os : input->binterchangeable[b]) {
    BoolVarArgs as;
    for (operation o : os) as << a(o);
    rel(*this, as, IRT_GQ, ipl);
  }

}


void Model::post_copy_dominance_constraints(block b) {

  // A copy can only be active when all its dominating copies with lower index
  // are active:

  for (operation o1 : input->copies[b])
    for (operation o2 : input->copies[b]) {
      if ((o1 < o2) &&
          subseteq(input->temps[src(o1)], input->temps[src(o2)]) &&
          subseteq(input->users[input->single_temp[dst(o2)]],
                   input->users[input->single_temp[dst(o1)]])) {

        IntArgs own_ops;
        for (unsigned int ii = 0; ii < input->instructions[o2].size(); ii++) {
          instruction i = input->instructions[o2][ii];
          if (!contains(input->instructions[o1], i)) own_ops << ii;
        }
        BoolVar o2_own_op(*this, 0, 1);
        dom(*this, i(o2), IntSet(own_ops), o2_own_op, ipl);

        IntArgs own_temps;
        for (unsigned int ti = 0; ti < input->temps[src(o2)].size(); ti++) {
          temporary t = input->temps[src(o2)][ti];
          if (!contains(input->temps[src(o1)], t)) own_temps << ti;
        }
        BoolVar o2_own_temp(*this, 0, 1);
        dom(*this, y(src(o2)), IntSet(own_temps), o2_own_temp, ipl);

        constraint(a(o1) || (!a(o2)) || o2_own_op || o2_own_temp);
      }
    }

}

void Model::post_non_decreasing_temporary_usage_constraints(block b) {

  // A copy cannot feed more uses than its interchangeable copies with lower
  // index:

  for (vector<operation> os : input->binterchangeable[b]) {
    IntVarArgs uss;
    for (operation o : os) uss << us(input->single_temp[dst(o)]);
    rel(*this, uss, IRT_GQ, ipl);
  }

}

void Model::post_first_k_copies_constraints(block b) {

  // If a group of interchangeable copies have k different users, at most the k
  // first interchangeable copies can be active:

  for (vector<operation> os : input->binterchangeable[b]) {
    vector<temporary> ts;
    SetVarArgs allusers;
    for (operation o : os) {
      temporary t = input->single_temp[dst(o)];
      ts.push_back(t);
      allusers << users(t);
    }
    IntVar card = var(cardinality(setunion(allusers)));
    for (unsigned int k = 1; k < ts.size(); k++) {
      constraint((card <= k) >> !l(ts[k]));
    }
  }

}


void Model::post_minimum_temporary_duration_constraints(block b) {

  // The duration of the live range of a temporary is equal or longer
  // than the maximum between the latencies of its uses and its
  // minimum live range:

  for (operation o : input->ops[b])
    for (operand p : input->operands[o])
      if (!input->use[p]) {
        temporary t = input->single_temp[p];
        IntVarArgs lats;
        for (operand q : input->users[t])
          lats << var(ite(u(q, t), lat(q, t), input->minlive[t]));
        constraint(ld(t) >= max(lats));
      }

}

void Model::post_define_issue_cycle_constraints(block b) {

  // Define operations are issued together with the corresponding single
  // consumer operation (if there is only one of them):

  for (operation o : input->ops[b])
    if (input->type[o] == DEFINE) {
      set<operation> js;
      for (operand p : input->operands[o])
        for (operand q : input->users[input->single_temp[p]])
          js.insert(input->oper[q]);
      if (js.size() == 1) {
        operation j = *(js.begin());
        assert(input->oblock[j] == b);
        IntVarArgs lats;
        for (operand p : input->operands[o]) {
          temporary t = input->single_temp[p];
          for (operand q : input->users[t]) lats << lat(q, t);
        }
        constraint(c(j) == c(o) + max(lats));
      }
    }

}

void Model::post_kill_issue_cycle_constraints(block b) {

  // Kill operations are issued together with the corresponding producer
  // operations:

  for (operation o2 : input->ops[b])
    if (input->type[o2] == KILL) {
      if (input->mandatory_index[o2]>=0) {
	operation o1 = -1;
	IntVarArgs lats;
	for (operand q : input->operands[o2]) {
	  temporary t = input->single_temp[q];
	  operand p = input->definer[t];
	  lats << lat(q, t);
	  o1 = input->oper[p];
	}
        constraint(c(o2) == c(o1) + max(lats));
      } else {
	operand q = input->operands[o2][0];
	IntVarArgs cs;
	vector<operation>os;
	for (temporary t : input->temps[q]) {
          if (t == NULL_TEMPORARY) {
	    IntVarArgs pcs;
            for (temporary t1 : input->real_temps[q])
              pcs << var(c(input->def_opr[t1]));
            cs << var(min(pcs));
	  } else {
	    operand p = input->definer[t];
	    cs << var(c(input->oper[p]) + max(input->min_active_lat[p], lt(p)) + slack(p));
	    os.push_back(input->oper[p]);
	  }
	}
	constraint(c(o2) == element(cs, y(q)) + lt(q) + slack(q));
	if (os.size()==1) {
	  operand p1 = input->definer[input->single_temp[q]];
	  constraint(y(p1) == y(q));
	  constraint(ry(p1) == ry(q));
	} else {
	  BoolVarArgs as;
	  for (operation o : os) as << a(o);
	  rel(*this, as, IRT_LQ, 1, ipl);
	}
      }
    }

}

void Model::post_disjoint_congruent_operand_constraints(block b) {

  // The live range of congruent local operands cannot overlap
  // (TODO: this is subsumed by the 'before' constraints, replace!):

  for (operation o : input->ops[b])
    for (operand p : input->operands[o])
      if (input->use[p]) {
        operand q = -1;
        for (operand q1 : input->operands[o])
          if (!input->use[q1] &&
              (input->operand_congruence[p] == input->operand_congruence[q1]))
            q = q1;
        if (q != -1) {
          IntVarArgs les;
          for (temporary t1 : input->temps[p])
            les << (t1 == NULL_TEMPORARY ? zero : le(t1));
          constraint((lt(q) > 0) >> (element(les, y(p)) <= c(o)));
        }
      }

}

void Model::post_disjoint_component_operand_constraints(block b) {
  for (vector<int> tuple : input->baligned[b]) {
    operand p = tuple[0],
            q = tuple[2];
    operation op = input->oper[p],
              oq = input->oper[q];
    instruction i = tuple[1],
                j = tuple[3];
    if (op == oq &&
        i == j &&
        input->instructions[op].size() == 1 &&
        input->instructions[op][0] == i &&
        input->use[p] != input->use[q]) {
      IntVarArgs ples;
      ples << ple(p) << ple(q);
      constraint(min(ples) == c(op));
    }
  }

}

void Model::post_space_capacity_constraints(block b) {

  // The capacity of a register space is never exceeded:

  for (register_space rs : input->RS) {
    register_atom fa = input->range[rs][0], la = input->range[rs][1];
    if (!input->infinite[rs] && may_saturate(b, fa, la)) {
      // Total capacity of the resource
      int cap = la - fa + 1;
      IntVarArgs st; // Start time of each task
      IntVarArgs dur; // Duration of each task
      IntVarArgs et; // End time of each task
      IntArgs use; // Capacity of the resource used by each task
      BoolVarArgs ot; // Whether each task is scheduled
      for (temporary t : input->tmp[b]) {
        // Define task for t
        st << ls(t);
        dur << ld(t);
        et << le(t);
        // Assumption: if t is wider than rs and allocated to rs, it saturates
        // rs
        use << min(cap, input->width[t]);
        ot << var(l(t) && (ld(t) > 0) && totally_in_register_space(t, rs));
      }
      cumulative(*this, cap, st, dur, et, use, ot, IPL_BASIC_ADVANCED);
    }
  }

}

void Model::post_branch_issue_cycle_constraints(block b) {

  // Branch operations are issued as late as possible:

  operation bi = -1;
  for (operation o : input->ops[b])
    if (input->type[o] == BRANCH) {
      bi = o;
      break;
    }
  operation oi = input->out[b];
  if (contains(input->ops[b], bi)) {
    // Maximum fixed distance to the out operation
    int dist = -1;
    for (unsigned int e = 0; e < input->dep[b].size(); e++) {
      operation o1 = input->dep[b][e][0], o2 = input->dep[b][e][1];
      if (o1 == bi && o2 == oi) {
        dist = max_of(input->dist[b][e]);
        break;
      }
    }
    assert(dist != -1);
    constraint(a(bi) >> (c(bi) == c(oi) - dist));
  }

}

void Model::post_redefined_operand_constraints(block b) {

  // Redefined operands of different operations cannot be connected to the same
  // temporary if their latencies are non-null:

  for (operation o1 : input->ops[b])
    for (operation o2 : input->ops[b])
      if (o1 < o2)
        for (pair<operand, operand> pp : input->redefined[o1])
          for (pair<operand, operand> qq : input->redefined[o2]) {
            operand p = pp.first, p2 = pp.second,
                    q = qq.first, q2 = qq.second;
            if (subseteq(input->temps[p], input->temps[q]) ||
                subseteq(input->temps[q], input->temps[p])) {
              constraint(((lt(p2) > 0) && (lt(q2) > 0)) >> (y(p) != y(q)));
	    }
          }

}

void Model::post_active_first_copy_constraints(block b) {

  // [MC] Difficult to understand whether or not this gives anything; see in particular the !rtp_in_o2_atoms condition.
  // If a copy is needed, the first dominant copy in a group of
  // related copies must be active:

  for (temporary t : input->tmp[b]) {
    operand p = input->definer[t];
    if (input->type[input->oper[p]] != COPY) {
      for (operand q : input->users[t]) {
        if (input->type[input->oper[q]] != COPY && must_connect(q)) {
          // Copy with minimum index having t as a source
          operation o1 = input->first_copy[t];
          if (o1 != -1) {
            IntArgs o2_atoms;
            for (operand r : input->users[t]) {
              operation o2 = input->oper[r];
              if (input->type[o2] == COPY && o1 < o2) {
                for (unsigned int ii = 0; ii < input->instructions[o2].size(); ii++) {
                  instruction i = input->instructions[o2][ii];
                  if (!contains(input->instructions[o1], i)) {
                    o2_atoms << IntArgs(input->atoms[input->rclass[o2][ii][0]]);
		  }
                }
              }
            }
            BoolVar rtp_in_o2_atoms(*this, 0, 1);
            dom(*this, ry(p), IntSet(o2_atoms), rtp_in_o2_atoms, ipl);
            BoolVarArgs ncs;
            for (temporary t1 : input->temps[q])
              if (input->type[input->def_opr[t1]] != COPY) ncs << u(q, t1);
            // If q is connected to a copy-defined temporary that is related to
            // t and t is assigned to a register atom which can be defined by o1
            // (the first copy of t), then o1 must be active:
            constraint((sum(ncs) == 0 && !rtp_in_o2_atoms) >> a(o1));
          }
        }
      }
    }
  }

}

void Model::post_callee_saved_symmetry_breaking_constraints(block b) {

  // Assuming all callee-saved registers and copies are symmetric,
  // callee-saved copies with lower index must be active first
  // spill operations must be smallest register first
  // unspill operations must be smallest register last
  // e.g. "calleesaved_spill": [[3,158,167],[4,159,168],[5,160,169],[6,161,170],[7,162,171],[8,163,172]],

  if (!input->calleesaved_spill.empty()) {
    unsigned nrows = input->calleesaved_spill[0].size();
    unsigned ncols = input->calleesaved_spill.size();

    // callee-saved copies with lower index must be active first, and by nondecreasing cycle
    for (unsigned j=0; j<nrows; j++) {
      operation firstop = input->calleesaved_spill[0][j];
      if (input->oblock[firstop] == b) {
	BoolVarArgs as;
	as << a(firstop);
	for (unsigned k=1; k<ncols; k++) {
	  operation prevop = input->calleesaved_spill[k-1][j];
	  operation nextop = input->calleesaved_spill[k][j];
	  as << a(nextop);
	  if (j == 0)		// spill
	    constraint(a(nextop) >> (c(prevop) <= c(nextop)));
	  else			// unspill
	    constraint(a(nextop) >> (c(prevop) >= c(nextop)));
	}
	rel(*this, as, IRT_GQ);
      }
    }

    // if spill + unspill sequences in block 0
    //  then prevent useless spill+unspill in the same cycle
    if (b == 0 && nrows >= 2 && input->oblock[input->calleesaved_spill[0][1]] == 0) {
      for (unsigned k=0; k<ncols; k++) {
	operation s = input->calleesaved_spill[k][0];
	operation u = input->calleesaved_spill[k][1];
	constraint(a(u) >> (c(s) < c(u)));
      }
    }
  }
}

void Model::post_irreflexive_precedence_constraints(block b) {

  // Operations cannot precede each other:

  for (operation o1 : input->mandatory[b])
    for (operation o2 : input->mandatory[b])
      constraint(!(p(o1, o2) && p(o2, o1)));

}

void Model::post_transitive_precedence_constraints(block b) {

  // Precedences among operations are transitive:

  for (operation o1 : input->mandatory[b])
    for (operation o2 : input->mandatory[b])
      for (operation o3 : input->mandatory[b]) {
        constraint((p(o1, o2) && p(o2, o3)) >> p(o1, o3));
        constraint((!p(o1, o2) && !p(o2, o3)) >> !p(o1, o3));
      }

}

void Model::post_killed_temporary_precedence_constraints(block b) {

  // A temporary killer cannot precede other uses of the temporary:

  for (operation o1 : input->mandatory[b])
    for (pair<operand, operand> pp1 : input->redefined[o1]) {
      operand p1 = pp1.first;
      for (temporary t : input->temps[p1])
        for (operand q : input->users[t]) {
          operation o2 = input->oper[q];
          if (o2 != o1 && contains(input->mandatory[b], o2)) {
            operand p2 = -1;
            for (vector<operand> c : input->bcongr[b]) {
               for (operand p : input->operands[o1])
                 if (!input->use[p] && contains(c, p)) {
                   p2 = p;
                   break;
                 }
               if (p2 != -1) break;
            }
            assert(p2 != -1);
            constraint(((lt(p2) > 0) && u(p1, t) && u(q, t)) >> !p(o1, o2));
          }
        }
    }

}

void Model::post_cost_domain_constraints(block b) {

  // If the resource whose consumption is to be optimized is other than issue
  // cycles, allow only multiples of the greatest common divisor of the
  // consumptions:

  for (unsigned int n = 0; n < input->N; n++) {
    resource r = input->optimize_resource[n];
    if (r != ISSUE_CYCLES) {
      IntArgs cons = consumption_domain(r, input->ops[b]);
      dom(*this, f(b, n), IntSet(cons));
    }
  }

}

void Model::post_local_congruence_constraints(block b) {

  // Local connected congruent operands are assigned to the same register:

  for (vector<operand> c : input->bcongr[b]) {
    for (unsigned pi = 0; pi < c.size(); pi++)
      for (unsigned qi = pi + 1; qi < c.size(); qi++) {
        operand p = c[pi], q = c[qi];
        constraint((x(p) && x(q)) >> (ry(p) == (ry(q))));
      }
  }

}

void Model::post_ultimate_source_constraints(block b) {

  // [MC] somewhat related to quasi_adjacent, but not subsumed by presolver.
  // Ultimate source temporaries are live if some operand is connected to their
  // derived temporaries:

  for (operand p : input->ope[b])
    if (input->use[p]) {
      map<temporary, vector<int> > t2us;
      for (unsigned ti = 0; ti < input->temps[p].size(); ti++) {
        temporary t = input->temps[p][ti];
        if (t != NULL_TEMPORARY) {
          temporary tsrc = input->ultimate_source[t];
          t2us[tsrc].push_back(ti);
        }
      }
      for (auto tus : t2us) {
        temporary t = tus.first;
        if (!must_connect(input->definer[t])) {
          vector<int> us = tus.second;
          if (us.size() > 1) {
            BoolVar yp_in_us(*this, 0, 1);
            Gecode::dom(*this, y(p), IntSet(IntArgs(us)), yp_in_us);
            constraint(yp_in_us >> l(t));
          }

          // If p is the only ultimate user, the implication goes in the other
          // direction as well:

          bool p_only_ultimate_user = true;
          for (operand q : input->ope[b])
            if (q != p && input->use[q] &&
                input->type[input->oper[q]] != COPY) {
              for (temporary t1 : input->real_temps[q]) {
                if (input->ultimate_source[t1] == t) {
                  p_only_ultimate_user = false;
                  break;
                }
              }
              if (!p_only_ultimate_user) break;
            }

          if (p_only_ultimate_user) {
            BoolVar yp_in_us(*this, 0, 1);
            Gecode::dom(*this, y(p), IntSet(IntArgs(us)), yp_in_us);
            constraint(l(t) >> yp_in_us);
          }

        }
      }
    }

}

void Model::post_presolver_constraints(block b) {

  if (!options->disable_presolver_constraints()) {
    if (!options->disable_minimum_number_of_optional_operations_constraints())
      post_minimum_number_of_optional_operations_constraints(b);
    if (!options->disable_allowed_activation_constraints())
      post_allowed_activation_constraints(b);
    if (!options->disable_copy_activation_and_dataflow_constraints())
      post_allowed_copy_activation_and_dataflow_constraints(b);
    if (!options->disable_nogood_constraints())
      post_nogood_constraints(b);
    if (!options->disable_conditional_precedence_constraints())
      post_conditional_precedence_constraints(b);
    if (!options->disable_partially_ordered_live_range_constraints())
      post_partially_ordered_live_range_constraints(b);
    if (!options->disable_across_call_disjoint_temporary_constraints())
      post_across_call_disjoint_temporary_constraints(b);
    if (!options->disable_across_call_disjoint_temporary_set_constraints())
      post_across_call_disjoint_temporary_set_constraints(b);
    if (!options->disable_temporary_symmetry_breaking_constraints())
      post_temporary_symmetry_breaking_constraints(b);
    post_diffregs_constraints(b);
    post_difftemps_constraints(b);
    post_dominates_constraints(b);
    post_wcet_constraints(b);
    // TODO: to be revisited after minlive model improvement
    // post_killer_operand_constraints(b);
    post_mandatory_reuse_constraints(b);
  }

}

void Model::post_minimum_number_of_optional_operations_constraints(block b) {

  // At least a certain number of optional operations is active:

  int n = input->optional_min[b];
  if (n > 0) {
    BoolVarArgs as;
    for (operation o : input->ops[b])
      if (!contains(input->mandatory[b], o)) as << a(o);
    constraint(sum(as) >= n);
  }

}

void Model::post_allowed_activation_constraints(block b) {

  // Only certain combinations of active operations are allowed:

  for (PresolverActiveTable table : input->bactive_tables[b]) {

    if (table.tuples.empty()) continue;

    BoolVarArgs as;
    for (operation o : table.os) as << a(o);

    TupleSet ts(table.tuples[0].size());
    for (vector<int> tuple : table.tuples) ts.add(IntArgs(tuple));
    ts.finalize();

    extensional(*this, as, ts);

  }

}

void Model::post_allowed_copy_activation_and_dataflow_constraints(block b) {

  // Only certain combinations of active copies and operand connections are
  // allowed:

  for (PresolverCopyTmpTable table : input->btmp_tables[b]) {

    if (table.tuples.empty()) continue;

    IntVarArgs ats;
    for (operation o : table.os) {
      IntVar ai(*this, 0, 1);
      channel(*this,  a(o), ai, ipl);
      ats << ai;
    }
    for (operand p : table.ps) ats << y(p);

    TupleSet ts(table.tuples[0].size());
    for (vector<int> tuple : table.tuples) {
      IntArgs tp;
      for (unsigned tpi = 0; tpi < table.os.size(); tpi++) tp << tuple[tpi];
      for (unsigned tpi = table.os.size(); tpi < tuple.size(); tpi++) {
	operand p = table.ps[tpi - table.os.size()];
	temporary t = tuple[tpi];
	unsigned int ti = find_index(input->temps[p], t);
	tp << ti;
      }
      ts.add(tp);
    }
    ts.finalize();

    extensional(*this, ats, ts);

  }

}

void Model::post_nogood_constraints(block b) {

  vector<UnisonConstraintExpr> ngs;
  if (!options->disable_basic_nogood_constraints())
    ngs = concat(ngs, input->bnogoods[b]);
  if (!options->disable_additional_nogood_constraints())
    ngs = concat(ngs, input->bnogoods2[b]);

  // All nogoods should be false:

  for (UnisonConstraintExpr ng : ngs) {
    constraint(!adhoc_constraint_var(ng));
  }

}

void Model::post_conditional_precedence_constraints(block b) {

  vector<UnisonConstraintExpr> ps;
  if (!options->disable_basic_conditional_precedence_constraints())
    ps = concat(ps, input->bprecedences[b]);
  if (!options->disable_additional_conditional_precedence_constraints())
    ps = concat(ps, input->bprecedences2[b]);
  
  // Some precedences apply if a given condition holds:

  for (UnisonConstraintExpr p : ps) {
    // FIXME: take into account that dependencies involving delimiters can now
    // be shorter due to slack variables
    constraint(adhoc_constraint_var(p));
  }
}

void Model::post_partially_ordered_live_range_constraints(block b) {

  vector<PresolverBeforeJSON> bfs;
  if (!options->disable_basic_partially_ordered_live_range_constraints())
    bfs = concat(bfs, input->bbefore[b]);
  if (!options->disable_additional_partially_ordered_live_range_constraints())
    bfs = concat(bfs, input->bbefore2[b]);

  // The live ranges of two partially ordered operands do not overlap if a given
  // condition holds:

  for (PresolverBeforeJSON bf : bfs) {
    operand p = bf.p, q = bf.q;
    UnisonConstraintExpr e = bf.e;
    constraint(adhoc_constraint_var(e) >> (ple(p) <= pls(q)));
  }

}

void Model::post_across_call_disjoint_temporary_constraints(block b) {

  // Temporaries whose live range crosses a call operation must be assigned to
  // disjoint registers:

  for (PresolverAcrossJSON sa : input->bacross[b]) {

    operation o = sa.o;

    IntVarArgs rs, w, re, ys, h, ye;
    BoolVarArgs m;
    BoolVar m1(*this, 1, 1);

    // Caller-saved and extra register atoms
    // TODO: group atoms into wider registers
    for (register_atom ra : concat(input->callersaved, sa.ras)) {
      rs << var(ra);
      w << one;
      re << var(ra + 1);
      ys << zero;
      h << one;
      ye << one;
      m << m1;
    }

    // Temporaries
    for (PresolverAcrossItemJSON ai : sa.as) {
      temporary t = ai.t;
      UnisonConstraintExpr e = ai.e;
      if (in_block(e, b, input)) {
	rs << r(t);
	w << var(input->width[t]);
	re << var(r(t) + input->width[t]);
	ys << zero;
	h << one;
	ye << one;
	m << var((l(t) && (ld(t) > 0) && (ls(t) <= c(o)) && (le(t) > c(o)))
		 || adhoc_constraint_var(e));
      }
    }

    nooverlap(*this, rs, w, re, ys, h, ye, m, ipl);

  }

}

void Model::post_across_call_disjoint_temporary_set_constraints(block b) {

  // Temporaries from a set whose live range crosses a call operation must be
  // assigned to disjoint registers:

  for (PresolverSetAcross sa : input->bset_across[b]) {

    operation o = sa.o;

    IntVarArgs rs, w, re, ys, h, ye;

    // Caller-saved and extra register atoms
    // TODO: group atoms into wider registers
    for (register_atom ra : concat(input->callersaved, sa.ras)) {
      rs << var(ra);
      w << one;
      re << var(ra + 1);
      ys << zero;
      h << one;
      ye << one;
    }

    // Temporary sets
    for (vector<temporary> ts : sa.tsets) {

      if (ts.size() == 1) {
	temporary t = ts[0];
	rs << r(t);
	w << var(input->width[t]);
	re << var(r(t) + input->width[t]);
      } else {

	IntVarArgs rss, ws, res, lss, les;
	BoolVarArgs ll;	// MC
	for (temporary t : ts) {
	  rss << r(t);
	  ws << var(input->width[t]);
	  res << var(r(t) + input->width[t]);
	  ll << l(t);		// MC
	  lss << ls(t);
	  les << le(t);
	}

	IntVar ti(*this, 0, ts.size() - 1);
	constraint((element(ll, ti)) && // MC
		   (element(lss, ti) <= c(o)) &&
		   (element(les, ti) > c(o)));

	IntVar rtt(*this, -1, input->RA.size() - 1),
	  wt(*this, 0, input->RA.size()),
	  ret(*this, 0, input->RA.size());

	constraint(rtt == element(rss, ti));
	constraint(wt  == element(ws, ti));
	constraint(ret == element(res, ti));

	rs << rtt;
	w << wt;
	re << ret;

      }

      ys << zero;
      h << one;
      ye << one;

    }

    nooverlap(*this, rs, w, re, ys, h, ye, ipl);

  }

}

void Model::post_temporary_symmetry_breaking_constraints(block b) {

  // Interchangeable temporaries must be used in increasing order by their
  // corresponding operands:

  for (vector<vector<int> > interchangeable : input->bdomops[b]) {
    vector<operand> ps = interchangeable[0];
    vector<temporary> ts = interchangeable[1];
    IntVarArgs ysargs;
    for (operand p : ps) ysargs << y(p);
    IntArgs tsargs(ts.begin(), ts.end());
    precede(*this, ysargs, tsargs);
  }

}

void Model::post_dominates_constraints(block b) {

  // A copy can only be active when all its dominating copies with lower index
  // are active:

  for(PresolverDominates pd : input->bdominates[b]) {
    operand p = input->operands[pd.o2][0];
    vector<int> t_indexes;	// Indexes of temporaries
    vector<int> i_indexes;    // indexed of instructions

    for(temporary t : pd.temps)
      t_indexes.push_back(input->temporary_index[p][t]);

    if(input->temps[p][0] == NULL_TEMPORARY)
      t_indexes.push_back(0);

    for(instruction i : pd.ins)
      i_indexes.push_back(find_index(input->instructions[pd.o2], i));

    if(input->instructions[pd.o2][0] == NULL_INSTRUCTION)
      i_indexes.push_back(0);

    IntArgs temps_args(t_indexes);
    IntArgs ins_args(i_indexes);

    if(pd.temps.empty() && pd.ins.empty()) {
      constraint(a(pd.o1) || !a(pd.o2));
    } else if(pd.ins.empty()) {
      BoolVar b(*this, 0, 1);
      dom(*this, y(p), IntSet(temps_args), b);
      constraint(a(pd.o1) || b);
    } else if(pd.temps.empty()) {
      BoolVar b(*this, 0, 1);
      dom(*this, i(pd.o2), IntSet(ins_args), b);
      constraint(a(pd.o1) || b);
    } else {
      BoolVar b1(*this, 0, 1);
      BoolVar b2(*this, 0, 1);
      dom(*this, y(p), IntSet(temps_args), b1);
      dom(*this, i(pd.o2), IntSet(ins_args), b2);
      constraint(a(pd.o1) || b1 || b2);
    }
  }
}

void Model::post_difftemps_constraints(block b) {

  // Some sets of operands must take distinct temporaries:

  for(vector<operand> ps : input->bdifftemps[b]) {
    IntVarArgs va;
    for(operand p : ps) {
      IntArgs ia(input->temps[p]);
      va << var(element(ia, y(p)));
    }
    Gecode::distinct(*this, va, IPL_DOM);
  }
}

void Model::post_diffregs_constraints(block b) {

  // Some sets of operands must take disjoint registers:

  for(vector<operand> ps : input->bdiffregs[b])
    disjoint_operand_registers(ps);
}

void Model::post_wcet_constraints(block b) {

  // Longest tolerable inter-issue distance

  IntVarArgs worst;
  map<operation,vector<int>> slacks;
  if (input->bwcet[b].empty())
    return;
  for(PresolverWCET& wcet : input->bwcet[b])
    slacks[wcet.o].push_back(wcet.d);
  for(auto kv : slacks) {
    operation o = kv.first;
    bool allOne = true;
    bool allOneButFirst = true;
    bool first = true;
    bool mand = input->instructions[o][0] != NULL_INSTRUCTION;
    
    for(int d : kv.second) {
      if (d!=1) allOne = false;
      if (!first && d!=1) allOneButFirst = false;
      first = false;
    }
    if(allOne) {
      worst << var(1);
    } else if(allOneButFirst && !mand) {
      IntVar ia(*this, 0, 1);
      Gecode::channel(*this, ia, a(o), IPL_DOM);
      worst << ia;
    } else {
      worst << var(element(kv.second, i(o)));
    }
  }
  constraint(c(input->out[b]) <= sum(worst));
    
}

void Model::post_killer_operand_constraints(block b) {

  // A temporary dies at the cycle in which its killer operand is issued:
  // Disabled -- says above "TODO: to be revisited after minlive model improvement"

  for (operand p : input->last_use)
    if (input->pb[p] == b) {
      operation o = input->oper[p];
      constraint(ple(p) == c(o));
    }

}

void Model::post_initial_precedence_constraints(block b) {

  // Statically derived precedences must hold:

  for (vector<operation> pr : input->bprecs[b]) {
    operation o1 = pr[0], o2 = pr[1];
    constraint(p(o1, o2));
  }

}

void Model::post_mandatory_reuse_constraints(block b) {

  for (vector<operand> ps : input->bdomuses[b]) {
    operand p = ps[0], q = ps[1], r = ps[2];
    operation o = input->oper[p];

    for (pair<operand, operand> pp : input->redefined[o])
      if (pp.first == p) continue;

    bool p_preassigned = false;
    for (vector<int> pa : input->preassign) {
      if (pa[0] == p) {
	p_preassigned = true;
	break;
      }
    }

    if (input->type[o] == LOW || input->type[o] == HIGH ||
        input->type[o] == SPLIT2 || input->type[o] == SPLIT4 ||
	input->type[o] == COMBINE) continue;

    if (p_preassigned) continue;
    if (contains(input->last_use, p)) continue; // [MC]

    map<register_space, set<unsigned int> > rs2ois;
    unsigned int pi = input->operand_index[p];
    for (unsigned int ii = 0; ii < input->instructions[o].size(); ii++) {
      if (input->instructions[o][ii] != NULL_INSTRUCTION) {
	register_class rc = input->rclass[o][ii][pi];
	rs2ois[input->space[rc]].insert(ii);
      }
    }

    for (pair<register_space, set<unsigned int> > rsii : rs2ois) {
      register_space rs = rsii.first;
      IntArgs iis;
      for (unsigned int ii : rsii.second) iis << ii;
      BoolVar iop(*this, 0, 1);
      Gecode::dom(*this, i(o), IntSet(iis), iop, ipl);
      for (temporary t : input->temps[r]) if (t != NULL_TEMPORARY) {
	  assert(contains(input->temps[p], t));
	  assert(contains(input->temps[q], t));
	  constraint((u(q, t) && iop && al(rs, t)) >> u(p, t));
	}
    }
  }
}

void Model::post_before_scheduling_constraints(block b) {

  post_temporary_interference_constraints(b);

}

void Model::post_temporary_interference_constraints(block b) {

  // Temporaries live in the same cycle must be assigned different registers:

  for (int cycle = 0; cycle <= c(input->out[b]).max(); cycle++) {
    IntVarArgs rs;    // Temporary registers
    BoolVarArgs live; // Whether the temporary is live in the given cycle
    for (temporary t : input->tmp[b]) {
      // Due to the architecture of Gecode, we cannot exclude temporaries based
      // on the domain of the l variables here
      BoolVar tlive(*this, 0, 1);
      constraint(tlive == (l(t) && (ls(t) <= cycle) && (le(t) > cycle)));
      for (int w = 0; w < input->width[t]; w++) {
        rs << var(r(t) + w);
        live << tlive;
      }
    }
    distinct(*this, rs, live, IPL_DOM);
  }

}

void Model::post_instruction_assignment_constraints(
            const vector<InstructionAssignment> & forbidden) {

  for (InstructionAssignment fb : forbidden) {
    operation o = fb.first;
    unsigned int ii = fb.second;
    constraint(i(o) != ii);
  }

}

void Model::post_cost_definition(block b) {

  // The cost of a block is either its number of issue cycles (makespan) or the
  // total consumption of a given resource:

  for (unsigned int n = 0; n < input->N; n++) {
    resource r = input->optimize_resource[n];
    if (r == ISSUE_CYCLES) {
      constraint(f(b, n) == c(input->out[b]));
    } else {
      IntVarArgs cons;
      for (operation o : input->ops[b]) {
        IntArgs icons;
        for (instruction i : input->instructions[o]) icons << input->con[i][r];
        cons << var(element(icons, i(o)));
      }
      rel(*this, f(b, n) == sum(cons), IPL_BND);
    }
  }

}

void Model::print(ostream & pOs, block b) const {

  int tColWidth = 8;

  vector<operation> os = input->ops[b];
  vector<temporary> ps = input->ope[b];
  vector<temporary> ts = input->tmp[b];

  pOs << "b" << b << ":";

  pOs << endl << endl;

  for (operation o : os)
    pOs << left << "o" << setw(tColWidth) << o << " ";

  pOs << endl;

  for (operation o : os)
    pOs << left << setw(tColWidth) << i(o) << "  ";

  pOs << endl;

  for (operation o : os)
    pOs << left << setw(tColWidth) << c(o) << "  ";

  pOs << endl << endl;

  for (operand p : ps)
    pOs << left << "p" << setw(tColWidth) << p << " ";

  pOs << endl;

  for (operand p : ps)
    pOs << left << setw(tColWidth) << ry(p) << "  ";

  pOs << endl << endl;

  for (temporary t : ts)
    pOs << left << "t" << setw(tColWidth) << t << " ";

  pOs << endl;

  for (temporary t : ts)
    pOs << left << setw(tColWidth) << r(t) << "  ";

  pOs << endl << endl;

}

void Model::compare(const Space& sp, std::ostream& pOs) const {

  const Model& m = static_cast<const Model&>(sp);

  pOs << "Decision variables:" << endl << endl;

  map<int, unsigned int> temp_index;
  for (temporary t : T()) temp_index[t] = (unsigned) temp(t);
  map<int, unsigned int> instr_index;
  for (operation o : O()) instr_index[o] = (unsigned) instr(o);
  map<int, unsigned int> opr_index;
  for (operand p : P()) opr_index[p] = (unsigned) opr(p);
  map<int, unsigned int> live_index;
  for (temporary t : T()) live_index[t] = (unsigned) instr(input->def_opr[t]);

  pOs << "r  : " << compare<IntVar>("r", "t", temp_index, v_r, m.v_r) << endl;
  pOs << "i  : " << compare<IntVar>("i", "o", instr_index, v_i, m.v_i) << endl;
  pOs << "c  : " << compare<IntVar>("c", "o", instr_index, v_c, m.v_c) << endl;
  pOs << "y  : " << compare<IntVar>("y", "p", opr_index, v_y, m.v_y) << endl;

  pOs << endl;

  pOs << "Secondary variables:" << endl << endl;

  pOs << "a  : " << compare<BoolVar>("a", "o", instr_index, v_a, m.v_a) << endl;
  pOs << "ls : " << compare<IntVar>("ls", "t", temp_index, v_ls, m.v_ls) << endl;
  pOs << "le : " << compare<IntVar>("le", "t", temp_index, v_le, m.v_le) << endl;

}


void Model::assign_instruction(operation o, int ii) {
  constraint(i(o) == ii);
}
