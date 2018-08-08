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


#include "globalmodel.hpp"

double GlobalModel::cluster_energy(global_cluster gc) const {
  double e = 0.0;
  for (operand p : input->clusters[gc])
    if (!x(p).assigned()) e += operand_energy(p);
  for (global_cluster gc0 : input->implied_clusters[gc])
    e += cluster_energy(gc0);
  return e;
}

double GlobalModel::energy(global_congruence g) const {
  double e = 0.0;
  for (operand p : input->congr[input->regular[g]]) e += operand_energy(p);
  return e;
}

double GlobalModel::operand_energy(operand p) const {
  block b = input->pb[p];
  return input->freq[b] * input->operand_width[p] *
    (c(input->out[b]).max() / 2.0);
}

double GlobalModel::
cluster_allocation_cost(global_cluster gc, RangeListIter & A) const {
  double cost = 0.0;
  for (operand p : input->clusters[gc])
    if (!x(p).assigned()) cost += operand_allocation_cost(p, A);
  for (global_cluster gc0 : input->implied_clusters[gc])
    cost += cluster_allocation_cost(gc0, A);
  return cost;
}

double GlobalModel::
cluster_remat_cost(global_cluster gc, RangeListIter & A) const {
  double cost = 0.0;
  set<block> bs;
  for (operand p : input->clusters[gc])
    if (input->remat.count(p))
      for (operation o : input->remat[p])
        if (!a(o).assigned()) bs.insert(input->oblock[o]);
  for (operand p : input->clusters[gc])
    if (!x(p).assigned() && bs.count(input->pb[p]))
      cost += operand_allocation_cost(p, A);
  for (global_cluster gc0 : input->implied_clusters[gc])
    cost += cluster_remat_cost(gc0, A);
  return cost;
}

double GlobalModel::
allocation_cost(global_congruence g, RangeListIter & A) const {
  double cost = 0.0;
  for (operand p : input->congr[input->regular[g]])
    cost += operand_allocation_cost(p, A);
  return cost;
}

double GlobalModel::
operand_allocation_cost(operand p, RangeListIter & A) const {
  block b = input->pb[p];
  int minc = INT_MAX, maxc = INT_MIN;
  for (temporary t : input->temps[p])
    if ((t != NULL_TEMPORARY) && !is_dead(t)) {
      if (ls(t).min() < minc) minc = ls(t).min();
      if (ls(t).max() > maxc) maxc = le(t).max();
    }
  pair<int,int> C = make_pair(minc, maxc);
  int w = input->operand_width[p];
  double cost = (saturation_likelihood(b, C, A) * spilling_cost(w, b, A)) / 2.0;
  return cost;
}

double GlobalModel::spilling_cost(int w, block b, RangeListIter &) const {
   return w * input->freq[b];
}

double GlobalModel::connection_benefit(global_cluster gc, bool connect) const {

  set<operand> ps;
  insert_cluster_operands(gc, ps);

  set<operation> os;
  for (operand p : ps)
    if (input->remat.count(p))
      for (operation o : input->remat[p])
        os.insert(o);

  double total_benefit = 0.0, lowest_benefit = INT_MAX;
  bool lowest = false;
  for (operation o : os) {
    double b = inactive_cycle_savings(o);
    total_benefit += b;
    if (b < lowest_benefit) {
      lowest_benefit = b;
      lowest = true;
    }
  }
  if (lowest == false) lowest_benefit = 0;

  // benefits are:
  // - the cheapest of the inactive savings for os, if !connect
  // - all the benefit but the cheapest,            if connect
  // (this is based on the assumption that the original remat operations are
  // placed in the cheapest possible point)
  double benefit =
    connect ?
    total_benefit - lowest_benefit :
    lowest_benefit;

  return benefit;

}

void GlobalModel::
insert_cluster_operands(global_cluster gc, set<operand> & ps) const {
  for (operand p : input->clusters[gc]) ps.insert(p);
  for (global_cluster gc0 : input->implied_clusters[gc])
    insert_cluster_operands(gc0, ps);
}

double GlobalModel::
allocation_benefit(global_congruence g, RangeListIter & A) const {
  double benefit = 0.0;
  for (operation o : input->def_copies[g])
    benefit += coalescing_chances(dst(o), A) * inactive_cycle_savings(o);
  for (operation o : input->use_copies[g])
    benefit += coalescing_chances(src(o), A) * inactive_cycle_savings(o);
  return benefit;
}

double GlobalModel::coalescing_chances(operand p, RangeListIter & A) const {
  operand q = opposite(p);
  // Compute common atoms between A and the home register space of q
  register_space rsq = input->home[q];
  Singleton Aq(input->range[rsq][0], input->range[rsq][1]);
  Inter<RangeListIter, Singleton> Ahrs(A, Aq);
  // Return percentage of A atoms in the home register space of q
  double chances = (double)range_size(Ahrs) / (double)range_size(A);
  return chances;
}

double GlobalModel::inactive_cycle_savings(operation o) const {
  // Compute average latency for non-null instructions that can implement o
  if (a(o).assigned()) return 0.0;
  int n = 0;
  double uselat = 0.0, deflat = 0.0;
  for (IntVarValues ii(i(o)); ii(); ++ii) {
    instruction i = input->instructions[o][ii.val()];
    if (i != NULL_INSTRUCTION) {
      for (unsigned pi = 0; pi < input->operands[o].size(); pi++) {
        operand p = input->operands[o][pi];
        if (input->use[p]) uselat += input->lat[o][ii.val()][pi];
        else deflat += input->lat[o][ii.val()][pi];
      }
      n++;
    }
  }
  uselat /= n;
  deflat /= n;

  double savings = uselat + deflat;
  savings *= input->freq[input->oblock[o]];
  return savings;

}

GlobalModel::GlobalModel(Parameters * p_input, ModelOptions * p_options,
                         IntPropLevel p_ipl) :
  CompleteModel(p_input, p_options, p_ipl)
{
  v_pal  = bool_var_array(P().size() * input->RS.size(), 0, 1);
  v_pals = set_var_array(input->G.size(), IntSet::empty,
                         IntSet(0, input->RS.size() - 1));
  v_oa   = bool_var_array(input->A.size(), 0, 1);
  v_ali  = set_var_array(input->G.size(), IntSet::empty,
                         IntSet(0, input->G.size() - 1));

  // Individual domains of problem variables
  CompleteModel::post_decision_variable_domain_definitions();

  // Secondary variable definitions
  post_secondary_variable_definitions();

  // Basic model
  CompleteModel::post_basic_model_constraints();

  // Improved model
  post_improved_model_constraints();

  // Presolved model
  CompleteModel::post_presolver_constraints();

  // Global cost
  CompleteModel::post_global_cost_definition();

  // Cost of each block
  CompleteModel::post_cost_definition();

}

GlobalModel::GlobalModel(GlobalModel& cg) :
  CompleteModel(cg),
  af(cg.af),
  cf(cg.cf)
{
  v_pal.update(*this, cg.v_pal);
  v_pals.update(*this, cg.v_pals);
  v_oa.update(*this, cg.v_oa);
  v_ali.update(*this, cg.v_ali);
}

GlobalModel* GlobalModel::copy(void) {
  return new GlobalModel(*this);
}

void GlobalModel::post_secondary_variable_definitions(void) {
  CompleteModel::post_secondary_variable_definitions();
  post_operand_allocation_definition();
  post_operand_allocation_set_definition();
  post_alignable_operands_definition();
  post_aligned_congruences_definition();
}

void GlobalModel::post_operand_allocation_definition(void) {

  // pal[p][rs] <-> p is allocated to register space rs

  for (operand p : input->P)
    for (register_space rs : input->RS) {
      register_atom fa = input->range[rs][0],
                    la = input->range[rs][1];
      constraint(pal(p, rs) ==
                 ((ry(p) >= fa) && (ry(p) <= (la - input->operand_width[p] + 1))));
    }

}

void GlobalModel::post_operand_allocation_set_definition(void) {

  // rs \in pals[g] <-> operands in global congruence g are allocated to rs:

  for (global_congruence g : input->G) {
    operand p = input->representative[input->regular[g]];
    BoolVarArgs palrs;
    for (register_space rs : input->RS) palrs << pal(p, rs);
    channel(*this, palrs, pals(g));
  }

}

void GlobalModel::post_alignable_operands_definition(void) {

  // An alignable operand pair is aligned when both operands are connected and
  // assigned to the same register:

  for (alignable a : input->A) {
    operand p = input->pairs[a].first,
            q = input->pairs[a].second;
    constraint(oa(a) == (x(p) && x(q) && (ry(p) == ry(q))));
    // FIXME: why does this propagate more than (ry(p) != ry(q)) >> !oa(a)?
    BoolVar diff(*this, 0, 1);
    rel(*this, ry(p), IRT_NQ, ry(q), diff);
    constraint((x(p) && x(q) && diff) >> !oa(a));
  }

}

void GlobalModel::post_aligned_congruences_definition(void) {

  for (global_congruence g : input->G) dom(*this, ali(g), SRT_SUP, g, g);

  for (alignable a : input->A) {
    operand p = input->pairs[a].first,
            q = input->pairs[a].second;
    global_congruence pg = input->global[input->operand_congruence[p]],
                      qg = input->global[input->operand_congruence[q]];
    constraint(oa(a) == (ali(pg) == ali(qg)));
  }

}

void GlobalModel::post_improved_model_constraints(void) {
  CompleteModel::post_improved_model_constraints();
  if (!options->disable_improving()) {
    post_disjoint_operand_congruence_constraints();
    post_aligned_neighbor_operand_constraints();
    post_preserved_space_capacity_constraints();
    post_global_cost_domain_constraints();
  }
}

void GlobalModel::post_disjoint_operand_congruence_constraints(void) {

  set<pair<global_congruence, global_congruence> > disj_global_congruences;

  for (operation o : input->O)
    if (input->delimiter[o]) {
      vector<operand> ps = input->operands[o];
      for (unsigned int i = 0; i < ps.size(); i++)
        for (unsigned int j = i + 1; j < ps.size(); j++) {
          operand p = ps[i], q = ps[j];
          if (disjoint_sets(input->temps[p], input->temps[q])) {
            global_congruence pg = input->global[input->operand_congruence[p]],
                              qg = input->global[input->operand_congruence[q]];
            disj_global_congruences.insert(make_pair(pg, qg));
          }
        }
    }

  for (pair<global_congruence, global_congruence> gs : disj_global_congruences) {
    global_congruence pg = gs.first, qg = gs.second;
    constraint(ali(pg) || ali(qg));
  }

}

void GlobalModel::post_aligned_neighbor_operand_constraints(void) {

  // Pairs of alignable operands connected to the same temporary are aligned:

  for (alignable a : input->A) {
    operand p = input->pairs[a].first,
            q = input->pairs[a].second;
    temporary t = input->single_temp[p];
    constraint(u(q, t) >> oa(a));
  }

}

void GlobalModel::post_preserved_space_capacity_constraints(void) {

  // The capacity of the preserved registers in each block is not exceeded:

  for (block b : input->B) {
    set<register_atom> destroyed = input->destroyed[b];
    if (destroyed.empty()) continue;
    for (register_space rs : input->RS) {
      set<register_atom> preserved;
      for (register_atom a = input->range[rs][0]; a <= input->range[rs][1]; a++)
        if(!destroyed.count(a)) preserved.insert(a);
      if (preserved.empty() ||
          !may_saturate(b, *(preserved.begin()), *(preserved.rbegin())))
        continue;
      int cap = preserved.size();
      for (operation o : {input->in[b], input->out[b]}) {
        IntArgs ws; // Number of atoms used by each operand
        BoolVarArgs uses; // Whether each operand uses the preserved registers
        IntVarArgs rts; // Operand registers
        // TODO: pick a representative for each class instead
        for (operand p : single_class(input->operands[o])) {
          ws << min(cap, input->operand_width[p]);
          BoolVar uses1(*this, 0, 1);
          IntArgs pas(to_vector(preserved));
          Gecode::dom(*this, ry(p), IntSet(pas), uses1, ipl);
          uses << uses1;
          rts << ry(p);
        }
        // This overconstrains the problem since it assumes that the live range
        // related to the operands will always cross the destroyed register
        // atoms. This assumption seems to be useful to reduce the search space
        // in the case of Hexagon but might lead to sub-optimal solutions or
        // even unsatisfiable problems in other targets (see b7 in
        // adpcm.timing.main):
        if (options->overconstrain()) {
          constraint(sum(ws, uses) <= cap);
        }

        distinct(*this, rts, uses, IPL_DOM);
      }
    }
  }

}

void GlobalModel::post_global_cost_domain_constraints() {

  // If the objective is not weighted and the resource whose consumption is
  // to be optimized is other than issue cycles, allow only multiples of
  // the greatest common divisor of the consumptions:

  for (unsigned int n = 0; n < input->N; n++) {
    resource r = input->optimize_resource[n];
    if (!input->optimize_dynamic[n] && r != ISSUE_CYCLES) {
      IntArgs cons = consumption_domain(r, input->O);
      dom(*this, gf()[n], IntSet(cons));
    }
  }

}


void GlobalModel::post_register_symmetry_breaking_constraints(void) {
  vector<PresolverValuePrecedeChain> chains = value_precede_chains(*input, this, true, -1);
  for (const PresolverValuePrecedeChain& vpc : chains) {
    IntVarArgs rts;
    for (temporary t : vpc.ts) {
      rts << r(t);
    }
    for (vector<register_atom> rs : vpc.rss) {
      IntArgs ras(rs.begin(), rs.end());
      precede(*this, rts, ras);
    }
  }
}


void GlobalModel::post_active_operation(operation o) {
  if (o != NULL_OPERATION) constraint(a(o));
}

void GlobalModel::post_inactive_operation(operation o) {
  if (o != NULL_OPERATION) constraint(!a(o));
}

void GlobalModel::
post_lower_bounds(operation o1, operation o2, block b, int lb) {
  BoolVar all_active(*this, 0, 1);
  BoolVarArgs as;
  if (o1 != NULL_OPERATION) as << a(o1);
  if (o2 != NULL_OPERATION) as << a(o2);
  rel(*this, BOT_AND, as, all_active);
  constraint(all_active >> (f(b, 0) >= lb));
}

void GlobalModel::
post_relaxation_nogood(operation o1, operation o2) {
  BoolVar all_active(*this, 0, 1);
  BoolVarArgs as;
  if (o1 != NULL_OPERATION) as << a(o1);
  if (o2 != NULL_OPERATION) as << a(o2);
  rel(*this, BOT_AND, as, all_active);
  constraint(!all_active);
}

void GlobalModel::
post_connection_lower_bound(operand p, bool connect, block b, int lb) {
  BoolVar cond = connect ? x(p) : var(!x(p));
  constraint(cond >> (f(b, 0) >= lb));
}

void GlobalModel::
post_instruction_nogood(int cost, InstructionAssignment forbidden) {
  operation o = forbidden.first;
  unsigned int ii = forbidden.second;
  block b = input->oblock[o];
  constraint((f(b, 0) < cost) >> (i(o) != ii));
}

void GlobalModel::
post_activation_nogood(operation o, int lb) {
  constraint(a(o) >> (cost()[0] >= lb));
}

void GlobalModel::post_cluster_connection_decision(
     global_cluster gc, bool connect) {
  operand p = input->clusters[gc][0];
  if (connect)
    constraint(x(p));
  else
    constraint(!x(p));
}

void GlobalModel::post_effective_callee_saved_spilling(operation o) {
  // A callee-saved register is spilled iff it is assigned within the function:
  assert(contains(input->callee_saved_stores, o));
  temporary t = input->single_temp[src(o)];
  operand p = input->definer[t];
  register_atom fa = input->p_preassign[p],
                la = input->p_preassign[p] + input->operand_width[p] - 1;
  BoolVarArgs rts;
  for (temporary t : input->T) {
    operation d = input->def_opr[t];
    if ((input->type[d] != IN) &&
        !contains(input->callee_saved_stores, d) &&
        !contains(input->callee_saved_loads, d)) {
      for (register_atom ra = fa - input->width[t] + 1; ra <= la; ra++) {
        BoolVar t_overlap(*this, 0, 1);
        // For some reason, having l = fa - input->width[t] + 1 and m = la in
        // the dom constraint does not propagate with domain consistency
        dom(*this, r(t), ra, ra, t_overlap, IPL_DOM);
        rts << t_overlap;
      }
    }
  }
  rel(*this, BOT_OR, rts, a(o), IPL_DOM);
}

void GlobalModel::constrain_local_cost(block b, IntRelType irt, int cost) {
  rel(*this, f(b, 0), irt, cost, ipl);
}

void GlobalModel::post_different_solution(GlobalModel * g1, bool unsat) {

  BoolVarArgs lits;
  for (global_congruence g : input->G)
    if (!input->preassigned[g]) {
      operand p = input->representative[input->regular[g]];
      lits << var(ry(p) == g1->ry(p).val());
    }
  for (operand p : input->P)
    if (input->global_operand[p]) {
      lits << var(slack(p) == g1->slack(p).val());
    }
  if (input->B.size() > 1 || (input->B.size() == 1 && unsat)) {
    for (activation_class ac : input->AC) {
      operation o = input->activation_class_representative[ac];
      lits << var(a(o) == g1->a(o).val());
    }
  }
  if (input->B.size() == 1 && unsat) {
    for (operation o : input->callee_saved_stores)
      lits << var(a(o) == g1->a(o).val());
  }
  if (lits.size() > 0)
    rel(*this, BOT_AND, lits, 0);

}

void GlobalModel::post_local_solution_cost(LocalModel * l) {
  block b = l->b;
  BoolVarArgs lits;
  for (operation o : {input->in[b], input->out[b]})
    for (operand p : input->operands[o])
      if (!ry(p).assigned()) lits << var(ry(p) == l->ry(p));
  for (activation_class ac : input->AC)
    for (operation o : input->activation_class_operations[ac])
      if (contains(input->ops[b], o))
        if (!a(o).assigned()) lits << var(a(o) == l->a(o));
  if (input->B.size() == 1) {
    for (operation o : input->callee_saved_stores) {
      lits << var(a(o) == l->a(o).val());
    }
  }
  BoolVar local_solution(*this, 0, 1);
  if (lits.size() > 0)
    rel(*this, BOT_AND, lits, local_solution);
  for (unsigned int n = 0; n < input->N; n++)
    constraint(local_solution >> (f(b, n) == l->cost()[n]));
}

void GlobalModel::post_branchers(void) {

  // connection of global operands

  BoolVarArgs xs;
  for (global_cluster gc : input->GC) {
    operand p = input->clusters[gc][0];
    xs << x(p);
  }

  if (cf) {
    branch(*this,
           xs,
           BOOL_VAR_MERIT_MAX(&Merit::cluster_energy),
           BOOL_VAL_MAX(),
           NULL,
           &print_cluster_connection_decision);
  } else {
    branch(*this,
           xs,
           BOOL_VAR_MERIT_MAX(&Merit::cluster_energy),
           BOOL_VAL(&most_effective_connection_decision),
           NULL,
           &print_cluster_disconnection_decision);
  }

  // register allocation

  branch(*this,
         v_pals,
         SET_VAR_MERIT_MAX(&Merit::energy),
         SET_VAL(&most_effective),
         &allocatable,
         &print_allocation_decision);

  // activation

  BoolVarArgs as;
  for (activation_class ac : input->AC) {
    operation o = input->activation_class_representative[ac];
    as << a(o);
  }
  branch(*this, as, BOOL_VAR_NONE(), BOOL_VAL_MAX(),
         NULL, &print_activation_decision);

  if (!options->disable_hints()) {

    // hinted register avoidance

    BoolVarArgs avoidhs;
    for (AvoidHint hint : input->avoidhints) {
      operand p = get<0>(hint);
      vector<register_atom> as = get<1>(hint);
      IntArgs ras(as);
      BoolVar h(*this, 0, 1);
      dom(*this, ry(p), IntSet(ras), h);
      avoidhs << h;
    }

    branch(*this, avoidhs, BOOL_VAR_NONE(), BOOL_VAL_MIN(),
           NULL, &print_hinted_avoidance_decision);

    // hinted register assignment

    BoolVarArgs assignhs;
    for (vector<int> hint : input->assignhints) {
      operand p = hint[0];
      register_atom a = hint[1];
      BoolVar h(*this, 0, 1);
      assignhs << var(ry(p) == a);
    }

    branch(*this, assignhs, BOOL_VAR_NONE(), BOOL_VAL_MAX(),
           NULL, &print_hinted_assignment_decision);

  }

  // register alignment

  branch(*this, v_oa, BOOL_VAR_NONE(), BOOL_VAL_MAX(),
         NULL, &print_alignment_decision);

  // slack assignment

  branch(*this, v_s, INT_VAR_NONE(), INT_VAL(&closest_to_zero),
         NULL, &print_slack_assignment_decision);

  // register assignment

  IntVarArgs prs;

  for (global_congruence g : input->G)
    prs << ry(input->representative[input->regular[g]]);

  branch(*this, prs, INT_VAR_SIZE_MIN(), INT_VAL(&least_assigned),
         NULL, &print_assignment_decision);

}

void GlobalModel::post_callee_saved_branchers(void) {

  vector<operation> cs = input->callee_saved_stores;
  unsigned int active = (1.0 - af) * cs.size();

  BoolVarArgs as1;
  for(unsigned int oi = 0; oi < active; oi++) as1 << a(cs[oi]);

  BoolVarArgs as2;
  for(unsigned int oi = active; oi < cs.size(); oi++) as2 << a(cs[oi]);

  branch(*this, as1, BOOL_VAR_NONE(), BOOL_VAL_MAX());
  branch(*this, as2, BOOL_VAR_NONE(), BOOL_VAL_MIN());

}

void GlobalModel::post_complete_branchers(unsigned int s) {

  branch(*this, cost(), INT_VAR_NONE(), INT_VAL_MIN(),
         NULL, &print_global_cost_decision);

  Rnd r;
  r.seed(s);
  branch(*this, v_a, BOOL_VAR_NONE(), BOOL_VAL_RND(r),
         NULL, &print_global_inactive_decision);

  branch(*this, v_i, INT_VAR_NONE(), INT_VAL_MIN(),
         NULL, &print_global_instruction_decision);

  branch(*this, v_y, INT_VAR_NONE(), INT_VAL_MIN(),
         NULL, &print_global_temporary_decision);

  branch(*this, v_c, INT_VAR_NONE(), INT_VAL_MIN(),
         &schedulable, &print_global_cycle_decision);

  branch(*this, v_r, INT_VAR_NONE(), INT_VAL_RND(r),
         &global_assignable, &print_global_register_decision);


}

bool GlobalModel::master(const MetaInfo& mi) {
  if (mi.type() == MetaInfo::PORTFOLIO) {
    assert(mi.type() == MetaInfo::PORTFOLIO);
    return true; // default return value for portfolio master (no meaning)
  } else if (mi.type() == MetaInfo::RESTART) {
    if (mi.last() != NULL)
      constrain(*mi.last());
    mi.nogoods().post(* this);
    return true; // forces a restart even if a solution has been found
  }
  GECODE_NEVER;
}

bool GlobalModel::slave(const MetaInfo& mi) {
  if (mi.type() == MetaInfo::PORTFOLIO) {
    post_complete_branchers(mi.asset());
    return true; // default return value for portfolio slave (no meaning)
  } else if (mi.type() == MetaInfo::RESTART) {
    return true; // the search space in the slave space is complete
  }
  GECODE_NEVER;
}

void GlobalModel::compare(const Space& sp, std::ostream& pOs) const {

  const GlobalModel& m = static_cast<const GlobalModel&>(sp);

  Model::compare(sp, pOs);

  map<int, unsigned int> opr_index;
  for (operand p : P()) opr_index[p] = (unsigned) opr(p);

  map<int, unsigned int> ali_index;
  for (alignable a : input->A) ali_index[a] = (unsigned) a;

  pOs << "ry : " <<
    Model::compare<IntVar>("rt", "p", opr_index, v_ry, m.v_ry) << endl;

  pOs << "oa : " <<
    Model::compare<BoolVar>("oa", "a", ali_index, v_oa, m.v_oa) << endl;

}

bool GlobalModel::equal_to(const GlobalModel * gs) const {
  for (global_congruence g : input->G) {
    operand p = input->representative[input->regular[g]];
    if (ry(p).val() != gs->ry(p).val()) return false;
  }
  for (operand p : input->P)
    if (input->global_operand[p]) {
      if (s(p).val() != gs->s(p).val()) return false;
    }
  for (activation_class ac : input->AC) {
    operation o = input->activation_class_representative[ac];
    if (a(o).val() != gs->a(o).val()) return false;
  }
  return true;
}

void GlobalModel::apply_solution(LocalModel * ls) {

  block b = ls->b;

  for (temporary t1 : input->tmp[b])
    if (!ls->is_dead(t1))
      constraint(r(t1) == ls->r(t1));

  for (operation o : input->ops[b])
    constraint(i(o) == ls->i(o));

  for (operation o : input->ops[b])
    if (!ls->is_inactive(o))
      constraint(c(o) == ls->c(o));

  for (operand p : input->ope[b])
    constraint(y(p) == ls->y(p));

}

vector<activation_class> GlobalModel::unnecessary_activation_classes() {
  vector<activation_class> unnecessary;
  for (activation_class ac : input->AC)
    if (active_class(ac) && !necessary(ac)) unnecessary.push_back(ac);
  return unnecessary;
}

bool GlobalModel::active_class(activation_class ac) {
  operation o = input->activation_class_representative[ac];
  return a(o).val();
}

bool GlobalModel::necessary(activation_class ac) {
  for (operation o : input->O)
    for (instruction i1 : input->activation_class_instructions[ac])
      if (i1 == input->instructions[o][i(o).val()]) return true;
  return false;
}

void GlobalModel::
apply_solution_and_deactivate(GlobalModel * gs,
                              vector<activation_class> & acs) {
  for (operand p : input->P) {
    if (input->global_operand[p]) {
      assert(gs->ry(p).assigned());
      assert(gs->x(p).assigned());
      assert(gs->slack(p).assigned());
      constraint(x(p) == gs->x(p));
      constraint(ry(p) == gs->ry(p));
      constraint(slack(p) == gs->slack(p));
    }
  }
  for (activation_class ac : input->AC) {
    operation o = input->activation_class_representative[ac];
    assert(gs->a(o).assigned());
    if (contains(acs, ac)) // Deactivate activation class represented by o
      constraint(!a(o));
    else
      constraint(a(o) == gs->a(o));
  }
}
