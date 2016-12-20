/*
 *  Main authors:
 *    Roberto Castaneda Lozano <rcas@sics.se>
 *
 *  This file is part of Unison, see http://unison-code.github.io
 *
 *  Copyright (c) 2016, SICS Swedish ICT AB
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


#include "globalprocedures.hpp"

void presolve_effective_callee_saved_spilling(GlobalModel * base) {
  if (base->options->verbose())
    cerr << pre()
         << "computing effective callee-saved copies..." << endl;
  for (operation o : base->input->callee_saved_stores) {
    GlobalModel * g = (GlobalModel*) base->clone();
    g->post_effective_callee_saved_spilling(o);
    Gecode::SpaceStatus ss = g->status();
    if (ss == SS_FAILED) return; // Proof that there is no solution
    if (g->a(o).assigned()) {
      if (g->a(o).val()) {
        base->post_active_operation(o);
      } else {
        base->post_inactive_operation(o);
      }
      Gecode::SpaceStatus ss1 = base->status();
      if (ss1 == SS_FAILED) return; // Proof that there is no solution
    }
    delete g;
  }
}

void presolve_minimum_consumption(GlobalModel * base) {
  resource r = base->input->optimize_resource;
  assert(r != ISSUE_CYCLES);
  for (block b : base->input->B) {
    int n = base->input->optional_min[b];
    if (n > 0) {
      // Minimum total consumption of mandatory operations
      int mancon = 0;
      for (operation o : base->input->mandatory[b]) {
        int mincon = std::numeric_limits<int>::max();
        for (IntVarValues ii(base->i(o)); ii(); ++ii) {
          instruction i = base->input->instructions[o][ii.val()];
          if (base->input->con[i][r] < mincon)
            mincon = base->input->con[i][r];
        }
        mancon += mincon;
      }
      // Minimum consumption among all instructions that can implement
      // non-mandatory operations
      int mincon = std::numeric_limits<int>::max();
      for (operation o : base->input->ops[b])
        if (!contains(base->input->mandatory[b], o)) {
          // If the non-mandatory operation is already active, decrease
          if (base->a(o).assigned() && base->a(o).val()) {
            n--;
          } else {
            for (IntVarValues ii(base->i(o)); ii(); ++ii) {
              if (ii.val() > 0) {
                instruction i = base->input->instructions[o][ii.val()];
                if (base->input->con[i][r] < mincon)
                  mincon = base->input->con[i][r];
              }
            }
          }
        }
      base->constrain_local_cost(b, IRT_GQ, mancon + mincon * n);

      Gecode::SpaceStatus ss = base->status();
      if (ss == SS_FAILED) return; // Proof that there is no solution
    }
  }
}

void
presolve_relaxation(GlobalModel * base, GIST_OPTIONS * go, GIST_OPTIONS * lo) {
  for (operation co : concat({NULL_OPERATION},
                             base->input->callee_saved_stores)) {
    for (activation_class ac : concat({NULL_ACTIVATION_CLASS},
                                      base->input->AC)) {
      operation oo = ac == NULL_ACTIVATION_CLASS ? NULL_OPERATION :
        base->input->activation_class_representative[ac];
      if (base->options->verbose()) {
        cerr << pre() << "computing lower bounds"
             << (co == NULL_OPERATION ? "" : (" assuming a(o" + to_string(co) + ")"))
             << (oo == NULL_OPERATION ? "" : (" assuming a(o" + to_string(oo) + ")"))
             << "..." << endl;
      }
      GlobalModel * g = (GlobalModel*) base->clone();
      g->post_active_operation(co);
      g->post_active_operation(oo);
      g->status();

#ifdef GRAPHICS
      if (base->options->gist_presolving() && base->options->gist_global())
        Gist::dfs(g, *go);
#endif

      set<block> ac_blocks;
      if (ac != NULL_ACTIVATION_CLASS)
        for (operation o : base->input->activation_class_operations[ac])
          ac_blocks.insert(base->input->oblock[o]);

      for (block b : base->input->B) {

        // If the activation class does not affect b, we can skip the relaxation
        if (ac != NULL_ACTIVATION_CLASS && !contains(ac_blocks, b)) continue;

        LocalModel * ls = g->make_local(b, IPL_DOM);
        ls->post_minimum_cost_branchers();
#ifdef GRAPHICS
        if (base->options->gist_presolving() && base->options->gist_block() == b)
          Gist::bab(ls, *lo);
#endif
        Search::Stop * preStop = new_stop(2000, base->options);
        Search::Options preOptions;
        preOptions.stop = preStop;
        BAB<LocalModel> e(ls, preOptions);
        while (LocalModel* nextls = e.next()) {
          LocalModel * oldls = ls;
          ls = nextls;
          delete oldls;
        }
        int lb = 0;
        if (!preStop->stop(e.statistics(), preOptions) &&
            ls->status() == SS_SOLVED)
          lb = ls->cost().val();
        if (ls != NULL) delete ls;
        delete preStop;
        base->post_lower_bounds(co, oo, b, lb);
      }
      delete g;
      Gecode::SpaceStatus ss = base->status();
      if (ss == SS_FAILED) return; // Proof that there is no solution
    }
  }
  return;
}

void presolve_shaving(GlobalModel * base) {
  for (block b : base->input->B) {
    if (base->input->ops[b].size() > base->options->shaving_threshold()) {
      if (base->options->verbose())
        cerr << pre() << "large block (" << base->input->ops[b].size()
             << " operations), skipping shaving" << endl;
      continue;
    }
    if (base->options->verbose())
      cerr << pre()
           << "computing instruction nogoods for b" << b << "..." << endl;
    Search::Stop * stop =
      new_stop(base->options->global_shaving_limit(), base->options);
    Search::Statistics stats;
    Search::Options dummyOpts;
    LocalModel * l = base->make_local(b, IPL_DOM);
    Gecode::SpaceStatus lss = l->status();
    if (lss == SS_FAILED) { // A local problem failed, force failure in global
      base->constrain_local_cost(b, IRT_LE, -1);
      assert(base->status() == SS_FAILED);
      return;
    }
    for (int cost = l->cost().min() + 1; cost <= l->cost().max(); cost++) {
      LocalModel * l1 = (LocalModel*) l->clone();
      l1->constrain_cost(IRT_LE, cost);
      Gecode::SpaceStatus ss = l1->status();
      if (ss == SS_FAILED) {
        delete l1;
        base->constrain_local_cost(b, IRT_GQ, cost);
      } else {
        vector<InstructionAssignment> forbidden =
          shave_instructions(l1, stop, stats);
        delete l1;
        for (InstructionAssignment fb : forbidden)
          base->post_instruction_nogood(cost, fb);
        if (forbidden.empty()) break;
      }
      if (stop->stop(stats, dummyOpts)) {
        if (base->options->verbose())
          cerr << pre() << "limit" << endl;
        break;
      }
    }
    delete l;
    delete stop;
    Gecode::SpaceStatus ss = base->status();
    if (ss == SS_FAILED) return;
  }
}

void presolve_global_cluster_impact(GlobalModel * base, GIST_OPTIONS * lo) {
  if (base->options->verbose())
    cerr << pre()
         << "computing lower bounds for cluster connections..." << endl;
  for (global_cluster gc : base->input->GC) {
    if (base->options->verbose())
      cerr << pre() << "computing lower bounds for "
           << show(base->input->clusters[gc],",","p","{}") << "..." << endl;
    presolve_global_cluster_impact(base, gc, true, lo);
    presolve_global_cluster_impact(base, gc, false, lo);
  }
}

void presolve_global_cluster_impact(
     GlobalModel * base, global_cluster gc, bool connect, GIST_OPTIONS * lo) {
  Parameters * input = base->input;
  GlobalModel * g = (GlobalModel*) base->clone();
  g->post_cluster_connection_decision(gc, connect);
  g->status();
  operand p = input->clusters[gc][0];

  vector<operand> touched;
  for (operand p : input->P)
    if (!base->x(p).assigned() && g->x(p).assigned())
      touched.push_back(p);

  // blocks with optional non-copy operations related to the touched operands
  set<block> affected;
    for (operand p : touched) {
      for (temporary t : input->real_temps[p]) {
        set<operand> qs;
        for (operand q : input->users[t]) qs.insert(q);
        qs.insert(input->definer[t]);
        for (operand q : qs) {
          operation o = input->oper[q];
          if (input->type[o] != COPY && !input->delimiter[o] &&
              input->instructions[o][0] == NULL_INSTRUCTION &&
              contains(input->temps[q], t)) {
            affected.insert(input->oblock[o]);
          }
        }
      }
    }

  for (block b : affected) {
    LocalModel * ls = g->make_local(b, IPL_DOM);
    ls->post_minimum_cost_branchers();
#ifdef GRAPHICS
    if (base->options->gist_presolving() && base->options->gist_block() == b)
      Gist::bab(ls, *lo);
#endif
    Search::Stop * preStop = new_stop(2000, base->options);
    Search::Options preOptions;
    preOptions.stop = preStop;
    BAB<LocalModel> e(ls, preOptions);
    while (LocalModel* nextls = e.next()) {
      LocalModel * oldls = ls;
      ls = nextls;
      delete oldls;
    }
    int lb = 0;
    if (!preStop->stop(e.statistics(), preOptions) &&
      ls->status() == SS_SOLVED)
      lb = ls->cost().val();
    if (ls != NULL) delete ls;
    delete preStop;
    if (lb > base->f(b).min()) {
      base->post_connection_lower_bound(p, connect, b, lb);
      base->status();
    }
  }

  delete g;

}

void presolve_global_shaving(GlobalModel * base) {
  int l = base->cost().min(), h = base->cost().max();
  while (l != h) {
    int m = (l + h)/2;
    GlobalModel * g = (GlobalModel*) base->clone();
    g->post_upper_bound(m);
    Gecode::SpaceStatus ss = g->status();
    delete g;
    if (ss == SS_FAILED) { // cost >= m
      l = m + 1;
    } else {
      h = m;
    }
  }
  if (l > base->cost().min()) {
    base->post_lower_bound(l);
    base->status();
    if (base->options->verbose())
      cerr << "[pre]\t increased cost lower bound: " << l << endl;
  }
}

void presolve_global_activation_shaving(GlobalModel * base) {

  for (activation_class ac : base->input->AC) {
    if (base->options->verbose()) {
      cerr << pre() << "computing activation nogoods for "
           << show(base->input->activation_class_operations[ac],",","o","{}")
           << "..." << endl;
    }
    operation o = base->input->activation_class_representative[ac];
    GlobalModel * g = (GlobalModel*) base->clone();
    g->post_active_operation(o);
    Gecode::SpaceStatus ss = SS_BRANCH;
    if (g->status() == SS_FAILED) {
      base->post_inactive_operation(o);
      ss = base->status();
      if (base->options->verbose())
        cerr << pre() << "activation class disabled" << endl;
    } else if (g->cost().min() > base->cost().min()) {
      base->post_activation_nogood(o, g->cost().min());
      ss = base->status();
      if (base->options->verbose())
        cerr << pre() << "activation class increases cost lower bound" << endl;
    }
    delete g;
    if (ss == SS_FAILED) return; // Proof that there is no solution
  }

}

unsigned long int
global_limit(Parameters * input, ModelOptions * options, int best) {
  unsigned long int globalLimit =
    options->global_budget() * input->O.size();
  return globalLimit +
    (best == Int::Limits::max ? options->global_setup_limit() : 0);
}

Solution<GlobalModel>
solve_global(GlobalModel * base, IterationState & state, int best,
             GIST_OPTIONS * go, int iteration) {

  // Create global problem with aggressiveness a
  GlobalModel * g = (GlobalModel*) base->clone();
  g->set_aggressiveness(state.a);
  g->set_connect_first(state.cf);
  g->post_branchers();
  if (base->input->B.size() == 1) g->post_callee_saved_branchers();

#ifdef GRAPHICS
  if (base->options->gist_global() &&
     (base->options->gist_iteration() == -1 ||
      base->options->gist_iteration() == iteration)) Gist::dfs(g, *go);
#endif

  // Global options
  Search::Stop * globalStop =
    new_stop(global_limit(base->input, base->options, best), base->options);
  Search::Options globalOptions;
  globalOptions.stop = globalStop;

  // Global search engine
  DFS<GlobalModel> e(g, globalOptions);
  delete g;

  // Solve the global problem
  GlobalModel * g1 = e.next();

  SolverResult r;
  if (g1 == NULL)
    r = globalStop->stop(e.statistics(), globalOptions) ? LIMIT : UNSATISFIABLE;
  else
    r = SOME_SOLUTION;

  delete globalStop;

  if (!base->options->disable_global_shaving() && r == SOME_SOLUTION)
    r = shave_local_costs(g1);

  // Return result, solution (if applicable) and failures
  return Solution<GlobalModel>(r, g1, e.statistics().fail, e.statistics().node);
}

Solution<GlobalModel>
deactivate(GlobalModel * base, GlobalModel * gs,
           vector<activation_class> & acs) {
  GlobalModel * gsd = (GlobalModel*) base->clone();
  gsd->apply_solution_and_deactivate(gs, acs);
  Gecode::SpaceStatus ss = gsd->status();
  SolverResult r = ss == SS_FAILED ? UNKNOWN : SOME_SOLUTION;
  return Solution<GlobalModel>(r, gsd, 0, 0);
}

SolverResult shave_local_costs(GlobalModel * base) {
  for (block b : base->input->B) {
    Search::Stop * stop =
      new_stop(base->options->post_global_shaving_limit(), base->options);
    Search::Statistics stats;
    Search::Options dummyOpts;
    for (int cost = base->f(b).min(); cost < base->f(b).max(); cost++) {
      GlobalModel * g1 = (GlobalModel*) base->clone();
      g1->constrain_local_cost(b, IRT_GR, cost);
      if (g1->status() == SS_FAILED) {
        delete g1;
        base->constrain_local_cost(b, IRT_LQ, cost);
        Gecode::SpaceStatus ss = base->status();
        stats.fail++;
        if (ss == SS_FAILED) {
          delete base;
          if (base->options->verbose()) {
            cerr << global()
                 << "failed due to cost shaving: f(b" << b << ") <= "
                 << cost << endl;
          }
          return SHAVING_FAILURE;
        }
        break;
      }
      stats.fail++;
      delete g1;
      if (stop->stop(stats, dummyOpts)) {
        if (base->options->verbose())
          cerr << global() << "cost shaving limit for b" << b << endl;
        break;
      }
    }
    delete stop;
  }
  return SOME_SOLUTION;
}

Solution<GlobalModel> solve_monolithic(GlobalModel * base, GIST_OPTIONS * go) {

  GlobalModel * m = (GlobalModel*) base->clone();
  m->post_complete_branchers();

#ifdef GRAPHICS
  if (base->options->gist_global()) Gist::bab(m, *go);
#endif

  unsigned int scale = base->input->O.size() / 10;
  Search::Cutoff* c = Search::Cutoff::luby(scale);
  if (base->options->verbose())
    cerr << monolithic() << "Luby scale: " << scale << endl;
  unsigned int FACTOR = 10;
  double limit =
    base->options->monolithic_budget() * base->input->O.size() * FACTOR;
  Search::Stop * monolithicStop = new_stop(limit, base->options);
  if (base->options->verbose())
    cerr << monolithic() << "time limit: " << limit << endl;
  Search::Options o;
  o.cutoff = c;
  o.nogoods_limit = 128;
  o.stop = monolithicStop;

  RBS<GlobalModel, BAB> e(m, o);

  bool found_solution = false;
  while (GlobalModel* nextm = e.next()) {
    found_solution = true;
    if (base->options->verbose())
      cerr << monolithic()
           << "found solution with cost " << nextm->cost() << endl;
    GlobalModel * oldm = m;
    m = nextm;
    delete oldm;
  }

  SolverResult r;
  if (monolithicStop->stop(e.statistics(), o))
    r = found_solution ? SOME_SOLUTION : LIMIT;
  else
    r = found_solution ? OPTIMAL_SOLUTION : UNSATISFIABLE;

  return Solution<GlobalModel>(
           r, found_solution ? m : NULL,
           e.statistics().fail,
           e.statistics().node);

}

string global() { return "[global] "; }

string pre() { return "[pre]\t "; }

string monolithic() { return "[mthic]\t "; }
