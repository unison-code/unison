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


#include "localprocedures.hpp"

Solution<LocalModel> local_problem(GlobalModel * g1, block b) {
  IntPropLevel ipl = IPL_DOM;
  if (g1->input->ops[b].size() > g1->options->consistency_threshold()) {
    ipl = IPL_BND;
    if (g1->options->verbose())
      cerr << local(b) << "large block (" << g1->input->ops[b].size()
           << " operations), reducing consistency level" << endl;
  }
  LocalModel * l = g1->make_local(b, ipl);
  Gecode::SpaceStatus lss = l->status();
  SolverResult r;
  r = lss == SS_FAILED ? UNSATISFIABLE : UNKNOWN;
  return Solution<LocalModel>(r, l, 0, 0);
}

Solution<LocalModel>
solve_local_portfolio(LocalModel * base, GIST_OPTIONS * lo, int iteration) {
  if (base->options->custom_portfolio())
    return solve_custom_portfolio(base, lo, iteration);
  else
    return solve_generic_portfolio(base, lo, iteration);
}

Solution<LocalModel>
solve_generic_portfolio(LocalModel * base, GIST_OPTIONS * lo, int iteration) {

  block b = base->b;

#ifdef GRAPHICS
  if ((base->options->gist_block() == base->b) &&
      (base->options->gist_iteration() == -1 ||
       base->options->gist_iteration() == iteration)) {
    LocalModel * lg = (LocalModel*) base->clone();
    lg->post_branchers(base->options->local_portfolio()[0]);
    Gist::bab(lg, *lo);
  }
#endif

  // Create local problem (TODO: do we need this? cannot we just use base?)
  LocalModel * l = (LocalModel*) base->clone();

  // TODO: apply local shaving? (see strange phenomenon with
  // adpcm.rawcaudio.main without presolver where it seems detrimental)

  // Local options
  Search::Stop * localStop =
    new_stop(base->options->local_limit(), base->options);
  Search::Options localOptions;
  localOptions.stop = localStop;
  int n = base->options->local_portfolio().size();
  localOptions.assets = n;
  localOptions.threads = n;

  // Local portfolio meta-engine
  PBS<LocalModel, BAB> e(l, localOptions);

  // Solve the local problem for the given search strategy
  bool found_local_solution = false;
  while (LocalModel* nextl = e.next()) {
    found_local_solution = true;
    LocalModel * oldl = l;
    l = nextl;
    delete oldl;
  }

  SolverResult r;
  if (localStop->stop(e.statistics(), localOptions)) {
    r = found_local_solution ? SOME_SOLUTION : LIMIT;
  } else if (found_local_solution) {
    r = OPTIMAL_SOLUTION;
  } else {
    r = UNSATISFIABLE;
  }
  delete localStop;

  // Result, solution (if applicable) and failures
  Solution<LocalModel> ls(r, l, e.statistics().fail, e.statistics().node);

  if (base->options->verbose()) {
    if (ls.result == SOME_SOLUTION) {
      cerr << local(b) << "found limit solution (cost: "
           << ls.solution->cost().val()
           << ", failures: " << ls.failures
           << ", nodes: " << ls.nodes
           << ")" << endl;
    } else if (ls.result == LIMIT) {
      cerr << local(b) << "limit"
           << " (failures: " << ls.failures
           << ", nodes: " << ls.nodes
           << ")" << endl;
    } else if (ls.result == OPTIMAL_SOLUTION) {
      cerr << local(b) << "found optimal solution (cost: "
           << ls.solution->cost().val()
           << ", failures: " << ls.failures
           << ", nodes: " << ls.nodes
           << ")" << endl;
    } else if (ls.result == UNSATISFIABLE) {
      cerr << local(b) << "proven unsatisfiable"
           << " (failures: " << ls.failures
           << ", nodes: " << ls.nodes
           << ")" << endl;
    }
  }

  return ls;

}

Solution<LocalModel>
solve_custom_portfolio(LocalModel * base, GIST_OPTIONS * lo, int iteration) {

  // Failure and node count for all search strategies
  long long int total_failed = 0;
  long long int total_nodes = 0;

  block b = base->b;

  LocalModel * best = NULL;

  bool optimal = false;
  bool unsat = false;

  bool improved = true;

  for (char search : base->options->local_portfolio()) {

    Gecode::SpaceStatus ss = base->status();
    assert(ss != SS_FAILED);

    Solution<LocalModel> ls(UNSATISFIABLE, NULL, 0, 0);

    if (improved && !base->options->disable_local_shaving()) {
      if (base->input->ops[b].size() > base->options->shaving_threshold()) {
        cerr << local(b) << "large block (" << base->input->ops[b].size()
             << " operations), skipping shaving" << endl;
      } else {
        Search::Stop * stop =
          new_stop(base->options->local_shaving_limit(), base->options);
        Search::Statistics stats;
        Search::Options dummyOpts;
        base->post_instruction_assignment_constraints(
          shave_instructions(base, stop, stats));
        ss = base->status();
        if (stop->stop(stats, dummyOpts)) {
          cerr << local(b) << "shaving limit" << endl;
        }
        delete stop;
      }
    }

    if (ss != SS_FAILED) {

      ls = solve_local(base, search, lo, iteration);
      total_failed += ls.failures;
      total_nodes += ls.nodes;

      if (ls.result == SOME_SOLUTION || ls.result == OPTIMAL_SOLUTION) {
        if (best != NULL) {
          assert(ls.solution->cost().val() < best->cost().val());
          delete best;
        }
        best = ls.solution;
      }
    }

    if (best != NULL) {
      base->constrain_cost(IRT_LE, best->cost().val());
      Gecode::SpaceStatus ss2 = base->status();
      // If the base space fails after constraining the cost, the solution found
      // was indeed optimal
      if (ss2 == SS_FAILED) {
        ls.result = OPTIMAL_SOLUTION;
        ls.solution = best;
      }
    }

    if (ls.result == SOME_SOLUTION) {
      improved = true;
      cerr << local(b) << "found limit solution (cost: " << ls.solution->cost().val()
           << ", failures: " << ls.failures
           << ", nodes: " << ls.nodes << ", branching: " << search << ")"
           << endl;
    } else if (ls.result == LIMIT) {
      improved = false;
      cerr << local(b) << "limit (failures: " << ls.failures
           << ", nodes: " << ls.nodes << ", branching: " << search << ")" << endl;
    } else if (ls.result == OPTIMAL_SOLUTION) {
      improved = true;
      cerr << local(b) << "found optimal solution (cost: " << ls.solution->cost().val()
           << ", failures: " << ls.failures << ", nodes: " << ls.nodes
           << ", branching: " << search << ")" << endl;
    } else if (ls.result == UNSATISFIABLE && best != NULL) {
      improved = false;
      cerr << local(b) << "proven optimality (failures: " << ls.failures
           << ", nodes: " << ls.nodes << ", branching: " << search << ")" << endl;
    } else if (ls.result == UNSATISFIABLE) {
      improved = false;
      cerr << local(b) << "proven unsatisfiable (failures: " << ls.failures
           << ", nodes: " << ls.nodes << ", branching: " << search << ")" << endl;
    }

    if (ls.result == OPTIMAL_SOLUTION ||
        (ls.result == UNSATISFIABLE && best != NULL)) {
      optimal = true;
      break;
    }

    if (ls.result == UNSATISFIABLE && best == NULL) {
      unsat = true;
      break;
    }

    if (base->options->first_solution() &&
        (ls.result == SOME_SOLUTION || ls.result == OPTIMAL_SOLUTION))
      break;

  }

  SolverResult r1;

  if (optimal) r1 = OPTIMAL_SOLUTION;
  else if (best != NULL) r1 = SOME_SOLUTION;
  else if (unsat) r1 = UNSATISFIABLE;
  else r1 = LIMIT;

  // Return result, solution (if applicable) and failures
  return Solution<LocalModel>(r1, best, total_failed, total_nodes);

}

Solution<LocalModel>
solve_local(LocalModel * base, char search, GIST_OPTIONS * lo, int iteration) {

  // Create local problem with the given search strategy
  LocalModel * l = (LocalModel*) base->clone();
  switch (search) {
  case AGGRESSIVE_SEARCH:
    l->post_aggressive_branchers();
    break;
  case TRIVIAL_SEARCH:
    l->post_trivial_branchers();
    break;
  case MINIMUM_COST_SEARCH:
    l->post_minimum_cost_branchers();
    break;
  case FAIL_FIRST_SEARCH:
    l->post_fail_first_branchers();
    break;
  case CONSERVATIVE_SEARCH:
    l->post_conservative_branchers();
    break;
  default:
    GECODE_NEVER;
  }

#ifdef GRAPHICS
  if ((base->options->gist_block() == base->b) &&
      (base->options->gist_iteration() == -1 ||
       base->options->gist_iteration() == iteration)) {
    Gist::bab(l, *lo);
  }
#endif

  // Local options
  Search::Stop * localStop =
    new_stop(base->options->local_limit(), base->options);
  Search::Options localOptions;
  localOptions.stop = localStop;

  // Local search engine
  BAB<LocalModel> e(l, localOptions);

  // Solve the local problem for the given search strategy
  bool found_local_solution = false;
  while (LocalModel* nextl = e.next()) {
    found_local_solution = true;
    LocalModel * oldl = l;
    l = nextl;
    delete oldl;
  }

  SolverResult r;
  if (localStop->stop(e.statistics(), localOptions)) {
    r = found_local_solution ? SOME_SOLUTION : LIMIT;
  } else if (found_local_solution) {
    r = OPTIMAL_SOLUTION;
  } else {
    r = UNSATISFIABLE;
  }
  delete localStop;

  // Return result, solution (if applicable) and failures
  return Solution<LocalModel>(r, l, e.statistics().fail, e.statistics().node);
}

string local(block b) {
  stringstream s;
  s << "[b" << b << "]\t ";
  return s.str();
}
