/*
 *  Main authors:
 *    Roberto Castaneda Lozano <rcas@sics.se>
 *
 *  Contributing authors:
 *    Gabriel Hjort Blindell <ghb@kth.se>
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


#include <iostream>
#include <fstream>
#include <cstring>
#include <string>
#include <cmath>

#ifdef GRAPHICS
#include <QtGui>
#include <QtScript/QScriptEngine>
#endif

#include <gecode/search.hh>
#include <gecode/driver.hh>

#ifdef GRAPHICS
#include <gecode/gist.hh>
#endif

#include "common/definitions.hpp"
#include "models/parameters.hpp"
#include "models/options.hpp"
#include "models/simplemodel.hpp"
#include "models/globalmodel.hpp"
#include "models/localmodel.hpp"
#include "procedures/globalprocedures.hpp"
#include "procedures/localprocedures.hpp"

#ifndef GRAPHICS
#include <json/value.h>
#include <json/reader.h>
#endif

#ifdef GRAPHICS
#include "inspectors/registerarrayinspector.hpp"
#include "inspectors/issuecycleinspector.hpp"
#include "inspectors/liverangeinspector.hpp"
#include "inspectors/assignmentinspector.hpp"
#include "inspectors/allocationinspector.hpp"
#include "inspectors/livedurationinspector.hpp"
#include "inspectors/selectioninspector.hpp"
#include "inspectors/operandassignmentinspector.hpp"
#include "inspectors/resourceconsumptioninspector.hpp"
#include "inspectors/dataflowinspector.hpp"
#include "inspectors/alignmentinspector.hpp"
#include "inspectors/alignmentpartitioninspector.hpp"
#include "inspectors/operandallocationinspector.hpp"
#include "inspectors/congruenceallocationinspector.hpp"
#include "inspectors/precedenceinspector.hpp"
#include "inspectors/precedencematrixinspector.hpp"
#include "inspectors/usersinspector.hpp"
#include "inspectors/operandlatencyinspector.hpp"
#endif

using namespace Gecode;
using namespace std;

class ResultData {

public:

  GlobalModel * solution;
  bool proven;
  long long int fail;
  long long int it_fail;
  long long int node;
  long long int it_node;
  int presolver_time;
  int presolving_time;
  int solving_time;
  int it_solving_time;

  ResultData(GlobalModel * solution, bool proven, long long int it_fail,
             long long int it_node, int presolver_time, int presolving_time,
             int solving_time, int it_solving_time) {
    this->solution = solution;
    this->proven = proven;
    fail = -1;
    this->it_fail = it_fail;
    node = -1;
    this->it_node = it_node;
    this->presolver_time = presolver_time;
    this->presolving_time = presolving_time;
    this->solving_time = solving_time;
    this->it_solving_time = it_solving_time;
  }
};

class GlobalData {

public:

  int global_n_int_vars;
  int global_n_bool_vars;
  int global_n_set_vars;

  GlobalData(int global_n_int_vars0,
             int global_n_bool_vars0,
             int global_n_set_vars0) :
    global_n_int_vars(global_n_int_vars0),
    global_n_bool_vars(global_n_bool_vars0),
    global_n_set_vars(global_n_set_vars0) {}

};

string produce_json(const ResultData& rd,
                    const GlobalData& gd,
                    unsigned int it_num = 0)
{
    string json;
    if (rd.solution) json = rd.solution->solution_to_json();
    else             json = "{}";
    stringstream ss;
    if (rd.solution) ss << ", ";
    ss << "\"has_solution\": " << (rd.solution ? "true" : "false");
    ss << ", \"proven\": " << (rd.proven ? "true" : "false");
    if (rd.fail >= 0) {
      ss << ", \"failures\": " << rd.fail;
    }
    if (rd.it_fail >= 0) {
      ss << ", \"it_failures\": " << rd.it_fail;
    }
    if (rd.node >= 0) {
      ss << ", \"nodes\": " << rd.node;
    }
    if (rd.it_node >= 0) {
      ss << ", \"it_nodes\": " << rd.it_node;
    }
    if (it_num == 0) {
      if (rd.presolver_time >= 0) {
        ss << ", \"presolver_time\": " << rd.presolver_time;
      }
      ss << ", \"presolving_time\": " << rd.presolving_time;
    }
    if (rd.solving_time >= 0) {
      ss << ", \"solving_time\": " << rd.solving_time;
    }
    if (rd.it_solving_time >= 0) {
      ss << ", \"it_solving_time\": " << rd.it_solving_time;
    }
    ss << ", \"global_int_variables\": " << gd.global_n_int_vars;
    ss << ", \"global_bool_variables\": " << gd.global_n_bool_vars;
    ss << ", \"global_set_variables\": " << gd.global_n_set_vars;
    string more_json = ss.str();
    json.insert(json.find_last_of("}"), more_json);
    return json;
}

void emit_local(LocalModel * local, unsigned long int iteration, string prefix) {
  block b = local->b;
  ofstream fout;
  fout.open(
    (prefix + ".i" + to_string(iteration) + ".b" + to_string(b) + ".json")
    .c_str());
  fout << "{" << endl;
  Parameters binput = local->input->make_local(b);
  fout << binput.emit_json();
  fout << init(init(local->emit_json()));
  fout << endl << "}" << endl;
  fout.close();
}

void emit_output(GlobalModel * base, vector<ResultData> & results,
                 GlobalData & gd, IterationState & state, string prefix,
                 GIST_OPTIONS * go) {

  assert(!results.empty());

  ResultData best_rd = results.back();
  // The last result with solution is always the best
  for (ResultData rd : results)
    if (rd.solution) best_rd = rd;

  // Accumulated statistics for the best ResultData
  best_rd.fail = 0;
  best_rd.node = 0;
  for (ResultData rd : results) {
    best_rd.fail += rd.it_fail;
    best_rd.node += rd.it_node;
  }
  best_rd.it_fail = -1;
  best_rd.it_node = -1;

  for (ResultData rd : results) {
    if (rd.solving_time > best_rd.solving_time)
      best_rd.solving_time = rd.solving_time;
  }
  best_rd.it_solving_time = -1;

  if (best_rd.solution) {

#ifdef GRAPHICS
    if (base->options->gist_solution()) Gist::dfs(best_rd.solution, *go);
#endif

    int best_val = best_rd.solution->cost().val(),
        min_val  = base->cost().min(),
        max_val  = base->input->maxf;
    double rog = ((((double)(best_val - min_val))/(double)min_val) * 100.0),
           imp = ((((double)(max_val - best_val))/(double)best_val) * 100.0);
    if (base->options->verbose()) {
      cerr << fixed << setprecision(1);
      cerr << (best_rd.proven ? "optimal" : "best found")
           << " solution has cost " << best_val << endl;
      if (!best_rd.proven) {
        cerr << "lower bound: " << min_val
             << ", optimality gap: " << rog << "%" << endl;
      }
      cerr << "upper bound: " << max_val
           << ", improvement: " << imp << "%" << endl;
    }
  }

  if (base->options->output_file() == "") {
    cout << produce_json(best_rd, gd) << endl;
  } else {
    ofstream fout;
    if (base->options->output_every_iteration()) {
      // If we want to output all iterations, fill the solutions vector with NULL
      // for the missing iterations
      while(!state.stop(base->options)) {
        results.push_back(ResultData(NULL, false, 0, 0, best_rd.presolver_time,
                                     best_rd.presolving_time,
                                     best_rd.solving_time, 0));
        state.next(base->options);
      }
      for (unsigned int i = 0; i < results.size(); i++) {
        fout.open ((prefix + "." + to_string(i) + ".out.json").c_str());
        fout << produce_json(results[i], gd, i);
        fout.close();
      }
    }
    fout.open(base->options->output_file());
    fout << produce_json(best_rd, gd);
    fout.close();
  }


}

bool has_solution(vector<ResultData> & results) {
  for (ResultData rd : results) {
    if (rd.solution) {
      return true;
    }
  }
  return false;
}

int main(int argc, char* argv[]) {

  int argc0 = argc;
  vector<string> argv0;
  for (int i = 0; i < argc; i++) argv0.push_back(argv[i]);

  ModelOptions options;
  options.parse(argc, argv);

  if (argc < 2) {
    options.help();
    cout << endl;
    exit(EXIT_FAILURE);
  }

  if (strlen(options.instance()) == 0) {
    cerr << "Null input file, original program arguments:" << endl;
    for (int i = 0; i < argc0; i++) {
      cerr << "arg" << i << ": " << argv0[i] << endl;
    }
    cerr << "Remaining program arguments:" << endl;
    for (int i = 0; i < argc; i++) {
      cerr << "arg" << i << ": " << argv[i] << endl;
    }
    exit(EXIT_FAILURE);
  }

#ifdef GRAPHICS
  QApplication *app = new QApplication(argc, argv, false);
#endif

  string name(options.instance());
  string prefix = name.substr(0,name.find(".json"))
                      .substr(0,name.find(".ext"));
  ifstream fin;
  fin.open(name.c_str(), ios::in);
  if (fin.fail()) {
    cerr << "Failed to open " << name << ": " << strerror(errno) << endl;
#ifdef GRAPHICS
    cerr << "Working directory: "
         << QDir::currentPath().toStdString() << endl;
#endif
    exit(EXIT_FAILURE);
  }
  string json_input ((std::istreambuf_iterator<char>(fin)),
                     (std::istreambuf_iterator<char>()));
  fin.close();
  if (fin.fail()) {
    cerr << "Failed to close " << name << ": " << strerror(errno) << endl;
    exit(EXIT_FAILURE);
  }

#ifdef GRAPHICS
  QScriptValue root;
  QScriptEngine engine;
  root = engine.evaluate("(" + QString::fromStdString(json_input) + ")");
  if (engine.hasUncaughtException()) {
    QScriptValue val = engine.uncaughtException();
    if (val.isError()) {
      cerr << "Failed to parse " << name << ": "
           << val.toString().toStdString() << " at line "
           << engine.uncaughtExceptionLineNumber() << endl
           << "Backtrace: "
           << engine.uncaughtExceptionBacktrace().join("\n").toStdString()
           << endl;
    }
    exit(EXIT_FAILURE);
  }
  app->exit();
  delete app;
#else
  Json::Value root;
  Json::Reader reader;
  if (!reader.parse(json_input, root)) {
    cerr << "Failed to parse " << name << endl <<
      reader.getFormatedErrorMessages();
    exit(EXIT_FAILURE);
  }
#endif

  Parameters input(root);

  bool single_block = input.B.size() == 1;
  int presolver_time = 0;
#ifdef GRAPHICS
  {
    QScriptValue property = root.property("presolver_time");
    if (property.isValid()) {
      presolver_time = property.toInt32();
    }
  }
#else
  if (root.isMember("presolver_time")) {
    presolver_time = root["presolver_time"].asInt();
  }
#endif

  GIST_OPTIONS * go = new GIST_OPTIONS(),
               * lo  = new GIST_OPTIONS();

#ifdef GRAPHICS

  // Options for global Gist visualization

  GlobalRegisterArrayInspector * grai = new GlobalRegisterArrayInspector();
  go->inspect.click(grai);
  GlobalIssueCycleInspector * gici = new GlobalIssueCycleInspector();
  go->inspect.click(gici);
  GlobalLiveRangeInspector * glri = new GlobalLiveRangeInspector();
  go->inspect.click(glri);
  GlobalAssignmentInspector * gassi = new GlobalAssignmentInspector();
  go->inspect.click(gassi);
  GlobalAllocationInspector * galli = new GlobalAllocationInspector();
  go->inspect.click(galli);
  Gist::Print<GlobalModel> * gprp =
    new Gist::Print<GlobalModel>("Problem variables");
  go->inspect.click(gprp);
  GlobalSelectionInspector * gsi = new GlobalSelectionInspector();
  go->inspect.click(gsi);
  GlobalLiveDurationInspector * gldi = new GlobalLiveDurationInspector();
  go->inspect.click(gldi);
  GlobalOperandAssignmentInspector * goassi =
    new GlobalOperandAssignmentInspector();
  go->inspect.click(goassi);
  GlobalResourceConsumptionInspector * grci =
    new GlobalResourceConsumptionInspector();
  go->inspect.click(grci);
  GlobalDataFlowInspector * gdfi = new GlobalDataFlowInspector();
  go->inspect.click(gdfi);
  GlobalAlignmentInspector * goai = new GlobalAlignmentInspector();
  go->inspect.click(goai);
  GlobalAlignmentPartitionInspector * goapi =
    new GlobalAlignmentPartitionInspector();
  go->inspect.click(goapi);
  GlobalOperandAllocationInspector * goali =
    new GlobalOperandAllocationInspector();
  go->inspect.click(goali);
  GlobalCongruenceAllocationInspector * gcai =
    new GlobalCongruenceAllocationInspector();
  go->inspect.click(gcai);
  GlobalUsersInspector * gui = new GlobalUsersInspector();
  go->inspect.click(gui);
  GlobalOperandLatencyInspector * goli = new GlobalOperandLatencyInspector();
  go->inspect.click(goli);
  Gist::VarComparator<GlobalModel> *gprc =
    new Gist::VarComparator<GlobalModel>("Compare problem and secondary variables");
  go->inspect.compare(gprc);

  // Options for local Gist visualization

  LocalRegisterArrayInspector * lrai = new LocalRegisterArrayInspector();
  lo->inspect.click(lrai);
  LocalIssueCycleInspector * lici = new LocalIssueCycleInspector();
  lo->inspect.click(lici);
  LocalLiveRangeInspector * llri = new LocalLiveRangeInspector();
  lo->inspect.click(llri);
  LocalAssignmentInspector * lassi = new LocalAssignmentInspector();
  lo->inspect.click(lassi);
  LocalAllocationInspector * lalloi = new LocalAllocationInspector();
  lo->inspect.click(lalloi);
  Gist::Print<LocalModel> * lprp =
    new Gist::Print<LocalModel>("Problem variables");
  lo->inspect.click(lprp);
  Gist::VarComparator<LocalModel> * lprc =
    new Gist::VarComparator<LocalModel>("Compare problem and secondary variables");
  lo->inspect.compare(lprc);
  LocalSelectionInspector * lsi = new LocalSelectionInspector();
  lo->inspect.click(lsi);
  LocalLiveDurationInspector * lldi = new LocalLiveDurationInspector();
  lo->inspect.click(lldi);
  LocalOperandAssignmentInspector * loassi =
    new LocalOperandAssignmentInspector();
  lo->inspect.click(loassi);
  LocalResourceConsumptionInspector * lrci =
    new LocalResourceConsumptionInspector();
  lo->inspect.click(lrci);
  LocalPrecedenceInspector * lpi;
  lpi = new LocalPrecedenceInspector();
  LocalPrecedenceMatrixInspector * lpmi;
  lpmi = new LocalPrecedenceMatrixInspector();
  if (!options.disable_precedence_variables()) {
    lo->inspect.click(lpi);
    lo->inspect.click(lpmi);
  }
  LocalUsersInspector * lui = new LocalUsersInspector();
  lo->inspect.click(lui);
  LocalOperandLatencyInspector * loli = new LocalOperandLatencyInspector();
  lo->inspect.click(loli);
  LocalDataFlowInspector * ldfi = new LocalDataFlowInspector();
  lo->inspect.click(ldfi);

#endif

  vector<ResultData> results;
  vector<vector<LocalModel *> > local_solutions;
  for (unsigned int b = 0; b < input.B.size(); b++)
    local_solutions.push_back(vector<LocalModel *>());

  // Best global cost so far
  int best_cost = Int::Limits::max;

  // Whether the solver has proven optimality or that there is no solution
  bool proven = false;

  IterationState state(options.initial_aggressiveness(), false);

  Support::Timer t;
  t.start();

  // Base global space to accumulate bounds and nogoods: not to search
  GlobalModel * base = new GlobalModel(&input, &options, IPL_DOM);
  GlobalData gd(base->n_int_vars, base->n_bool_vars, base->n_set_vars);

  if (input.O.size() > options.solving_threshold()) {
    double execution_time = t.stop();
    if (options.verbose()) {
      cerr << "giving up: number of operations (" << input.O.size()
           << ") exceeds solving threshold (" << options.solving_threshold()
           << ")" << endl;
      cerr << "execution time: " << execution_time << " ms" << endl;
    }
    results.push_back(ResultData(NULL, false, 0, 0, presolver_time,
                                 0, 0, execution_time));
    emit_output(base, results, gd, state, prefix, go);
    exit(0);
  }

  Gecode::SpaceStatus ss = base->status();
  assert(ss != SS_FAILED); // At this point the problem should be solvable

  if (options.verbose())
    cerr << global() << "variables: "
         << gd.global_n_int_vars << " int, "
         << gd.global_n_bool_vars << " bool, "
         << gd.global_n_set_vars << " set" << endl;

  // Post cost upper bound
  base->post_upper_bound(input.maxf);
  Gecode::SpaceStatus ss1 = base->status();
  if (ss1 == SS_FAILED) { // The problem has no solution
    double execution_time = t.stop();
    if (options.verbose()) {
      cerr << global()
           << "proven absence of solutions with cost less or equal than "
           << input.maxf << endl;
      cerr << "execution time: " << execution_time << " ms" << endl;
    }
    results.push_back(ResultData(NULL, true, 1, 1, presolver_time,
                                 0, 0, execution_time));
    emit_output(base, results, gd, state, prefix, go);
    exit(EXIT_SUCCESS);
  } else {
    results.push_back(ResultData(NULL, false, 0, 0, presolver_time, 0, 0, 0));
  }

  if (options.verbose())
    cerr << global() << "cost: " << base->cost() << endl;

  // Failure and node count for all problems
  long long int total_failed = 0;
  long long int total_nodes = 0;

  Support::Timer t_pre;
  t_pre.start();

  if (!options.disable_presolving()) {

    presolve_effective_callee_saved_spilling(base);

    if (input.optimize_resource != ISSUE_CYCLES)
      presolve_minimum_consumption(base);

    Gecode::SpaceStatus ss2 = base->status();
    if (ss2 == SS_FAILED) { // The problem has no solution
      double execution_time = t.stop();
      if (options.verbose()) {
        cerr << pre()
             << "proven absence of solutions with cost less or equal than "
             << input.maxf << endl;
        cerr << "execution time: " << execution_time << " ms" << endl;
      }
      results.push_back(ResultData(NULL, true, 0, 0, presolver_time, 0, 0,
                                   execution_time));
      emit_output(base, results, gd, state, prefix, go);
      exit(EXIT_SUCCESS);
    }

    if (!single_block) presolve_relaxation(base, go, lo);

    Gecode::SpaceStatus ss3 = base->status();
    if (ss3 == SS_FAILED) { // The problem has no solution
      double execution_time = t.stop();
      if (options.verbose()) {
        cerr << pre()
             << "proven absence of solutions with cost less or equal than "
             << input.maxf << endl;
        cerr << "execution time: " << execution_time << " ms" << endl;
      }
      results.push_back(ResultData(NULL, true, 0, 0, presolver_time, 0, 0,
                                   execution_time));
      emit_output(base, results, gd, state, prefix, go);
      exit(EXIT_SUCCESS);
    }

    presolve_global_cluster_impact(base, lo);

    presolve_global_shaving(base);

    presolve_shaving(base);

    Gecode::SpaceStatus ss4 = base->status();
    if (ss4 == SS_FAILED) { // The problem has no solution
      double execution_time = t.stop();
      if (options.verbose()) {
        cerr << pre()
             << "proven absence of solutions with cost less or equal than "
             << input.maxf << endl;
        cerr << "execution time: " << execution_time << " ms" << endl;
      }
      results.push_back(ResultData(NULL, true, 0, 0, presolver_time, 0, 0,
                                   execution_time));
      emit_output(base, results, gd, state, prefix, go);
      exit(EXIT_SUCCESS);
    }

    presolve_global_activation_shaving(base);

    Gecode::SpaceStatus ss5 = base->status();
    if (ss5 == SS_FAILED) { // The problem has no solution
      double execution_time = t.stop();
      if (options.verbose()) {
        cerr << pre()
             << "proven absence of solutions with cost less or equal than "
             << input.maxf << endl;
        cerr << "execution time: " << execution_time << " ms" << endl;
      }
      results.push_back(ResultData(NULL, true, 0, 0, presolver_time, 0, 0,
                                   execution_time));
      emit_output(base, results, gd, state, prefix, go);
      exit(EXIT_SUCCESS);
    }

  }

  int presolving_time = ceil(t_pre.stop());

  Support::Timer t_solver;
  t_solver.start();

  if (options.decomposition()) {

    unsigned long int iteration = 0;

    // Whether the current iteration is a "deactivation iteration"
    bool deactivation = false;
    // Activation classes to be deactivated
    vector<activation_class> acs;

    // Latest result of solving each local problem
    map<block, SolverResult> latest_result;
    for (block b : base->input->B) latest_result[b] = UNKNOWN;

    while(!proven && !state.stop(&options)) {
      Support::Timer t_it;
      t_it.start();

      // Failure and node count for the iteration
      long long int iteration_failed = 0;
      long long int iteration_nodes = 0;

      if (options.verbose()) {
        if (deactivation) {
          cerr << global() << "deactivation of unnecessary operations" << endl;
        } else {
          cerr << global()
               << "solving problem (i: " << iteration << ", state: " << state << ")"
               << endl;
        }
      }

      if (options.verbose())
        cerr << global() << "cost: " << base->cost() << endl;

      // If we are in a deactivation iteration, a solution from the previous
      // iteration must be available
      assert(!deactivation || results.back().solution);

      // Solve the global problem
      Solution<GlobalModel> gs =
        deactivation ?
        deactivate(base, results.back().solution, acs) :
        solve_global(base, state, best_cost, go, iteration);
      iteration_failed += gs.failures;
      iteration_nodes += gs.nodes;

      if (gs.result == SOME_SOLUTION) {

        if (options.verbose()) {
          cerr << global() << "found solution (failures: " << gs.failures
               << ", nodes: " << gs.nodes << ")" << endl;
        }

        if (base->options->solve_global_only()) exit(EXIT_SUCCESS);

        // Sort the local problems lexicographically in descending
        // <solution score, block frequency>. The idea is to solve the
        // hardest problems first.
        vector<block> blocks = sorted_blocks(base->input, latest_result);

        // Solve the local problems
        bool found_all_local = true, unsat = false;
        for (block b : blocks) {

          // Base local space to accumulate bounds while the portfolio is applied
          Solution<LocalModel> ls = local_problem(gs.solution, b);

          if (ls.result != UNSATISFIABLE) {
            LocalModel * base_local = ls.solution;
            Gecode::SpaceStatus lss = base_local->status();
            assert(lss != SS_FAILED);

            // Possibly output local problem
            if (base->options->output_local_problems() &&
                (!solved(base_local, local_solutions[b]) || single_block))
              emit_local(base_local, iteration, prefix);

            ls =
              solved(base_local, local_solutions[b]) && !single_block ?
              // If the local problem is already solved, fetch the cached solution
              fetch_solution(base_local, local_solutions[b]) :
              // Otherwise solve
              solve_local_portfolio(base_local, lo, iteration);
            delete base_local;
            iteration_failed += ls.failures;
            iteration_nodes += ls.nodes;
          }

          // Store the result of solving the local problem
          latest_result[b] = ls.result;

          if (ls.result == LIMIT) {
            found_all_local = false;
            if (options.verbose())
              cerr << local(b) << "could not find solution" << endl;
            break;
          } else if (ls.result == UNSATISFIABLE) {
            found_all_local = false;
            unsat = true;
            if (options.verbose())
              cerr << local(b) << "could not find solution (unsatisfiable)"
                   << endl;
            break;
          } else {
            // Extend the global solution with the newly found local solution
            GECODE_NOT_NULL(ls.solution);
            gs.solution->apply_solution(ls.solution);
            if (ls.result == CACHED_SOLUTION) {
              if (options.verbose())
                cerr << local(b) << "repeated solution" << endl;
            } else if (ls.result == OPTIMAL_SOLUTION) {
              local_solutions[b].push_back(ls.solution);
              base->post_local_solution_cost(ls.solution);
            }
            // Propagate the cost of the found local solution in the global problem
            SpaceStatus status = gs.solution->status();
            if (status == Gecode::SS_FAILED) {
              found_all_local = false;
              unsat = true;
              if (options.verbose())
                cerr << local(b)
                     << "could not extend global solution: too costly" << endl;
              break;
            }
          }

        }

        // Forbid the current global solution
        base->post_different_solution(gs.solution, unsat);
        base->status();

        // All local solutions could be found, combine them and tighten
        // the objective function
        if (found_all_local && gs.solution->status() != SS_FAILED) {

          // Update best found cost
          assert(gs.solution->cost().val() <= best_cost);
          best_cost = gs.solution->cost().val();

          if (options.verbose()) {
            cerr << global() << "found full solution (cost: " << best_cost
                 << ", failures: " << iteration_failed
                 << ", nodes: " << iteration_nodes << ")" << endl;
          }

          // Store the newly found solution
          if (deactivation) {
            results.pop_back();
            deactivation = false;
          }
          results.push_back(ResultData(gs.solution, false, iteration_failed,
                                       iteration_nodes, presolver_time,
                                       presolving_time, t_solver.stop(),
                                       t_it.stop()));

          // Tighten the objective function
          best_cost--;
          base->post_upper_bound(best_cost);
          base->status();

          acs = gs.solution->unnecessary_activation_classes();
          if (!acs.empty()) deactivation = true;

        } else {
          if (options.verbose())
            cerr << global() << "could not find a full solution" << endl;
          delete gs.solution;
          results.push_back(ResultData(NULL, false, iteration_failed,
                                       iteration_nodes, presolver_time,
                                       presolving_time, t_solver.stop(),
                                       t_it.stop()));
          deactivation = false;
        }

      } else if (gs.result == LIMIT) {
        if (options.verbose())
          cerr << global() << "limit" << endl;
        results.push_back(ResultData(NULL, false, iteration_failed,
                                     iteration_nodes, presolver_time,
                                     presolving_time, t_solver.stop(),
                                     t_it.stop()));
        deactivation = false;
      } else {
        if (options.verbose())
          cerr << global() << "failure" << endl;
        results.push_back(ResultData(NULL, false, iteration_failed,
                                     iteration_nodes, presolver_time,
                                     presolving_time, t_solver.stop(),
                                     t_it.stop()));
        deactivation = false;
      }

      if (base->options->solve_global_only()) exit(EXIT_SUCCESS);

      if (gs.result == UNSATISFIABLE || base->status() == SS_FAILED) {
        // If the global problem is unsatisfiable and there is some solution we
        // have found the optimal one
        if (has_solution(results)) {
          if (options.verbose())
            cerr << global() << "found optimal solution" << endl;
          // Remove results from the back until a solution is found
          while (!results.back().solution) results.pop_back();
          assert(results.back().solution);
        } else {
          if (options.verbose()) {
            cerr << global()
                 << "proven absence of solutions with cost less or equal than "
                 << input.maxf << endl;
          }
        }
        proven = true;
        results.back().proven = true;
      }

      total_failed += iteration_failed;
      total_nodes += iteration_nodes;

      if (has_solution(results) && options.first_solution()) break;

      if (!deactivation) { // Update aggressiveness
        state.next(&options);
        iteration++;
      }
    }
  }

  if (!proven && options.monolithic() &&
      input.O.size() <= options.monolithic_threshold()) { // Run monolithic solver
    if (options.verbose())
      cerr << monolithic() << "running monolithic solver..." << endl;
    Solution<GlobalModel> ms = solve_monolithic(base, go);
    double solving_time = t.stop();
    total_failed += ms.failures;
    total_nodes += ms.nodes;
    if (ms.result == OPTIMAL_SOLUTION) {
      if (options.verbose())
        cerr << monolithic() << "found optimal solution" << endl;
      results.push_back(ResultData(ms.solution, true, ms.failures, ms.nodes,
                                   presolver_time, presolving_time,
                                   solving_time, solving_time));
    } else if (ms.result == UNSATISFIABLE) {
      // If the global problem is unsatisfiable and there is some solution we
      // have found the optimal one
      if (has_solution(results)) {
        if (options.verbose())
          cerr << monolithic() << "found optimal solution" << endl;
        // Remove results from the back until a solution is found
        while (!results.back().solution) results.pop_back();
        assert(results.back().solution);
      } else {
        if (options.verbose()) {
          cerr << monolithic()
               << "proven absence of solutions with cost less or equal than "
               << input.maxf << endl;
        }
      }
      results.back().proven = true;
    }
  }

  double execution_time = t.stop();

  if (options.verbose()) {
    cerr << "execution time: " << execution_time << " ms" << endl;
    cerr << "total failed nodes in all problems: " << total_failed << endl;
    cerr << "total searched nodes in all problems: " << total_nodes << endl;
  }

  emit_output(base, results, gd, state, prefix, go);

#ifdef GRAPHICS
  delete grai;
  delete gici;
  delete glri;
  delete gassi;
  delete galli;
  delete gprp;
  delete gsi;
  delete gldi;
  delete goassi;
  delete grci;
  delete gdfi;
  delete goai;
  delete goapi;
  delete goali;
  delete gcai;
  delete gprc;
  delete lrai;
  delete lici;
  delete llri;
  delete lassi;
  delete lalloi;
  delete lprp;
  delete lprc;
  delete lsi;
  delete lldi;
  delete loassi;
  delete lrci;
  delete lpi;
  delete lpmi;
  delete ldfi;
#endif

  delete go;
  delete lo;

  exit(EXIT_SUCCESS);

}
