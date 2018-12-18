/*
 *  Main authors:
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
 *
 *  Contributing authors:
 *    Gabriel Hjort Blindell <ghb@kth.se>
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
#include <gecode/kernel.hh>

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
#include "third-party/jsoncpp/json/value.h"
#include "third-party/jsoncpp/json/reader.h"
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

class LocalJob : public Support::Job<Solution<LocalModel> > {
protected:
  // Base local space to accumulate bounds while the portfolio is applied
  Solution<LocalModel> ls;
  // visualization options (if any)
  GIST_OPTIONS * lo;
  // current iteration
  int iteration;
  // local solutions in earlier iterations
  vector<vector<LocalModel *> > * local_solutions;
public:
  LocalJob(Solution<LocalModel> ls0, GIST_OPTIONS * lo0, int iteration0,
           vector<vector<LocalModel *> > * local_solutions0) :
    ls(ls0), lo(lo0), iteration(iteration0),
    local_solutions(local_solutions0) {}
  virtual Solution<LocalModel> run(int) {
    block b = ls.solution->b;
    if (ls.result != UNSATISFIABLE) {
      LocalModel * base_local = ls.solution;
      Gecode::SpaceStatus lss = base_local->status();
      assert(lss != SS_FAILED);
      bool single_block = base_local->input->B.size() == 1;
      ls = solved(base_local, (*local_solutions)[b]) && !single_block ?
        // if the local problem is already solved, fetch the cached solution
        fetch_solution(base_local, (*local_solutions)[b]) :
        // otherwise solve
        solve_local_portfolio(base_local, lo, iteration);
      delete base_local;
    }
    if (ls.solution->options->verbose()) {
      if (ls.result == LIMIT) {
        cerr << local(b) << "could not find solution" << endl;
      } else if (ls.result == UNSATISFIABLE) {
        cerr << local(b) << "could not find solution (unsatisfiable)" << endl;
      } else if (ls.result == CACHED_SOLUTION) {
        cerr << local(b) << "repeated solution" << endl;
      }
    }
    if (ls.result == LIMIT || ls.result == UNSATISFIABLE) {
      throw Support::JobStop<Solution<LocalModel> >(ls);
    }
    return ls;
  }
};

class LocalJobs {
protected:
  // global solution from which the local problems are generated
  Solution<GlobalModel> gs;
  // visualization options (if any)
  GIST_OPTIONS * lo;
  // current iteration
  int iteration;
  // local solutions in earlier iterations
  vector<vector<LocalModel *> > * local_solutions;
  // blocks sorted in descending priority
  vector<block> blocks;
  // current block index
  unsigned int k;
public:
  LocalJobs(Solution<GlobalModel> gs0, GIST_OPTIONS * lo0, int iteration0,
            vector<vector<LocalModel *> > * local_solutions0,
            vector<block> blocks0) :
    gs(gs0), lo(lo0), iteration(iteration0), local_solutions(local_solutions0),
    blocks(blocks0), k(0) {}
  bool operator ()(void) const {
    return k < blocks.size();
  }
  LocalJob * job(void) {
    // FIXME: fork jobs in the order of blocks[b], use blocks[k] instead of k
    block b = k;
    // Base local space to accumulate bounds while the portfolio is applied
    Solution<LocalModel> ls = local_problem(gs.solution, b);
    k++;
    return new LocalJob(ls, lo, iteration, local_solutions);
  }
};

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
                    unsigned int N,
                    unsigned int it_num)
{
    string json;
    if (rd.solution) json = rd.solution->solution_to_json();
    else             json = "{}";
    stringstream ss;
    if (rd.solution) ss << ", ";
    ss << "\"solver\": \"gecode-solver\"";
    ss << ", \"has_solution\": " << (rd.solution ? "true" : "false");
    ss << ", \"proven\": " << (rd.proven ? "true" : "false");
    vector<int> ones;
    init_vector(ones, N, -1);
    ss << ", \"cost\": "
       << (rd.solution ? show(var_vector(rd.solution->cost())) : show(ones));
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
      ss << ", \"gecode_presolving_time\": " << rd.presolving_time;
    }
    if (rd.solving_time >= 0) {
      ss << ", \"solver_time\": " << rd.solving_time;
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

string unsat_report(const GlobalModel * base) {
  stringstream ss;
  ss << "proven absence of solutions with cost less or equal than "
     << show(base->input->maxf, ", ", "", "{}");
  return ss.str();
}

string cost_status_report(GlobalModel * base, const GlobalModel * sol) {
  vector<double> imps, ogs;
  for (unsigned int n = 0; n < base->input->N; n++) {
    int cost_ub  = base->input->maxf[n] + (n == (base->input->N - 1) ? 1 : 0),
        max_cost = sol->cost()[n].max();
    double imp = ((((double)(cost_ub - max_cost)) / (double)max_cost) * 100.0),
      og  = optimality_gap(base, sol, n);
    imps.push_back(imp);
    ogs.push_back(og);
  }
  stringstream ss;
  ss << "cost: " << sol->cost();
  if (sol != base) {
    ss << ", improvement: ";
    vector<string> percents;
    for (double imp : imps) {
      stringstream ss0;
      ss0 << fixed << setprecision(2);
      ss0 << imp << "%";
      percents.push_back(ss0.str());
    }
    ss << show(percents, ", ", "", "{}");
  }
  if (std::any_of(ogs.begin(), ogs.end(), [](double og){return og > 0.0;})) {
    ss << ", optimality gap: ";
    vector<string> percents;
    for (double og : ogs) {
      stringstream ss0;
      ss0 << fixed << setprecision(2);
      ss0 << og << "%";
      percents.push_back(ss0.str());
    }
    ss << show(percents, ", ", "", "{}");
  }
  return ss.str();
}

void emit_output_exit(GlobalModel * base, const vector<ResultData> & results,
                      const GlobalData & gd, GIST_OPTIONS * go) {
  (void)go;

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

    if (base->options->verbose()) {
      cerr << (best_rd.proven ? "optimal" : "best found") << " solution has "
           << cost_status_report(base, best_rd.solution) << endl;
    }
    if (base->options->emit_improvement()) {
      cerr << fixed << setprecision(2);
      cerr << cost_status_report(base, best_rd.solution) << endl;
    }
  }

  emit_lower_bound(base, best_rd.proven);

  if (base->options->output_file() == "") {
    cout << produce_json(best_rd, gd, base->input->N, 0) << endl;
  } else {
    ofstream fout;
    fout.open(base->options->output_file());
    fout << produce_json(best_rd, gd, base->input->N, 0);
    fout.close();
  }

  if (!(base->options->all_solutions() && !best_rd.proven)) exit(EXIT_SUCCESS);

}

void timeout_exit(GlobalModel * base, const vector<ResultData> & results,
                  const GlobalData & gd, GIST_OPTIONS * go, double time) {
  if (base->options->verbose())
    cerr << global() << "timeout (" << time << " ms)" << endl;
  emit_output_exit(base, results, gd, go);
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
  Json::CharReaderBuilder reader;
  std::stringstream json_input_stream;
  json_input_stream << json_input;
  std::string errs;
  if (!Json::parseFromStream(reader, json_input_stream, &root, &errs)) {
    cerr << "Failed to parse " << name << endl << errs;
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
  vector<int> best_cost;
  for (unsigned int n = 0; n < input.N; n++)
    best_cost.push_back(Int::Limits::max);

  // Whether the solver has proven optimality or that there is no solution
  bool proven = false;

  IterationState state(options.initial_aggressiveness(), false);

  Support::Timer t;
  t.start();

  // Base global space to accumulate bounds and nogoods: not to search
  GlobalModel * base = new GlobalModel(&input, &options, IPL_DOM);
  GlobalData gd(base->n_int_vars, base->n_bool_vars, base->n_set_vars);

  emit_lower_bound(base);

  double execution_time = t.stop();
  if (execution_time > options.timeout()) {
    results.push_back(ResultData(NULL, false, 0, 0, presolver_time,
                                 0, 0, execution_time));
    timeout_exit(base, results, gd, go, t.stop());
  }

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
    emit_output_exit(base, results, gd, go);
  }

  Gecode::SpaceStatus ss = status_lb(base);
  assert(ss != SS_FAILED); // At this point the problem should be solvable

  if (options.verbose())
    cerr << global() << "variables: "
         << gd.global_n_int_vars << " int, "
         << gd.global_n_bool_vars << " bool, "
         << gd.global_n_set_vars << " set" << endl;

  // Post cost upper bound
  base->post_upper_bound(input.maxf);
  Gecode::SpaceStatus ss1 = status_lb(base);

  // Emit initial optimality gap
  emit_initial_gap(base, base);

  if (ss1 == SS_FAILED) { // The problem has no solution
    double execution_time = t.stop();
    if (options.verbose()) {
      cerr << global() << unsat_report(base) << endl;
      cerr << "execution time: " << execution_time << " ms" << endl;
    }
    results.push_back(ResultData(NULL, true, 1, 1, presolver_time,
                                 0, 0, execution_time));
    emit_output_exit(base, results, gd, go);
  } else {
    results.push_back(ResultData(NULL, false, 0, 0, presolver_time, 0, 0, 0));
  }

  if (options.verbose())
    cerr << global() << cost_status_report(base, base) << endl;

  if (optimality_gap(base, base, 0) <= base->options->acceptable_gap() &&
      base->options->acceptable_gap() > 0) {
    if (options.verbose())
      cerr << global() << "reached acceptable optimality gap" << endl;
    emit_output_exit(base, results, gd, go);
  }

  if (t.stop() > options.timeout())
    timeout_exit(base, results, gd, go, t.stop());

  // Failure and node count for all problems
  long long int total_failed = 0;
  long long int total_nodes = 0;

  Support::Timer t_pre;
  t_pre.start();

  if (!options.disable_presolving()) {

    double tpre;

    Support::Timer t_pre1;
    t_pre1.start();
    presolve_effective_callee_saved_spilling(base);
    tpre = t_pre1.stop();
    if (options.verbose()) {
      cerr << pre() << "presolving time (effective callee-saved spilling): "
           << ceil(tpre) << " ms" << endl;
    }

    if (status_lb(base) == SS_FAILED) { // The problem has no solution
      double execution_time = t.stop();
      if (options.verbose()) {
        cerr << pre() << unsat_report(base) << endl;
        cerr << "execution time: " << execution_time << " ms" << endl;
      }
      results.push_back(ResultData(NULL, true, 0, 0, presolver_time, 0, 0,
                                   execution_time));
      emit_output_exit(base, results, gd, go);
    }

    if (t.stop() > options.timeout())
      timeout_exit(base, results, gd, go, t.stop());

    if (input.optimize_resource[0] != ISSUE_CYCLES) {
      Support::Timer t_pre2;
      t_pre2.start();
      presolve_minimum_consumption(base);
      tpre = t_pre2.stop();
      if (options.verbose()) {
        cerr << pre() << "presolving time (minimum consumption): "
             << ceil(tpre) << " ms" << endl;
      }
    }

    Gecode::SpaceStatus ss2 = status_lb(base);
    if (ss2 == SS_FAILED) { // The problem has no solution
      double execution_time = t.stop();
      if (options.verbose()) {
        cerr << pre() << unsat_report(base) << endl;
        cerr << "execution time: " << execution_time << " ms" << endl;
      }
      results.push_back(ResultData(NULL, true, 0, 0, presolver_time, 0, 0,
                                   execution_time));
      emit_output_exit(base, results, gd, go);
    }

    if (t.stop() > options.timeout())
      timeout_exit(base, results, gd, go, t.stop());

    if (!single_block) {
      Support::Timer t_pre3;
      t_pre3.start();
      presolve_relaxation(base, lo);
      tpre = t_pre3.stop();
      if (options.verbose()) {
        cerr << pre() << "presolving time (relaxation): "
             << ceil(tpre) << " ms" << endl;
      }
    }

    Gecode::SpaceStatus ss3 = status_lb(base);
    if (ss3 == SS_FAILED) { // The problem has no solution
      double execution_time = t.stop();
      if (options.verbose()) {
        cerr << pre() << unsat_report(base) << endl;
        cerr << "execution time: " << execution_time << " ms" << endl;
      }
      results.push_back(ResultData(NULL, true, 0, 0, presolver_time, 0, 0,
                                   execution_time));
      emit_output_exit(base, results, gd, go);
    }

    if (t.stop() > options.timeout())
      timeout_exit(base, results, gd, go, t.stop());

    Support::Timer t_pre4;
    t_pre4.start();
    presolve_global_cluster_impact(base, lo);
    tpre = t_pre4.stop();
    if (options.verbose()) {
      cerr << pre() << "presolving time (global cluster impact): "
           << ceil(tpre) << " ms" << endl;
    }

    if (t.stop() > options.timeout())
      timeout_exit(base, results, gd, go, t.stop());

    Support::Timer t_pre5;
    t_pre5.start();
    presolve_global_shaving(base);
    tpre = t_pre5.stop();
    if (options.verbose()) {
      cerr << pre() << "presolving time (global shaving): "
           << ceil(tpre) << " ms" << endl;
    }

    if (t.stop() > options.timeout())
      timeout_exit(base, results, gd, go, t.stop());

    Support::Timer t_pre6;
    t_pre6.start();
    presolve_shaving(base);
    tpre = t_pre6.stop();
    if (options.verbose()) {
      cerr << pre() << "presolving time (shaving): "
           << ceil(tpre) << " ms" << endl;
    }

    if (t.stop() > options.timeout())
      timeout_exit(base, results, gd, go, t.stop());

    Gecode::SpaceStatus ss4 = status_lb(base);
    if (ss4 == SS_FAILED) { // The problem has no solution
      double execution_time = t.stop();
      if (options.verbose()) {
        cerr << pre() << unsat_report(base) << endl;
        cerr << "execution time: " << execution_time << " ms" << endl;
      }
      results.push_back(ResultData(NULL, true, 0, 0, presolver_time, 0, 0,
                                   execution_time));
      emit_output_exit(base, results, gd, go);
    }

    if (t.stop() > options.timeout())
      timeout_exit(base, results, gd, go, t.stop());

    Support::Timer t_pre7;
    t_pre7.start();
    presolve_global_activation_shaving(base);
    tpre = t_pre7.stop();
    if (options.verbose()) {
      cerr << pre() << "presolving time (global activation shaving): "
           << ceil(tpre) << " ms" << endl;
    }

    Gecode::SpaceStatus ss5 = status_lb(base);
    if (ss5 == SS_FAILED) { // The problem has no solution
      double execution_time = t.stop();
      if (options.verbose()) {
        cerr << pre() << unsat_report(base) << endl;
        cerr << "execution time: " << execution_time << " ms" << endl;
      }
      results.push_back(ResultData(NULL, true, 0, 0, presolver_time, 0, 0,
                                   execution_time));
      emit_output_exit(base, results, gd, go);
    }

  }

  int presolving_time = ceil(t_pre.stop());

  if (options.verbose() && !options.disable_presolving()) {
    cerr << pre() << cost_status_report(base, base) << endl;
    cerr << pre() << "presolving time: " << presolving_time << " ms" << endl;
  }

  if (optimality_gap(base, base, 0) <= base->options->acceptable_gap() &&
      base->options->acceptable_gap() > 0) {
    if (options.verbose())
      cerr << global() << "reached acceptable optimality gap" << endl;
    emit_output_exit(base, results, gd, go);
  }

  if (t.stop() > options.timeout())
    timeout_exit(base, results, gd, go, t.stop());

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

    while(!proven && (options.complete() || !state.stop(&options))) {
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
               << "solving problem (i: " << iteration << ", state: " << state
               << ", cost: " << base->cost() << ")" << endl;
        }
      }

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

        if (t.stop() > options.timeout())
          timeout_exit(base, results, gd, go, t.stop());

        // Sort the local problems lexicographically in descending
        // <solution score, block frequency>. The idea is to solve the
        // hardest problems first.
        vector<block> blocks = sorted_blocks(base->input, latest_result);

        map<block, Solution<LocalModel> > latest_local_solutions;
        set<block> solved_blocks;
        LocalJobs ljs(gs, lo, iteration, &local_solutions, blocks);
        bool found_all_local = true, unsat = false;
        unsigned int top_threads =
          options.total_threads() / options.portfolio_threads();

        // Solve the local problems
        Support::RunJobs<LocalJobs, Solution<LocalModel> > p(ljs, top_threads);
        Solution<LocalModel> ls;
        while (p.run(ls)) {
          int i;
          Solution<LocalModel> fls;
          if (p.stopped(i, fls)) { // job stopped
            block b = fls.solution->b;
            latest_local_solutions[b] = fls;
            solved_blocks.insert(b);
            break;
          } else { // job finished
            block b = ls.solution->b;
            latest_local_solutions[b] = ls;
            solved_blocks.insert(b);
          }
        }

        if (t.stop() > options.timeout())
          timeout_exit(base, results, gd, go, t.stop());

        // Process the local solutions
        for (block b : solved_blocks) {
          Solution<LocalModel> ls = latest_local_solutions[b];
          iteration_failed += ls.failures;
          iteration_nodes += ls.nodes;
          // Store the result of solving the local problem
          latest_result[b] = ls.result;

          if (ls.result == LIMIT) {
            found_all_local = false;
            break;
          } else if (ls.result == UNSATISFIABLE) {
            found_all_local = false;
            unsat = true;
            break;
          } else { // a solution is found
            // Extend the global solution with the newly found local solution
            GECODE_NOT_NULL(ls.solution);
            gs.solution->apply_solution(ls.solution);
            if (ls.result == OPTIMAL_SOLUTION) {
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
        status_lb(base);

        // All local solutions could be found, combine them and tighten
        // the objective function
        if (found_all_local && gs.solution->status() != SS_FAILED) {

          // Update best found cost
          vector<int> new_best_cost = var_vector(gs.solution->cost());
          assert((best_cost == new_best_cost) ||
                 std::lexicographical_compare(
                   &new_best_cost, &new_best_cost + input.N,
                   &best_cost,     &best_cost     + input.N));
          best_cost = new_best_cost;

          if (options.verbose()) {
            cerr << global() << "found full solution "
                 << "(" << cost_status_report(base, gs.solution) << ")" << endl;
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
          best_cost.back()--;
          base->post_upper_bound(best_cost);
          status_lb(base);

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

      if (gs.result == UNSATISFIABLE || status_lb(base) == SS_FAILED) {
        // If the global problem is unsatisfiable and there is some solution we
        // have found the optimal one
        if (has_solution(results)) {
          if (options.verbose())
            cerr << global() << "found optimal solution" << endl;
          // Remove results from the back until a solution is found
          while (!results.back().solution) results.pop_back();
          assert(results.back().solution);
          delete base;
          base = (GlobalModel*) results.back().solution->clone();
        } else {
          if (options.verbose()) {
            cerr << global() << unsat_report(base) << endl;
          }
        }
        proven = true;
        results.back().proven = true;
      }

      total_failed += iteration_failed;
      total_nodes += iteration_nodes;

      if (has_solution(results) && !proven) {
        if (options.all_solutions()) emit_output_exit(base, results, gd, go);
        if (options.first_solution()) break;
      }

      ResultData best = results.back();
      for (ResultData rd : results)
        if (rd.solution) best = rd;
      if (!proven &&
          best.solution &&
          optimality_gap(base, best.solution, 0) <=
          base->options->acceptable_gap() &&
          base->options->acceptable_gap() > 0) {
        if (options.verbose())
          cerr << global() << "reached acceptable optimality gap" << endl;
        break;
      }

      if (t.stop() > options.timeout())
        timeout_exit(base, results, gd, go, t.stop());

      IterationState next_state(state);
      next_state.next(&options);
      if (!proven &&
          next_state.stop(&options) &&
          options.monolithic() &&
          input.O.size() <= options.monolithic_threshold()) { // Run monolithic solver
        if (options.verbose())
          cerr << monolithic() << "running monolithic solver..." << endl;
        Solution<GlobalModel> ms = solve_monolithic(base, go);
        double solving_time = t.stop();
        total_failed += ms.failures;
        total_nodes += ms.nodes;
        if (ms.result == OPTIMAL_SOLUTION) {
          vector<int> ms_sol = var_vector(ms.solution->cost());
          base->post_lower_bound(ms_sol);
          base->post_upper_bound(ms_sol);
          status_lb(base);
          if (options.verbose())
            cerr << monolithic() << "found optimal solution "
                 << "(" << cost_status_report(base, ms.solution) << ")" << endl;
          results.push_back(ResultData(ms.solution, true, ms.failures, ms.nodes,
                                       presolver_time, presolving_time,
                                       solving_time, solving_time));
          proven = true;
        } else if (ms.result == UNSATISFIABLE) {
          // If the global problem is unsatisfiable and there is some solution we
          // have found the optimal one
          if (has_solution(results)) {
            if (options.verbose())
              cerr << monolithic() << "proven optimality" << endl;
            // Remove results from the back until a solution is found
            while (!results.back().solution) results.pop_back();
            assert(results.back().solution);
            vector<int> opt_cost(var_vector(results.back().solution->cost()));
            base->post_lower_bound(opt_cost);
            base->post_upper_bound(opt_cost);
            status_lb(base);
          } else {
            if (options.verbose()) {
              cerr << monolithic() << unsat_report(base) << endl;
            }
          }
          proven = true;
          results.back().proven = true;
        }
      }

      if (!deactivation) { // Update aggressiveness
        state.next(&options);
        if (!proven && options.complete() && state.stop(&options)) {
          state = IterationState(options.initial_aggressiveness(), false);
        }
        iteration++;
      }
    }
  }

  execution_time = t.stop();

  if (options.verbose()) {
    cerr << "execution time: " << execution_time << " ms" << endl;
    cerr << "total failed nodes in all problems: " << total_failed << endl;
    cerr << "total searched nodes in all problems: " << total_nodes << endl;
  }

  emit_output_exit(base, results, gd, go);

}
