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

#ifndef GRAPHICS
#include "third-party/jsoncpp/json/value.h"
#include "third-party/jsoncpp/json/reader.h"
#endif

#include "common/definitions.hpp"
#include "models/parameters.hpp"
#include "presolver/presolver-options.hpp"
#include "presolver/presolve.hpp"

using namespace Gecode;
using namespace std;

string write_json_vector_int(string label, vector<int> v)
{
  stringstream ss;
  string sep = "[";

  ss << "\"";
  ss << label;
  ss << "\": ";
  for (int x : v) {
    ss << sep; ss << x; sep = ",";
  }
  ss << "]\n";
  return ss.str();
}

string produce_json(Parameters &input, int presolver_time)
{
  stringstream json;
  json << "{"
       << emit_json_line("B", input.B)
       << emit_json_line("O", input.O)
       << emit_json_line("P", input.P)
       << emit_json_line("T", input.T)
       << emit_json_line("block", input.oblock)
       << emit_json_line("operands", input.operands)
       << emit_json_line("temps", input.temps)
       << emit_json_line("use", input.use)
       << emit_json_line("adjacent", input.adjacent)
       << emit_json_line("preassign", input.preassign)
       << emit_json_line("width", input.width)
       << emit_json_line("freq", input.freq)
       << emit_json_line("minlive", input.minlive)
       << emit_json_line("dep", input.dep)
       << emit_json_line("preschedule", input.prescheduled)
       << emit_json_line("I", input.I)
       << emit_json_line("R", input.R)
       << emit_json_line("dist", input.dist)
       << emit_json_line("class", input.rclass)
       << emit_json_line("atoms", input.atoms)
       << emit_json_line("instructions", input.instructions)
       << emit_json_line("lat", input.lat)
       << emit_json_line("bypass", input.bypass)
       << emit_json_line("cap", input.cap)
       << emit_json_line("con", input.con)
       << emit_json_line("dur", input.dur)
       << emit_json_line("off", input.off)
       << emit_json_line("aligned", input.aligned)
       << emit_json_line("adist", input.adist)
       << emit_json_line("packed", input.packed)
       << emit_json_line("exrelated", input.exrelated)
       << emit_json_line("table", input.table)
       << emit_json_line("activators", input.activators)
       << emit_json_line("E", input.E)
       << emit_json_line("optimize_dynamic", input.optimize_dynamic)
       << emit_json_line("optimize_resource", input.optimize_resource)
       << emit_json_line("maxf", input.maxf)
       << emit_json_line("tmp", input.tmp)
       << emit_json_line("definer", input.definer)
       << emit_json_line("operation", input.oper)
       << emit_json_line("def_opr", input.def_opr)
       << emit_json_line("congr", input.congr)
       << emit_json_line("ops", input.ops)
       << emit_json_line("RS", input.RS)
       << emit_json_line("maxc", input.maxc)
       << emit_json_line("type", input.type)
       << emit_json_line("copyrel", input.copyrel)
       << emit_json_line("interchangeable", input.interchangeable)
       << emit_json_line("cfg", input.cfg)
       << emit_json_line("clusters", input.clusters)
       << emit_json_line("space", input.space)
       << emit_json_line("range", input.range)
       << emit_json_line("home", input.home)
       << emit_json_line("infinite", input.infinite)
       << emit_json_line("bounded", input.bounded)
       << emit_json_line("insname", input.insname)
       << emit_json_line("atomname", input.atomname)
       << emit_json_line("classname", input.classname)
       << emit_json_line("spacename", input.spacename)
       << emit_json_line("callersaved", input.callersaved)
       << emit_json_line("calleesaved", input.calleesaved)
       << emit_json_line("optional_min", input.optional_min)
       << emit_json_line("active_tables", input.active_tables)
       << emit_json_line("tmp_tables", input.tmp_tables)
       << emit_json_line("nogoods", input.nogoods)
       << emit_json_line("nogoods2", input.nogoods2)
       << emit_json_line("precedences", input.precedences)
       << emit_json_line("precedences2", input.precedences2)
       << emit_json_line("before", input.before)
       << emit_json_line("before2", input.before2)
       << emit_json_line("across", input.across)
       << emit_json_line("set_across", input.set_across)
       << emit_json_line("domops", input.domops)
       << emit_json_line("last_use", input.last_use)
       << emit_json_line("infassign", input.infassign)
       << emit_json_line("domuses", input.domuses)
       << emit_json_line("precs", input.precs)
       << emit_json_line("assignhints", input.assignhints)
       << emit_json_line("dominates", input.dominates)
       << emit_json_line("difftemps", input.difftemps)
       << emit_json_line("diffregs", input.diffregs)
       << emit_json_line("calleesaved_spill", input.calleesaved_spill)
       << emit_json_line("strictly_congr", input.strictly_congr)
       << emit_json_line("predecessors", input.predecessors)
       << emit_json_line("successors", input.successors)
       << emit_json_line("value_precede_chains", input.value_precede_chains)
       << emit_json_line("quasi_adjacent", input.quasi_adjacent)
       << emit_json_line("long_latency_index", input.long_latency_index)
       << emit_json_line("long_latency_def_use", input.long_latency_def_use)
       << emit_json_line("subsumed_resources", input.subsumed_resources)
       << emit_json_line("freq_scale", input.freq_scale)
       << emit_json_line_last("presolver_time", presolver_time)
       << "}\n";
  return json.str();
}

int main(int argc, char* argv[]) {

  int argc0 = argc;
  vector<string> argv0;
  for (int i = 0; i < argc; i++) argv0.push_back(argv[i]);

  PresolverOptions options;
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
  ofstream fout;
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

  Support::Timer t_presolver;
  t_presolver.start();
  presolve(input, options);
  double t_pre_ms = t_presolver.stop();
  if (options.output_file().empty()) {
    cout << produce_json(input, t_pre_ms);
  } else {
    fout.open(options.output_file());
    fout << produce_json(input, t_pre_ms);
    fout.close();
  }
  if (options.verbose())
    cerr << "presolved in " << t_pre_ms << " ms" << endl;

  exit(EXIT_SUCCESS);

}
