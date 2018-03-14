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


#ifndef __SOLVER_PARAMETERS__
#define __SOLVER_PARAMETERS__

#include <iostream>
#include <limits>
#include <algorithm>
#include <limits>
#include <vector>
#include <set>
#include <string>
#include <sstream>

#include <gecode/int.hh>

#ifdef GRAPHICS
#include <QScriptEngine>
#include <QStringList>
#include <QtScript/QScriptValueIterator>
#include <QDebug>
#else // Then we still need an alternative for parsing JSON
#include "third-party/jsoncpp/json/value.h"
#include "third-party/jsoncpp/json/reader.h"
#endif

#include "common/util.hpp"
#include "common/definitions.hpp"

// Mats's code changes
#define MCMOD 1
#define MCSLACK 1

using namespace Gecode;
using namespace std;

class Parameters {

public:

  // Program parameters

  // set of blocks
  vector<block> B;

  // set of operations
  vector<operation> O;

  // set of operands
  vector<operand> P;

  // set of temporaries
  vector<temporary> T;

  // block of each operation
  vector<block> oblock;

  // operands of each operation
  vector<vector<operand> > operands;

  // temporaries that can be connected to each operand
  vector<vector<temporary> > temps;

  // whether each operand is a use
  vector<bool> use;

  // adjacent operands
  vector<vector<operand> > adjacent;

  // pre-assignments from operands to registers
  vector<vector<int> > preassign;

  // number of register atoms that each temporary occupies
  vector<int> width;

  // estimation of the execution frequency of each block
  vector<int> freq;

  // minimum live range duration of each temporary
  vector<int> minlive;

  // fixed dependency graph
  vector<vector<vector<int> > > dep;

  // prescheduling of operations into issue cycles
  vector<vector<int> > prescheduled;

  // Processor parameters

  // set of instructions
  vector<instruction> I;

  // set of processor resources
  vector<instruction> R;

  // minimum issue distances of each fixed dependency and parent instruction
  vector<vector<vector<int> > > dist;

  // register class in which each operation implemented by each instruction
  // accesses its operands
  vector<vector<vector<register_class> > > rclass;

  // atoms of each register class
  vector<vector<register_atom> > atoms;

  // instructions that can implement each operation
  vector<vector<instruction> > instructions;

  // latency of each operand when its operation is implemented by each
  // instruction
  vector<vector<vector<int> > > lat;

  // whether each operand is bypassing when its operation is implemented by
  // each instruction
  vector<vector<vector<bool> > > bypass;

  // capacity of each processor resource
  vector<int> cap;

  // consumption of each processor resource by each instruction
  vector<vector<int> > con;

  // duration of usage of each processor resource by each instruction
  vector<vector<int> > dur;

  // offset of usage of each processor resource by each instruction
  vector<vector<int> > off;

  // aligned operand tuples
  vector<vector<int> > aligned;

  // alignment distance of each aligned operand tuple
  vector<int> adist;

  // packed operand pairs
  vector<vector<operand> > packed;

  // operands related extensionally
  vector<vector<operand> > exrelated;

  // table of register assignments of each related operand pair
  vector<vector<vector<register_atom> > > table;

  // instructions that activate each operation
  vector<vector<instruction> > activators;

  // set of ad-hoc processor constraints
  vector<UnisonConstraintExpr> E;

  // Objective function parameters

  // whether to use block frequencies as weight for the nth objectve
  vector<bool> optimize_dynamic;

  // resource whose consumption is to be optimized for the nth objective
  vector<resource> optimize_resource;

  // upper bound of the nth objective
  vector<int> maxf;

  // frequency scale factor
  double freq_scale;

  // Additional parameters

  // temporaries defined and used within each block
  vector<vector<temporary> > tmp;

  // operand that potentially defines each temporary
  vector<operand> definer;

  // operation that contains each operand
  vector<operation> oper;

  // congruent operand classes
  vector<vector<operand> > congr;

  // operations of each block
  vector<vector<operation> > ops;

  // set of register spaces
  vector<register_space> RS;

  // maximum issue cycle in each block
  vector<int> maxc;

  // type of each operation
  vector<int> type;

  // copy-related operand classes
  vector<vector<operand> > copyrel;

  // sets of interchangeable copies
  vector<vector<operation> > interchangeable;
  // control edges in the block control-flow graph
  vector<vector<block> > cfg;

  // clusters of global operands that must be connected simultaneously
  vector<vector<operand> > clusters;

  // register space of each register class
  vector<register_space> space;

  // atom range of each register space
  vector<vector<register_atom> > range;

  // home register space of each operand
  vector<register_space> home;

  // whether each register space is infinite
  vector<bool> infinite;

  // whether each register space is bounded
  vector<bool> bounded;

  // name of each instruction
  vector<string> insname;

  // name of each atom
  vector<string> atomname;

  // name of each register class
  vector<string> classname;

  // name of each register space
  vector<string> spacename;

  // caller-saved register atoms
  vector<register_atom> callersaved;

  // callee-saved register atoms
  vector<register_atom> calleesaved;

  // Presolver parameters

  // minimum number of active optional operations in each block
  vector<int> optional_min;

  // allowed combinations of active operations
  vector<PresolverActiveTable> active_tables;

  // allowed combinations of active operations and operand temporaries
  vector<PresolverCopyTmpTable> tmp_tables;

  // basic no-goods
  vector<UnisonConstraintExpr> nogoods;

  // additional no-goods
  vector<UnisonConstraintExpr> nogoods2;

  // basic presolver precedences
  vector<UnisonConstraintExpr> precedences;

  // additional presolver precedences
  vector<UnisonConstraintExpr> precedences2;

  // basic presolver partially ordered operands
  vector<PresolverBeforeJSON> before;

  // additional presolver partially ordered operands
  vector<PresolverBeforeJSON> before2;

  // presolver across-call temporaries
  vector<PresolverAcrossJSON> across;

  // presolver across-call temporary sets
  vector<PresolverSetAcross> set_across;

  // presolver sets of interchangeable temporaries
  vector<vector<vector<int> > > domops;

  // presolver killer operands
  vector<operand> last_use;

  // register atoms for temporaries assigned to infinite spaces
  vector<vector<int> > infassign;

  // set of dominated uses
  vector<vector<operand> > domuses;

  // set of precedences
  vector<vector<operation> > precs;

  // set of register assignment hints
  vector<vector<int> > assignhints;

  // set of dominates
  vector<PresolverDominates> dominates;

  // set of difftemps
  vector<vector<operand> > difftemps;

  // set of diffregs
  vector<vector<operand> > diffregs;

  // [MC] set of calleesaved_spill
  vector<vector<operation> > calleesaved_spill;

  // [MC] strictly congruent operand classes
  vector<vector<operand> > strictly_congr;

  // [MC] set of predecessors or successors
  vector<PresolverPred> predecessors;

  // [MC] set of predecessors or successors
  vector<PresolverSucc> successors;

  // [MC] set of value_precede_chain
  vector<PresolverValuePrecedeChain> value_precede_chains;

  // [MC] quasi_adjacent operands
  vector<vector<operand> > quasi_adjacent;

  // [MC] long_latency index: [in operands, its relevant def-uses, out operands, its relevant def-uses]
  vector<vector<vector<int> > > long_latency_index;
  
  // [MC] long_latency use-def hazards
  vector<vector<operand> > long_latency_def_use;

  // [MC] subsumed resources
  vector<vector<resource> > subsumed_resources;

  // [MC] register atom domain of each temp
  vector<vector<register_atom> > temp_domain;

  Parameters(JSONVALUE root);

  // emit parameters in JSON format
  string emit_json();

  // parameters for block b only
  Parameters make_local(block b);

  // Compute derived parameters
  void compute_derived();

  // Derived parameters

  // register classes
  vector<register_class> RC;

  // operands of each block
  vector<vector<operand> > ope;

  // congruences
  vector<congruence> C;

  // register atoms
  vector<register_atom> RA;

  // in-delimiter of each block
  vector<operation> in;

  // out-delimiter of each block
  vector<operation> out;

  // map from operand to block
  vector<block> pb;

  // map from temporary to block
  vector<block> tb;

  // congruences in each block
  vector<vector<vector<operand> > > bcongr;

  // representative operand of each congruence
  vector<operand> representative;

  // congruence to which each operand belongs
  vector<congruence> operand_congruence;

  // first possible temporary of each use operand
  map<operand, unsigned int> fu;

  // number of possible temporaries in use operands
  unsigned int nu;

  // first possible temporary of each use operand of each block
  vector<unsigned int> bfu;

  // number of possible temporaries in use operands of each block
  vector<unsigned int> bnu;

  // index of each temporary that can be connected to each operand
  vector<map<temporary, unsigned int> > temporary_index;

  // index of each operand in its operation
  map<operand, unsigned int> operand_index;

  // aligned operand tuples in each block
  vector<vector<vector<int> > > baligned;

  // alignment distance of each aligned operand tuple in each block
  vector<vector<int> > badist;

  // packed operand pairs in each block
  vector<vector<vector<operand> > > bpacked;

  // top representative operand of each copy-related class
  vector<operand> copyreltop;

  // operation that defines each temporary
  vector<operation> def_opr;

  // copies of each block
  vector<vector<operation> > copies;

  // mandatory operations of each block
  vector<vector<operation> > mandatory;

  // accumulated mandatory pairs up to each block
  vector<int> accman;

  // index of each mandatory operation within its block
  vector<int> mandatory_index;

  // alignable operands
  vector<alignable> A;

  // alignable operand pairs
  vector<pair<operand, operand> > pairs;

  // whether each operation os a delimiter
  vector<bool> delimiter;

  // global congruences
  vector<global_congruence> G;

  // regular congruence corresponding to each global congruence
  vector<congruence> regular;

  // global congruence corresponding to each regular congruence
  map<congruence, global_congruence> global;

  // single non-null temporary that can be connected to each operand
  map<operand, temporary> single_temp;

  // operands that might use each temporary
  vector<vector<operand> > users;

  // copies that use temporaries from each global congruence
  vector<vector<operation> > use_copies;

  // copies that define temporaries from each global congruence
  vector<vector<operation> > def_copies;

  // allocation candidate spaces of each global congruence
  vector<set<register_space> > candidate_spaces;

  // fixed predecessors of each operation
  vector<vector<operation> > fixed_predecessors;

  // redefined operand pairs of each operation
  vector<vector<pair<operand, operand> > > redefined;

  // grouped copy-related operands for branching
  vector<vector<operand> > groupcopyrel;

  // whether each global congruence is pre-assigned
  vector<bool> preassigned;

  // width of each operand
  vector<int> operand_width;

  // register atoms destroyed in each block
  vector<set<register_atom> > destroyed;

  // set of register avoidance hints
  vector<AvoidHint> avoidhints;

  // ordered callee-saved store copies
  vector<operation> callee_saved_stores;

  // ordered callee-saved store loads
  vector<operation> callee_saved_loads;

  // activation classes
  vector<activation_class> AC;

  // activators in each activation class
  vector<set<instruction> > activation_class_instructions;

  // operations in each activation class
  vector<set<operation> > activation_class_operations;

  // representative operation of each activation class
  vector<operation> activation_class_representative;

  // infinite register atom range assigned to each temporary and space
  map<pair<temporary, register_space>, vector<register_atom> >
  infinite_atom_range;

  // operands participating in a congruence
  set<operand> congruent;

  // successors of each global operand
  map<operand, vector<operand> > succ;

  // sets of interchangeable copies in each block
  vector<vector<vector<operation> > > binterchangeable;

  // minimum latency of each operand if its operation is active
  vector<int> min_active_lat;

  // global clusters
  vector<global_cluster> GC;

  // global clusters whose connection is implied by each global cluster
  vector<set<global_cluster> > implied_clusters;

  // set of remat operations with the same value as each operand
  map<operand, set<operation> > remat;

  // real temporaries that can be connected to each operand
  vector<vector<temporary> > real_temps;

  // operands related to an operand
  vector<set<operand> > related_operands;

  // copy with minimum index having each temporary as a source
  vector<operation> first_copy;

  // minimum operand latency
  int min_lat;

  // maximum operand latency
  int max_lat;

  // atom set of each register class
  vector<IntSet> atom_set;

  // whether each operand is global
  vector<bool> global_operand;

  // index of each global operand
  map<operand, int> global_index;

  // first index of a global operand in each block
  vector<int> first_global_index;

  // number of global operands in each block
  vector<int> n_global;

  // whether each operand is global and may be disconnected
  vector<bool> global_optional;

  // index of each global-optional operand
  map<operand, int> global_optional_index;

  // first index of a global-optional operand in each block
  vector<int> first_global_optional_index;

  // number of global-optional operands in each block
  vector<int> n_global_optionals;

  // ultimate source of each temporary
  vector<temporary> ultimate_source;

  // Derived parameters for presolver

  // preassigned reg of p, or -1
  vector<int> p_preassign;

  // for sure preassigned reg of t, or -1
  vector<int> t_preassign;

  // whether r is callee-saved
  vector<bool> r_calleesaved;

  // allowed combinations of active operations, per block
  vector<vector<PresolverActiveTable>> bactive_tables;

  // allowed combinations of active operations, across blocks
  vector<PresolverActiveTable> gactive_tables;

  // allowed combinations of active operations and operand temporaries, per block
  vector<vector<PresolverCopyTmpTable>> btmp_tables;

  // basic no-goods, across blocks
  vector<UnisonConstraintExpr> gnogoods;

  // basic no-goods, per block
  vector<vector<UnisonConstraintExpr> > bnogoods;

  // additional no-goods, per block
  vector<vector<UnisonConstraintExpr> > bnogoods2;

  // basic presolver partially ordered operands, per block
  vector<vector<PresolverBeforeJSON>> bbefore;

  // additional presolver partially ordered operands, per block
  vector<vector<PresolverBeforeJSON>> bbefore2;

  // presolver across-call temporaries, per block
  vector<vector<PresolverAcrossJSON> > bacross;

  // presolver across-call temporary sets, per block
  vector<vector<PresolverSetAcross> > bset_across;

  // basic presolver precedences, per block
  vector<vector<UnisonConstraintExpr>> bprecedences;

  // additional presolver precedences, per block
  vector<vector<UnisonConstraintExpr>> bprecedences2;

  // presolver sets of interchangeable temporaries, per block
  vector<vector<vector<vector<int> > > > bdomops;

  // set of dominated uses, per block
  vector<vector<vector<operand> > > bdomuses;

  // set of precedences
  vector<vector<vector<operation> > > bprecs;

  // set of dominates, per block
  vector<vector<PresolverDominates>> bdominates;

  // set of difftemps, per block
  vector<vector<vector<operand> > > bdifftemps;

  // set of diffregs, per block
  vector<vector<vector<operand> > > bdiffregs;

  // [MC] set of predecessors or successors, per block
  vector<vector<PresolverPred>> bpredecessors;

  // [MC] set of predecessors or successors, per block
  vector<vector<PresolverSucc>> bsuccessors;

  // Number of objectives
  unsigned int N;

protected:

  Parameters() {};

  JSONVALUE getRoot(JSONVALUE root, string p);
  void get_element(JSONVALUE root, bool & b);
  void get_element(JSONVALUE root, double & d);
  void get_element(JSONVALUE root, int & i);
  void get_element(JSONVALUE root, string & s);
  void get_element(JSONVALUE root, UnisonConstraintExpr & e);
  void get_element(JSONVALUE root, PresolverActiveTable & at);
  void get_element(JSONVALUE root, PresolverCopyTmpTable & ctt);
  // void get_element(JSONVALUE root, PresolverPrecedence & p);
  void get_element(JSONVALUE root, PresolverBeforeJSON & b);
  void get_element(JSONVALUE root, PresolverAcrossJSON & a);
  void get_element(JSONVALUE root, PresolverAcrossItemJSON & ai);
  void get_element(JSONVALUE root, PresolverSetAcross & sa);
  void get_element(JSONVALUE root, PresolverDominates & d);
  void get_element(JSONVALUE root, PresolverPred & d);
  void get_element(JSONVALUE root, PresolverSucc & d);
  void get_element(JSONVALUE root, PresolverInstrCond & d);
  void get_element(JSONVALUE root, PresolverValuePrecedeChain & d);

  template<class T>
  T get_scalar(JSONVALUE root) {
    T i;
    get_element(root, i);
    return i;
  }

  template<class T>
  vector<T> get_vector(JSONVALUE root) {
    vector<T> vi;
    get_element(root, vi);
    return vi;
  }

  template<class T>
  vector<vector<T> > get_2d_vector(JSONVALUE root) {
    return get_vector<vector<T> >(root);
  }

  template<class T>
  vector<vector<vector<T> > > get_3d_vector(JSONVALUE root) {
    return get_vector<vector<vector<T> > >(root);
  }


#ifdef GRAPHICS

  template<class T>
  void get_element(QScriptValue root, vector<T> & vi) {
    assert(root.isArray());
    QScriptValueIterator iti(root);
    while (iti.hasNext()) {
      iti.next();
      if (iti.name() != "length") {
        T e;
        get_element(iti.value(), e);
        vi.push_back(e);
      }
    }
  }

#else

  template<class T>
  void get_element(Json::Value root, vector<T> & vi) {
    assert(root.isArray());
    Json::ValueIterator iti = root.begin();
    while (iti != root.end()) {
      T e;
      get_element(*iti, e);
      vi.push_back(e);
      iti++;
    }
  }

#endif

};

#endif
