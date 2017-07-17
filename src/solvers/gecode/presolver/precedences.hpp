/*
 *  Main authors:
 *    Mats Carlsson <matsc@sics.se>
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
#ifndef __PRECEDENCES__
#define __PRECEDENCES__

#include "common/util.hpp" // min_of, etc...
#include "models/parameters.hpp"
#include "models/options.hpp"
#include "common/definitions.hpp" // class PresolverPrecedence
#include "auxiliary.hpp" // Kernel_set
#include <map>
#include <set>
#include <vector>

#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace std;
using namespace Gecode;
using namespace Iter::Ranges;

typedef vector<PresolverPrecedence> precedence_set;

// JSON.precedences: p.47 of the specification
// GenFixedPrecedences: p.49
void gen_fixed_precedences(const Parameters& input, precedence_set& PI);

// GenDataPrecedences(OpndToLat)
void gen_data_precedences(const Parameters& input,
			  map<operand,map<instruction,latency>>& opnd_to_lat,
			  precedence_set& PI);

// GenDataPrecedences1(d, o, p, q, Conj, OpndToLat);
void gen_data_precedences1(operation d, operation o,
			   operand p, operand q,
			   const presolver_conj& Conj,
			   map<operand,map<instruction,latency>>& opnd_to_lat,
			   precedence_set& PI);

class PrecedenceEdge {
    public:
        operation i, j;
        latency n;
        bool operator<(const PrecedenceEdge& p) const {
            if(i != p.i) return i < p.i;
            else if(j != p.j) return j < p.j;
            else return n < p.n;
        }
};

void gen_before_precedences(const Parameters& input,
                            PresolverOptions & options,
			    const vector<PresolverBefore>& before,
			    precedence_set& PI,
                            Support::Timer & t);

multimap<PrecedenceEdge, presolver_conj> gen_before_precedences1(const Parameters& input,
								 operand p,
								 operand q,
								 const presolver_disj& disj);

void gen_region_precedences(const Parameters& input, precedence_set& PI);

bool distinct_cycle(const Parameters& input,
		    operation i,
		    operation j,
		    const vector<vector<vector<int>>>& min_con_erg);

void partition_nodes(Digraph& G, vector<operation>& pnodes);

void gen_region_per_partition(const Parameters& input,
			      const Digraph& G,
			      const vector<operation>& pnodes,
			      precedence_set& PI,
			      const vector<vector<vector<int>>>& min_con_erg);

void gen_region(const Parameters& input,
		operation src, operation sink,
		Digraph& G, // forward
		Digraph& H, // backward
		precedence_set& PI,
		const vector<vector<vector<int>>>& min_con_erg,
		map<int,int>& pweights);

map<operation,int> dag_longest_paths_fwd(vector<operation>& region, map<int,int>& pweights);

map<operation,int> dag_longest_paths_bwd(vector<operation>& region, map<int,int>& pweights);

vector<operation> region_finishers(Digraph &R,
				   resource r, int cap,
				   const vector<vector<vector<int>>>& min_con_erg);

void region_finishers_rec(vector<operation>& In,
			  vector<operation>& Out,
			  int load,
			  int decr,
			  pair<int,vector<operation>>& incumbent,
		          Digraph &R,
			  resource r, int cap,
			  const vector<vector<vector<int>>>& min_con_erg);

void normalize_precedences(const Parameters& input, const precedence_set& P, precedence_set& P1);

map<operand, map<instruction, latency>> compute_opnd_to_lat(const Parameters& input);

void gen_long_latency(Parameters& input);

#if 0

void gen_predecessors_successors(Parameters& input);

int makespan(Parameters& input, const vector<operation>& N);

class MakeSpanModel : public MinimizeSpace {

public:

  Parameters * input;
  ModelOptions * options;
  IntPropLevel ipl;

  // Decision variables

  IntVarArray v_c;
  IntVarArray v_i;
  IntVar v_span;

  // convenience

  void constraint(const BoolExpr &e) {rel(*this, e, ipl);}

  IntVar var(const LinIntExpr &e) {return expr(*this, e, ipl);}

  // constructors

  MakeSpanModel(Parameters * p_input, ModelOptions * p_options, IntPropLevel p_ipl);

  MakeSpanModel(MakeSpanModel& cg);

  MakeSpanModel* copy(void);

  IntVar cost(void) const;

  // constraints

  void post(const vector<operation>& ops);

};

#endif

#endif
