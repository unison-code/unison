/*
 *  Main authors:
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
#ifndef __PRECEDENCES__
#define __PRECEDENCES__

#include "common/util.hpp" // min_of, etc...
#include "models/parameters.hpp"
#include "models/options.hpp"
#include "models/globalmodel.hpp"
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

void gen_min_con_erg(const Parameters& input,
		     vector<vector<vector<int>>>& min_con_erg);

void gen_precs_precedences(const Parameters& input,
			   const vector<vector<vector<int>>>& min_con_erg,
			   precedence_set& PI);

void gen_before_precedences(const Parameters& input,
                            PresolverOptions & options,
			    const vector<PresolverBeforeJSON>& before,
			    const vector<vector<vector<int>>>& min_con_erg,
			    precedence_set& PI,
                            Support::Timer & t);

void gen_before_precedences1(const Parameters& input,
			     operand p,
			     operand q,
			     const presolver_disj& disj,
			     const vector<vector<vector<int>>>& min_con_erg,
			     map<PrecedenceEdge, presolver_disj>& M);

/**************** region precedences ******************/

void gen_region_precedences(const Parameters& input,
			    const vector<vector<vector<int>>>& min_con_erg,
			    const precedence_set &PI,
			    precedence_set &PO);

void gen_region_init(const Parameters& input,
		     set<UnisonConstraintExpr>& entailed,
		     map<unsigned long,int>& pweight,
		     map<UnisonConstraintExpr,vector<PresolverPrecedence>>& prec_index,
		     const precedence_set& PI);

void gen_region_precedences_cond(const Parameters& input,
				 const operand p,
				 const temporary t,
				 const vector<vector<vector<int>>>& min_con_erg,
				 map<unsigned long,int>& pweight,
				 map<UnisonConstraintExpr,vector<PresolverPrecedence>>& prec_index,
				 precedence_set& PO);

void gen_region_precedences_block(const Parameters& input,
				  const block b,
				  const vector<vector<vector<int>>>& min_con_erg,
				  map<unsigned long,int>& pweight,
				  map<unsigned long,int>& pweight_c,
				  const presolver_disj& cond,
				  precedence_set& PO);

void partition_nodes(Digraph& G,
		     vector<operation>& pnodes);

void gen_region_per_partition(const Parameters& input,
			      Digraph& G,
			      const vector<operation>& pnodes,
			      const vector<vector<vector<int>>>& min_con_erg,
			      map<unsigned long,int>& pweight,
			      map<unsigned long,int>& pweight_c,
			      const presolver_disj& cond,
			      precedence_set& PO);

void gen_region(const Parameters& input,
		operation src,
		operation sink,
		Digraph& G, // forward
		Digraph& H, // backward
		const vector<vector<vector<int>>>& min_con_erg,
		map<unsigned long,int>& pweight,
		map<unsigned long,int>& pweight_c,
		const presolver_disj& cond,
		precedence_set& PO);

map<operation,int> dag_longest_paths_fwd(vector<operation>& region,
					 map<unsigned long,int>& pweight,
					 map<unsigned long,int>& pweight_c);

map<operation,int> dag_longest_paths_bwd(vector<operation>& region,
					 map<unsigned long,int>& pweight,
					 map<unsigned long,int>& pweight_c);

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

void normalize_precedences(const Parameters& input, const precedence_set& P, vector<UnisonConstraintExpr>& P1);

void gen_long_latency(Parameters& input);

bool analyzable(UnisonConstraintExpr& nogood);

void test_redundancy(Parameters & input, GlobalModel * gm);

void subsumed_resources(Parameters& input);

#if 0

void gen_predecessors_successors(Parameters& input);

int makespan(Parameters& input, const vector<operation>& N);

class MakeSpanModel : public Space {

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

  // constraints

  void post(const vector<operation>& ops);

};

#endif

#endif
