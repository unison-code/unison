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
#ifndef __ACROSS__
#define __ACROSS__


#include "../common/util.hpp" // min_of, etc...
#include "../models/parameters.hpp"
#include "auxiliary.hpp" // auxiliary functions as defined in the spec
#include "digraph.hpp"
#include "presolver_asserts.hpp"
#include <limits>
#include <vector>

using namespace std;

class PresolverSetAcrossTuple {
public:
  operation o;
  temporary t;
  temporary u;
};

// JSON.across
// JSON.set_across
// JSON.before2
// JSON.nogoods2
void presolve_across(PresolverAsserts& PA,
		     const Parameters& input,
		     vector<PresolverAcrossTuple>& across,
		     vector<PresolverSetAcrossTuple>& alt_across,
		     vector<PresolverBefore>& cond_before,
		     vector<nogood>& nogoods);

void alt_across_to_json(Parameters& input,
			const vector<PresolverSetAcrossTuple>& SetAcross);

void across_to_json(Parameters& input,
		    const vector<PresolverAcrossTuple>& Across,
		    const vector<nogood>& Nogoods);

void collect_at_call(const Parameters& input, operation o, vector<operand>& P, vector<temporary>& T);

void across_extra_regs(const Parameters& input,
		       const vector<operand>& P,
		       vector<register_atom>& E);

void gen_across_call(PresolverAsserts& PA,
		     const Parameters& input,
		     const vector<operation>& oset,
		     block b,
		     vector<PresolverBefore>& CondBefore,
		     vector<PresolverAcrossTuple>& Across);

void collect_before_call(PresolverAsserts& PA, const Parameters& input, operation o1, block b, map<temporary,presolver_disj>& Before);

void collect_after_call(PresolverAsserts& PA, const Parameters& input, operation o1, block b, map<temporary,presolver_disj>& After);

vector<pair<operation,presolver_conj>> extend_predecessors(const Parameters input,
							   const vector<operation>& O,
							   const vector<operand>& P);

presolver_disj merge_disjunctions(const Parameters& input,
				  const presolver_disj& d1,
				  const presolver_disj& d2);

void nogoods_or_across(const Parameters& input,
		       const PresolverAcrossTuple& acr,
		       vector<nogood>& Nogoods,
		       vector<PresolverAcrossTuple>& Across);

vector<temporary> across_candidates(PresolverAsserts& PA, const Parameters& input, operation o1, block b);

pair<bool,presolver_conj> cond_caller_saved(const Parameters& input,
					    operand p,
					    temporary t,
					    const presolver_conj& c);

void cond_before_items(PresolverAsserts& PA,
		       const Parameters& input,
		       const map<temporary,presolver_disj>& Before,
		       const map<temporary,presolver_disj>& After,
		       operand o1,
		       block b,
		       vector<PresolverBefore>& CondBefore);

map<temporary,presolver_disj> cond_before_filter(const Parameters& input,
						 const map<temporary,presolver_disj>& Before,
						 const vector<operation>& successors);

void alt_across_call(PresolverAsserts& PA,
		     const Parameters &input,
		     const vector<operation>& oset,
		     block b,
		     vector<PresolverSetAcrossTuple>& AltAcross);

#endif
