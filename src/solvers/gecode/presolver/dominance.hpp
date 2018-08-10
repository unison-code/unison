/*
 *  Main authors:
 *    Mikael Almgren <mialmg@kth.se>
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
 *    Mats Carlsson <mats.carlsson@ri.se>
 *
 *  This file is part of Unison, see http://unison-code.github.io
 *
 *  Copyright (c) 2015-2016, Mikael Almgren
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

#ifndef __PRESOLVER_DOMINANCE__
#define __PRESOLVER_DOMINANCE__

#include "common/definitions.hpp"
#include "common/util.hpp"
#include "models/relaxedmodel.hpp"
#include "models/parameters.hpp"
#include "procedures/presolverprocedures.hpp"
#include "presolver-options.hpp"
#include "auxiliary.hpp"

using namespace std;

/*****************************************************************************
    Helper functions
*****************************************************************************/
// Returns the union of two vectors
// vector<int> sunion(vector<int> s1,
// 		   vector<int> s2);

// if for every solution in S beginning with 0 there is another solution
// that replaces the initial 0 by 1, and vice versa
bool compression_1(const vector<vector<int>>& S);

// if s[o] = 1 for some o and every solution in s \in S
// returns the index of o. -1 if no stuck at 1
int  compression_2(const vector<vector<int>>& S, const vector<int>& O);

// if all solutions but the last of S end with 0, and the last
// one consists of all 1s
bool compression_3(const vector<vector<int>>& S);

// Return sorted keys according to the cardinality of their value. If same
// cardinality sort according to key
void sort_increasing_T(map<vector<temporary>,vector<temporary>>& M,
                       vector<vector<temporary>>& ret_keys);

/*****************************************************************************
    Code related to:
    - JSON.domops
*****************************************************************************/
void temp_domination(Parameters & input);

/*****************************************************************************
    Code related to:
    - JSON.dominates
*****************************************************************************/
void gen_dominates(Parameters & input);

/*****************************************************************************
    Code related to:
    - JSON.active_tables
    - JSON.optional_min
    - JSON.tmp_tables

  (returns whether the function times out according to 't' and 'options')

*****************************************************************************/
void gen_active_tables(Parameters & input, Support::Timer & t,
                       PresolverOptions & options);

void assert_active_tables(Parameters & input,
			  RelaxedModel * base,
			  map<vector<temporary>,vector<temporary>>& M,
			  vector<vector<temporary>>& ret_keys,
			  int timeout);

void decompose_copy_set(Parameters & input,
			const vector<operation>& o,
			const vector<vector<int>>& S,
			vector<PresolverActiveTable>& active_tables);

void assert_tmp_tables(Parameters & input,
		       RelaxedModel * base,
		       map<vector<temporary>,vector<temporary>>& M,
		       vector<vector<temporary>>& ret_keys,
		       int timeout);

void tidy(Parameters & input);

vector<vector<int>> trim_tmp_tables(at_map S, temporary k);

vector<vector<int>> trim_clump(set<vector<int>>C, temporary k);

bool tmp_subsumes(vector<int> T1, vector<int> T2, temporary k);

int optional_min_active_tables(Parameters& input, block b);

void filter_active_tables(Parameters & input);

// NEW CODE

void expr_operands(const UnisonConstraintExpr& e, vector<operand>& ps);

bool already_tabled(const vector<operand>& ps, map<operand, int>& P2Table);

UnisonConstraintExpr simplify_expr(const Parameters& input,
				   const UnisonConstraintExpr& n,
				   map<operand, int>& P2Table);

bool noop_copy_literal(const Parameters& input, const UnisonConstraintExpr& c);

bool noop_copy_conjunction(const Parameters& input, const presolver_conj& c);

#endif
