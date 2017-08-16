/*
 *  Main authors:
 *    Erik Ekstrom <eeks@sics.se>
 *    Mats Carlsson <mats.carlsson@ri.se>
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
 *
 *  This file is part of Unison, see http://unison-code.github.io
 *
 *  Copyright (c) 2015-2016, Erik Ekstrom
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
 *
 * Presolver for generating nogoods constraints data. Part of C++ Presolver.
 */

#ifndef __INFEASIBLE_PRESOLVER__
#define __INFEASIBLE_PRESOLVER__

#include <iostream>
#include <algorithm>
#include <map>
#include <vector>
#include <set>

#include "models/parameters.hpp"
#include "common/definitions.hpp"
#include "auxiliary.hpp"
#include "digraph.hpp"
#include "disjoint_set.hpp"
#include "presolver_asserts.hpp"


class InfeasiblePresolver {
private:
  PresolverAsserts& PA;

  Parameters& input;

  const bool param_overstrict;

  PresolverOptions &options;

  Support::Timer& timer;

  int cutoff;

  Disjoint_set<Temporand> ds_set;

  // Map from operand to congurence class
  vector<vector<operand> > congr_map;

  // Copy related temporaries (indexed with temporary)
  map<temporary, vector<temporary> > copy_rel_temps;

  // If operand is odd
  vector<bool> odd_operand;

  // If operand is even
  vector<bool> even_operand;

  // Tabled version of opnd_temps_but_null()
  vector<vector<temporary>> temps_but_null;

  // Map of subsumed conj
  map<presolver_conj,bool> subsumed_nogood_map;

  // D and U sets from pseudocode
  vector<temporand_set> D, D_7, D_4;
  equality_set U;

  // sets of reachable vertices
  vector<vector<operand> > R;

  vector<vector<temporand_set >> D_5;

  vector<vector<vector<nogood_cand_set> > > D_5_Cands;

  vector<vector<nogood_cand_set> > D_4_Cands;

  vector<vector<nogood_cand_set> > D_7_Cands;

  void redefined_operand_nogoods(vector<nogood>& Nogoods);

  void before_in_nogoods(vector<nogood>& Nogoods);

  void xchg_nogoods(vector<nogood>& Nogoods);

  void regdomain_nogoods(vector<nogood>& Nogoods);

  void dominsn_nogoods(vector<nogood>& Nogoods);

  // Auguments U with possible assignments for related use operands,
  // auguments D with set of unrelated use operands.
  pair<equality_set, vector<temporand_set > >
  alldiffs_for_co_use(const temporand_set& D_u,
		      const temporand_set& D_d,
		      const equality_set& U,
		      const vector<temporand_set >& D);


  // Generates chunks for which all elements must use non-overlapping
  // registers.
  vector<temporand_set >
  split_chunks_if_alldiff(const map<vector<temporary>, temporand_set >& M);


  // Returns true if C must use non-overlapping registers
  bool chunk_is_alldiff(const temporand_set& C);


  // Returns true if every member of C is congruent to a
  // def operand of the operation o
  bool has_def_proxy(const temporand_set& C, operation o);


  // Returns true if every member of C is congruent to a
  // def operand of the operation o
  bool has_use_proxy(const temporand_set& C, operation o);


  // Returns if the operands in P have disjoint temporary domains
  bool has_disjoint_temporary_domains(const vector<operand>& P);


  // Generate nogood assignments that would unite members of some set in D
  void single_nogoods(const vector<temporand_set >& D,
		      const vector<vector<nogood_cand_set> >& DCands,
		      const vector<vector<operand> >* R,
		      const bool filter_temps,
		      vector<nogood>& Nogoods1);


  // Generate nogood assignments that would unite members of some set in D
  void double_nogoods(const vector<temporand_set >& D,
		      const vector<vector<nogood_cand_set> >& DCands,
		      const vector<vector<operand> >* R,
		      const bool filter_temps,
		      vector<nogood>& Nogoods1);


  // Generate a set of nogoods from the given nogoods-candidates
  void emit_nogood(const vector<vector<operand> >* R,
		   const presolver_conj& Conj,
		   const Temporand& v1,
		   const Temporand& v2,
		   vector<nogood>& Nogoods1);


  // Generate set of noggood candidates (see notes in cpp)
  vector<vector<nogood_cand_set> > gen_candidates(const vector<temporand_set>& D, const vector<vector<Temporand> >& U);

  // Returns if conj is subsumed, i.e., if
  // there is a nogood n in PA.new_nogood such that n subset of conj.
  bool subsumed_nogood(const presolver_conj& conj);

  // Find cyclic precedences chains and recored the conditions for the
  // chians as nogoods
  void cycle_dfs(const operation i,
		 const operation j,
		 vector<vector<operation> > F,
		 const presolver_conj& conj,
		 map<vector<operation>, vector<pair<int,vector<nogood> > > >& M);


  // Generate nogoods to avoid cyclic precedences chanis for one SCC
  void break_cycle(const vector<operation>& scc,
		   const vector<vector<operation> >& E,
		   map<vector<operation>, vector<pair<int,vector<nogood> > > >& M);

  // Generates the cartesian product of V
  vector<vector<Temporand> > cartesian_product(const vector<temporand_set >& V);

  // Fills odds and evens with operands that must be assigned to odd and
  // even registers, respectively.
  void gen_odd_even_operands(vector<bool>& odds, vector<bool>& evens);


  // Returns 1 if all operands of Ps must be in odd registers,
  // 0 if must be in even and -1 in other cases.
  int is_odd_even(const vector<operand>& Ps);

  // Returns true if P1 is reachable from P2 in R, or vice versa, or if
  // p1 is a use and p2 is a def of the same low/high/combine or vice versa
  bool exist_before(const vector<vector<operand> >& R,
		    const vector<operand>& P1,
		    const vector<operand>& P2);

public:

  // member for precomputing reusable data structures
  void setup(void);

  // member for generating infeasible nogoods, avoiding very expensive stuff
  void pass1(vector<temporand_set>& Alldiffs, vector<nogood>& Nogoods);

  // member for generating infeasible nogoods, the expensive part
  void pass2(vector<nogood>& Nogoods);

  // member for detecting precedence cycles
  void detect_cycles(void);

  InfeasiblePresolver(PresolverAsserts& PA, Parameters& input, Support::Timer& t, PresolverOptions &options);

};

#endif
