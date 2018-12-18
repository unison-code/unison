/*
 *  Main authors:
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
 *
 *  Contributing authors:
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


#ifndef __COMMON_PROCEDURES__
#define __COMMON_PROCEDURES__

#include <gecode/search.hh>

#include "common/definitions.hpp"
#include "models/options.hpp"
#include "models/parameters.hpp"
#include "models/model.hpp"
#include "models/localmodel.hpp"

using namespace std;
using namespace Gecode;

// Result of solving a constraint problem
enum SolverResult
  { SOME_SOLUTION, UNSATISFIABLE, LIMIT, OPTIMAL_SOLUTION, CACHED_SOLUTION,
    SHAVING_FAILURE, UNKNOWN };

// Class to represent the solution of a Gecode problem
template <class T>
class Solution {
public:
  SolverResult result;
  T * solution;
  unsigned long int failures;
  unsigned long int nodes;
  Solution() : result(UNKNOWN), solution(0), failures(0), nodes(0) {}
  Solution(SolverResult result1, T * solution1, unsigned long int failures1,
           unsigned long int nodes1) :
    result(result1), solution(solution1), failures(failures1), nodes(nodes1) {}
};

// Class to store the state of the current iteration
class IterationState {
public:
  // Aggressiveness
  double a;
  // Whether to try first all global connections
  bool cf;
  // Basic constructor
  IterationState(double a1, bool cf1) : a(a1), cf(cf1) {}
  // Goes to the next iteration state
  void next(ModelOptions * options);
  // Tells whether to stop iterating
  bool stop(ModelOptions * options);
  // Printout
  friend ostream& operator<<(ostream & os, const IterationState & state);
};

// Creates a stop object with the appropiate limit unit
Search::Stop * new_stop(double limit, ModelOptions * options);

// Whether the solution to the given problem is contained in the solution vector
template <class T>
bool solved(T * problem, vector<T *> solutions) {
  for (T * solution : solutions)
    if (problem->equal_to(solution)) return true;
  return false;
}

// Fetches the solution to the given problem from the solution vector
template <class T>
Solution<T> fetch_solution(T * problem, vector<T *> solutions) {
  for (T * solution : solutions)
    if (problem->equal_to(solution))
      return Solution<T>(CACHED_SOLUTION, solution, 0, 0);
  return Solution<T>(LIMIT, NULL, 0, 0);
}

// Gives a vector of blocks sorted lexicographically in descending
// <solution score, block frequency>
vector<block>
sorted_blocks(Parameters * input, map<block, SolverResult> & latest_result);

// Gives the score of a solution result (for sorting local problems)
int score(SolverResult r);

// Gives assignments of operations to instructions forbidden by shaving
vector<InstructionAssignment> shave_instructions(
  Model * base, Search::Stop * stop, Search::Statistics & stats);

// Propagates, possibly emits the lower bound, and returns the status
Gecode::SpaceStatus status_lb(GlobalModel * base);

// Gives the optimality gap from a base space, a solution, and a goal index n
double optimality_gap(GlobalModel * base, const GlobalModel * sol,
                      unsigned int n);

// Emits the lower bound to a file (if specified by the options)
void emit_lower_bound(const GlobalModel * base, bool proven = false);

// Emits the initial optimality gap to a file (if specified by the options)
void emit_initial_gap(GlobalModel * base, const GlobalModel * sol);

// Makes a new local space out of a global one
LocalModel * make_local(const GlobalModel * gs, block b);
LocalModel * make_local(const GlobalModel * gs, block b, IntPropLevel p_ipl);

// register symmetry breaking
vector<PresolverValuePrecedeChain>
value_precede_chains(Parameters & input, Model * m, bool global, block b);

bool
no_conflict(register_atom a,
	    vector<register_class> Cs,
	    int w,
	    map<register_atom,vector<register_class>> CoveringClassesOfReg);

#endif
