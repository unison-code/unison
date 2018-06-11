/*
 *  Main authors:
 *    Erik Ekstrom <eeks@sics.se>
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
 *
 *  Contributing authors:
 *    Mats Carlsson <mats.carlsson@ri.se>
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
 * Presolver for generating before constraints data. Part of C++ Presolver.
 */

#ifndef __BEFORE_PRESOLVER__
#define __BEFORE_PRESOLVER__

#include <iostream>
#include <algorithm>
#include <map>
#include <vector>
#include <set>

#include "models/parameters.hpp"
#include "auxiliary.hpp"

using namespace std;

typedef vector<PresolverBefore> beforeset;

class BeforePresolver {
private:
  Parameters& input;

  // Map from operand to congurence class
  vector<vector<operand> > congr_map;

  // Copy related operands (indexed with operand)
  vector<vector<operand> > copy_rel_operands;


  BeforePresolver(Parameters& input);

  // Run before presolving
  void presolve(vector<presolver_conj>& Nogoods);

  // Generate before constraints data.
  void gen_before(beforeset& B);

  // Generate beforeset from p and q, which are congruent and any operands
  // depending on them via a low or high operation
  void before1(operand p, operand q, beforeset& b);

#if 0
  // Generate set of conditional operands, that functionally
  // depending on p via a low or high operation
  vector<pair<operand, presolver_conj> >
  lh_descendants(const operand p,
		 const presolver_conj& C,
		 const vector<vector<operand> >& LH);
#endif

  // Generate unconditional befores for operands preassigned to
  // overlapping registers
  vector<vector<operand> > emit_before(const vector<vector<int> >& C);

  // Split beforeset into two pars, befores and nogoods
  void before_vs_nogoods(beforeset& T, vector<presolver_conj>& Nogoods);

public:
  // Static member function, invoked to run before presolver
  static void presolve(Parameters& input, vector<presolver_conj>& Nogoods);
};

#endif
