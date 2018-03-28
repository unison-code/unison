/*
 *  Main authors:
 *    Mikael Almgren <mialmg@kth.se>
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
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

#ifndef __PRESOLVER_PROCEDURES__
#define __PRESOLVER_PROCEDURES__


#include "models/relaxedmodel.hpp"
#include "models/completemodel.hpp"
#include "models/parameters.hpp"
#include "common/definitions.hpp"
#include "commonprocedures.hpp"

using namespace std;
using namespace Gecode;

typedef map<vector<int>, set<vector<int> > > at_map;

const int RELAXED_TIMEOUT = 1;
const int RELAXED_NO_TIMEOUT = 0;
const int RELAXED_SOLUTION_FOUND = 1;
const int RELAXED_NO_SOLUTION_FOUND = 0;

class TmpTableResult {
public:
  at_map labelings;
  int timeout_status;
  int solution_status;
};

class ActiveTableResult {
public:
  vector<vector<int> > labelings;
  int timeout_status;
  int solution_status;
};

// Returns result from temp tables labelling
TmpTableResult get_labelings(Parameters& input, RelaxedModel * base,
			     vector<operation> O, vector<operand> P,
			     int timeout);

// Returns result from active tables labelling
ActiveTableResult get_labelings(RelaxedModel * base,
				vector<operation> O, vector<operand> P,
				int timeout);

// Returns true if there exists a labelling of i(o) | o \in O
// in the current search space
bool exists_i_labeling(RelaxedModel * base,
		       vector<operation> O);

// Runs a propagation loop and updates the parameters according to changes in
// variable domains. Returns whether the propagation succeeds.
bool propagate(Parameters& input);

// Updates the parameters according to the constraint model to reflect that the given operand is disconnected.
void disconnect_operand(Parameters& input, operand p);

void populate_r_domain(Model * m, temporary t, vector<int>& domain);

#endif
