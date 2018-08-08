/*
 *  Main authors:
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
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


#ifndef __COMPLETE_MODEL__
#define __COMPLETE_MODEL__

#include "model.hpp"

using namespace Gecode;
using namespace std;

class CompleteModel : public Model {

public:

  // Variables

  // gf: global cost
  IntVarArray v_gf;

  // f[b]: cost of block b
  IntVarArray v_f;

  // Variable accessors

  vector<temporary> & T() const;

  vector<temporary> & O() const;

  vector<temporary> & P() const;

  BoolVar x(operand p) const;

  BoolVar u(operand p, temporary t) const;

  IntVar lat(operand p, temporary t) const;

  BoolVar p(operation o, operation j);

  IntVar s(operand p) const;

  IntVarArray gf() const { return v_gf; }

  IntVar f(block b, unsigned int n) const {
    return v_f[(b * input->N) + n];
  };

  IntVarArgs cost() const;

  // Gecode space methods

  CompleteModel(Parameters * p_input, ModelOptions * p_options, IntPropLevel p_ipl);

  CompleteModel(CompleteModel& cg);

  CompleteModel* copy(void);

  // Constraints

  void post_decision_variable_domain_definitions(void);

  void post_secondary_variable_definitions(void);

  void post_basic_model_constraints(void);
  void post_global_operand_connection_constraints(void);
  void post_congruence_constraints(void);
  void post_activation_constraints(void);
  void post_slack_balancing_constraints(void);

  void post_improved_model_constraints(void);
  void post_slack_functional_constraints(void);

  void post_presolver_constraints(void);

  void post_global_cost_definition(void);

  void post_cost_definition(void);

  void post_upper_bound(vector<int> maxcost);
  void post_lower_bound(vector<int> mincost);

  void post_standalone_constraints(void);

  // Other methods

  void print(ostream & pOs) const;

  string solution_to_json() const;

};

#endif
