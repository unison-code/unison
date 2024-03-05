/*
 *  Main authors:
 *    Roberto Castaneda Lozano <rcas@acm.org>
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


#include "simplemodel.hpp"

SimpleModel::SimpleModel(Parameters * p_input, ModelOptions * p_options,
                         IntPropLevel p_ipl) :
  CompleteModel(p_input, p_options, p_ipl)
{
  v_users = SetVarArray(*this, T().size(), IntSet::empty,
                        IntSet(min_of(input->P), max_of(input->P)));

  // Individual domains of problem variables
  CompleteModel::post_decision_variable_domain_definitions();

  // Secondary variable definitions
  CompleteModel::post_secondary_variable_definitions();

  // Basic model
  CompleteModel::post_basic_model_constraints();

  // Global cost
  CompleteModel::post_global_cost_definition();

  // Cost of each block
  CompleteModel::post_cost_definition();

  // Trivial branchers
  post_trivial_branchers();

}

SimpleModel::SimpleModel(SimpleModel& cg) :
  CompleteModel(cg) {}

SimpleModel* SimpleModel::copy(void) {
  return new SimpleModel(*this);
}

void SimpleModel::post_trivial_branchers(void) {

  branch(*this, v_a, BOOL_VAR_NONE(), BOOL_VAL_MIN());

  branch(*this, v_i, INT_VAR_NONE(), INT_VAL_MIN());
  branch(*this, v_y, INT_VAR_NONE(), INT_VAL_MAX());
  // TODO: do not assign registers to dead temporaries
  branch(*this, v_r, INT_VAR_NONE(), INT_VAL_MIN());
  // TODO: do not assign cycles to inactive operations
  branch(*this, v_c, INT_VAR_NONE(), INT_VAL_MIN());
  branch(*this, v_ff, INT_VAR_NONE(), INT_VAL_MIN());
}
