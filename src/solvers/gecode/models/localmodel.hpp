/*
 *  Main authors:
 *    Roberto Castaneda Lozano <rcas@sics.se>
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


#ifndef __LOCAL_MODEL__
#define __LOCAL_MODEL__

#include "model.hpp"
#include "globalmodel.hpp"
#include "branchers/filters.hpp"
#include "branchers/printers.hpp"
#include "branchers/routingbrancher.hpp"
#include "branchers/pressureschedulingbrancher.hpp"

using namespace Gecode;
using namespace std;

class GlobalModel;

class LocalModel : public Model {

public:

  // Variables

  // f: cost of block
  IntVarArray v_f;

  // p[i][j]: whether operation o precedes operation j
  BoolVarArray v_p;

  // Parameters

  // b: block for which the local model is derived
  block b;

  // Variable accessors

  vector<temporary> & T() const;

  vector<operation> & O() const;

  vector<operand> & P() const;

  BoolVar x(operand p) const;

  BoolVar u(operand p, temporary t) const;

  IntVar lat(operand p, temporary t) const;

  BoolVar p(operation o1, operation o2);

  IntVar s(operand p) const;

  IntVar f(block, unsigned int n) const { return v_f[n]; };

  IntVarArray cost() const;

  // Gecode objects

  IntAction c_activity;

  // Auxiliary methods

  // Gecode space methods

  LocalModel(Parameters * p_input, ModelOptions * p_options, IntPropLevel p_ipl,
             const GlobalModel * gs, block b);

  LocalModel(LocalModel& cg);

  LocalModel* copy(void);

  virtual void constrain(const Space & _s);

  // Constraints

  void post_decision_variable_domain_definitions(void);

  void post_secondary_variable_definitions(void);

  void post_basic_model_constraints(void);

  void post_improved_model_constraints(void);

  void post_presolver_constraints(void);

  static void post_before_scheduling_constraints_in_space(Space& s);

  void post_cost_definition(void);

  void post_operand_symmetry_breaking_constraints(block b);
  void post_register_symmetry_breaking_constraints(block b);

  void constrain_cost(IntRelType irt, int cost);


  // Branchers

  void post_branchers(char search);
  void post_aggressive_branchers(void);
  void post_trivial_branchers(void);
  void post_minimum_cost_branchers(void);
  void post_fail_first_branchers(void);
  void post_conservative_branchers(void);
  void post_routing_branchers(bool aggressive);

  // Master and slave configuration

  bool master(const MetaInfo& mi);
  bool slave(const MetaInfo& mi);

  // Other methods

  void print(ostream & pOs) const;

  void apply_solution(const GlobalModel * gs);

  bool equal_to(const LocalModel * ls) const;

  string emit_json() const;

};

#endif
