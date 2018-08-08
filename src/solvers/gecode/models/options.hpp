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


#ifndef __SOLVER_OPTIONS__
#define __SOLVER_OPTIONS__

#include <limits>
#include <gecode/driver.hh>

#include "common/definitions.hpp"

using namespace Gecode;
using namespace std;

class ModelOptions : public InstanceOptions {

protected:

  // Interface

  Driver::StringValueOption _output_file; // Output file
  Driver::StringValueOption _dzn_file; // Dzn file (ignored)
  Driver::BoolOption _verbose; // Verbose mode
  Driver::BoolOption _emit_improvement; // Emit estimated improvement
  Driver::StringValueOption _lower_bound_file; // Lower bound file
#ifdef GRAPHICS
  Driver::BoolOption _gist_global; // Run Gist for global problems
  Driver::IntOption _gist_block; // Block on which to run Gist
  Driver::IntOption _gist_iteration; // Iteration on which to run Gist
  Driver::BoolOption _gist_presolving; // Run Gist during presolving
  Driver::BoolOption _gist_solution; // Run Gist for the solution
#endif

  // Limits

  Driver::StringValueOption _limit_unit; // Unit of limits (time, fails)
  Driver::DoubleOption _global_budget; // Global budget per operation
  Driver::DoubleOption _monolithic_budget; // Time budget per operation for the monolithic solver
  Driver::DoubleOption _global_setup_limit; // Limit for root global propagation
  Driver::DoubleOption _local_limit; // Local limit
  Driver::DoubleOption _global_shaving_limit; // Global shaving limit
  Driver::DoubleOption _post_global_shaving_limit; // Global shaving limit after global solver
  Driver::DoubleOption _local_shaving_limit; // Local shaving limit
  Driver::DoubleOption _local_relaxation_limit; // Local relaxation limit
  Driver::DoubleOption _acceptable_gap; // Acceptable optimality gap
  Driver::DoubleOption _timeout; // Global timeout

  // Solving

  Driver::UnsignedIntOption _total_threads; // Total number of threads
  Driver::UnsignedIntOption _portfolio_threads; // Threads for each portfolio
  Driver::BoolOption _complete; // Run to completeness
  Driver::BoolOption _decomposition; // Run decomposition
  Driver::BoolOption _monolithic; // Run monolithic solver
  Driver::DoubleOption _initial_aggressiveness; // Initial aggressiveness
  Driver::DoubleOption _step_aggressiveness; // Aggressiveness step
  Driver::DoubleOption _final_aggressiveness; // Final aggressiveness
  Driver::BoolOption _global_connection_iterations; // Interleave iterations where global connections are tried first
  Driver::BoolOption _custom_portfolio; // Run custom portfolio
  Driver::StringValueOption _local_portfolio; // Search portfolio for the local problem
  Driver::BoolOption _disable_presolving; // Disable presolving techniques
  Driver::BoolOption _disable_local_shaving; // Disable local shaving techniques
  Driver::BoolOption _disable_global_shaving; // Disable global shaving techniques
  Driver::UnsignedIntOption _consistency_threshold; // Operation threshold to switch to bounds consistency
  Driver::UnsignedIntOption _shaving_threshold; // Operation threshold to disable shaving
  Driver::BoolOption _solve_global_only; // Solve only the global problem
  Driver::BoolOption _first_solution; // Return the first solution found
  Driver::BoolOption _all_solutions; // Emit solutions as they are found
  Driver::UnsignedIntOption _solving_threshold; // Operation threshold to give up on solving
  Driver::UnsignedIntOption _monolithic_threshold; // Operation threshold to give up on running the monolithic solver

  // Model

  Driver::BoolOption _disable_hints; // Disable hints in global solver
  Driver::BoolOption _disable_precedence_variables; // Disable explicit precedence variables and constraints
  Driver::BoolOption _overconstrain; // Allow to overconstrain model

  // Constraints

  Driver::BoolOption _disable_improving; // Disable improved constraints
  Driver::BoolOption _disable_maximum_temporary_usage_constraints; // Disable maximum temporary usage constraints
  Driver::BoolOption _disable_copy_dominance_constraints; // Disable copy dominance constraints
  Driver::BoolOption _disable_space_capacity_constraints; // Disable space capacity constraints
  Driver::BoolOption _disable_operand_symmetry_breaking_constraints; // Disable operand symmetry breaking constraints
  Driver::BoolOption _disable_register_symmetry_breaking_constraints; // Disable register symmetry breaking constraints
  Driver::BoolOption _disable_presolver_constraints; // Disable presolver constraints
  Driver::BoolOption _disable_minimum_number_of_optional_operations_constraints; // Disable minimum number of optional operations constraints
  Driver::BoolOption _disable_allowed_activation_constraints; // Disable allowed activation constraints
  Driver::BoolOption _disable_copy_activation_and_dataflow_constraints; // Disable copy activation and dataflow constraints
  Driver::BoolOption _disable_nogood_constraints; // Disable nogood constraints
  Driver::BoolOption _disable_basic_nogood_constraints; // Disable basic nogood constraints
  Driver::BoolOption _disable_additional_nogood_constraints; // Disable additional nogood constraints
  Driver::BoolOption _disable_conditional_precedence_constraints; // Disable conditional precedence constraints
  Driver::BoolOption _disable_basic_conditional_precedence_constraints; // Disable basic conditional precedence constraints
  Driver::BoolOption _disable_additional_conditional_precedence_constraints; // Disable additional conditional precedence constraints
  Driver::BoolOption _disable_partially_ordered_live_range_constraints; // Disable partially ordered live range constraints
  Driver::BoolOption _disable_basic_partially_ordered_live_range_constraints; // Disable basic partially ordered live range constraints
  Driver::BoolOption _disable_additional_partially_ordered_live_range_constraints; // Disable additional partially ordered live range constraints
  Driver::BoolOption _disable_across_call_disjoint_temporary_constraints; // Disable across call disjoint temporary constraints
  Driver::BoolOption _disable_across_call_disjoint_temporary_set_constraints; // Disable across call disjoint temporary set constraints
  Driver::BoolOption _disable_temporary_symmetry_breaking_constraints; // Disable temporary symmetry breaking constraints
  Driver::BoolOption _disable_infinite_register_dominance_constraints; // Disable infinite register dominance constraints

public:

  ModelOptions(void);

  string output_file(void) const {return _output_file.value();}
  string dzn_file(void) const {return _dzn_file.value();}
  bool verbose(void) const {return _verbose.value();}
  bool emit_improvement(void) const {return _emit_improvement.value();}
  string lower_bound_file(void) const {return _lower_bound_file.value();}
#ifdef GRAPHICS
  bool gist_global(void) const {return _gist_global.value();}
  int gist_block(void) const {return _gist_block.value();}
  int gist_iteration(void) const {return _gist_iteration.value();}
  bool gist_presolving(void) const {return _gist_presolving.value();}
  bool gist_solution(void) const {return _gist_solution.value();}
#endif

  string limit_unit(void) const {return _limit_unit.value();}
  double global_budget(void) const {return _global_budget.value();}
  double monolithic_budget(void) const {return _monolithic_budget.value();}
  double global_setup_limit(void) const {return _global_setup_limit.value();}
  double local_limit(void) const {return _local_limit.value();}
  double global_shaving_limit(void) const {return _global_shaving_limit.value();}
  double post_global_shaving_limit(void) const {return _post_global_shaving_limit.value();}
  double local_shaving_limit(void) const {return _local_shaving_limit.value();}
  double local_relaxation_limit(void) const {return _local_relaxation_limit.value();}
  double acceptable_gap(void) const {return _acceptable_gap.value();}
  double timeout(void) const {return _timeout.value();}

  unsigned int total_threads(void) const {return _total_threads.value();}
  unsigned int portfolio_threads(void) const {return _portfolio_threads.value();}
  bool complete(void) const {return _complete.value();}
  bool decomposition(void) const {return _decomposition.value();}
  bool monolithic(void) const {return _monolithic.value();}
  double initial_aggressiveness(void) const {return _initial_aggressiveness.value();}
  double step_aggressiveness(void) const {return _step_aggressiveness.value();}
  double final_aggressiveness(void) const {return _final_aggressiveness.value();}
  bool global_connection_iterations(void) const {return _global_connection_iterations.value();}
  bool custom_portfolio(void) const {return _custom_portfolio.value();}
  string local_portfolio(void) const {return _local_portfolio.value();}
  bool disable_presolving(void) const {return _disable_presolving.value();}
  bool disable_local_shaving(void) const {return _disable_local_shaving.value();}
  bool disable_global_shaving(void) const {return _disable_global_shaving.value();}
  unsigned int consistency_threshold(void) const {return _consistency_threshold.value();}
  unsigned int shaving_threshold(void) const {return _shaving_threshold.value();}
  bool solve_global_only(void) const {return _solve_global_only.value();}
  bool first_solution(void) const {return _first_solution.value();}
  bool all_solutions(void) const {return _all_solutions.value();}
  unsigned int solving_threshold(void) const {return _solving_threshold.value();}
  unsigned int monolithic_threshold(void) const {return _monolithic_threshold.value();}

  bool disable_hints(void) const {return _disable_hints.value();}
  bool disable_precedence_variables(void) const {return _disable_precedence_variables.value();}
  bool overconstrain(void) const {return _overconstrain.value();}

  bool disable_improving(void) const {return _disable_improving.value();}
  bool disable_maximum_temporary_usage_constraints(void) const {return _disable_maximum_temporary_usage_constraints.value();}
  bool disable_copy_dominance_constraints(void) const {return _disable_copy_dominance_constraints.value();}
  bool disable_space_capacity_constraints(void) const {return _disable_space_capacity_constraints.value();}
  bool disable_operand_symmetry_breaking_constraints(void) const {return _disable_operand_symmetry_breaking_constraints.value();}
  bool disable_register_symmetry_breaking_constraints(void) const {return _disable_register_symmetry_breaking_constraints.value();}
  bool disable_presolver_constraints(void) const {return _disable_presolver_constraints.value();}
  bool disable_minimum_number_of_optional_operations_constraints(void) const {return _disable_minimum_number_of_optional_operations_constraints.value();}
  bool disable_allowed_activation_constraints(void) const {return _disable_allowed_activation_constraints.value();}
  bool disable_copy_activation_and_dataflow_constraints(void) const {return _disable_copy_activation_and_dataflow_constraints.value();}
  bool disable_nogood_constraints(void) const {return _disable_nogood_constraints.value();}
  bool disable_basic_nogood_constraints(void) const {return _disable_basic_nogood_constraints.value();}
  bool disable_additional_nogood_constraints(void) const {return _disable_additional_nogood_constraints.value();}
  bool disable_conditional_precedence_constraints(void) const {return _disable_conditional_precedence_constraints.value();}
  bool disable_basic_conditional_precedence_constraints(void) const {return _disable_basic_conditional_precedence_constraints.value();}
  bool disable_additional_conditional_precedence_constraints(void) const {return _disable_additional_conditional_precedence_constraints.value();}
  bool disable_partially_ordered_live_range_constraints(void) const {return _disable_partially_ordered_live_range_constraints.value();}
  bool disable_basic_partially_ordered_live_range_constraints(void) const {return _disable_basic_partially_ordered_live_range_constraints.value();}
  bool disable_additional_partially_ordered_live_range_constraints(void) const {return _disable_additional_partially_ordered_live_range_constraints.value();}
  bool disable_across_call_disjoint_temporary_constraints(void) const {return _disable_across_call_disjoint_temporary_constraints.value();}
  bool disable_across_call_disjoint_temporary_set_constraints(void) const {return _disable_across_call_disjoint_temporary_set_constraints.value();}
  bool disable_temporary_symmetry_breaking_constraints(void) const {return _disable_temporary_symmetry_breaking_constraints.value();}
  bool disable_infinite_register_dominance_constraints(void) const {return _disable_infinite_register_dominance_constraints.value();}

};

#endif
