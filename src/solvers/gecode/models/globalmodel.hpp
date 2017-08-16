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


#ifndef __GLOBAL_MODEL__
#define __GLOBAL_MODEL__

#include "completemodel.hpp"
#include "localmodel.hpp"
#include "branchers/merit.hpp"
#include "branchers/value.hpp"

using namespace Gecode;
using namespace std;

class LocalModel;

class GlobalModel : public CompleteModel {

public:

  // Variables

  // pal[p][rs]: whether operand p is allocated to register space rs
  BoolVarArray v_pal;

  // pals[g]: set of register spaces to which global congruence g is allocated
  SetVarArray v_pals;

  // oa[a]: whether each alignable operand pair a is aligned
  BoolVarArray v_oa;

  // ali[g]: set of global congruences that are aligned with g
  SetVarArray v_ali;

  // Parameters

  // af: aggressiveness factor in allocation branching
  double af;

  // cf: whether to try first all global connections
  bool cf;

  void set_aggressiveness(double a1) {af = a1;};

  void set_connect_first(bool cf1) {cf = cf1;};

  // Variable accessors

  BoolVar pal(operand p, register_space rs) const {
    return v_pal[opr(p) * input->RS.size() + rs];
  }

  SetVar pals(global_congruence g) const { return v_pals[g]; }

  BoolVar oa(alignable a) const { return v_oa[a]; }

  SetVar ali(global_congruence g) const { return v_ali[g]; }

  // Auxiliary methods

  // Energy of global cluster gc
  double cluster_energy(global_cluster gc) const;

  // Energy of global congruence g
  double energy(global_congruence g) const;

  // Energy of operand p
  double operand_energy(operand p) const;

  // Cost of allocating global cluster gc to atom set A
  double cluster_allocation_cost(global_cluster gc, RangeListIter & A) const;

  // Cost of disconnecting global cluster gc and allocating rematerializations
  // to atom set A
  double cluster_remat_cost(global_cluster gc, RangeListIter & A) const;

  // Cost of allocating global congruence g to atom set A
  double allocation_cost(global_congruence g, RangeListIter & A) const;

  // Cost of allocating operand p to atom set A
  double operand_allocation_cost(operand p, RangeListIter & A) const;

  // Average cycles spent in spilling w register atoms in block b out of set A.
  double spilling_cost(int w, block b, RangeListIter & A) const;

  // Benefit of (dis)connecting the global cluster gc
  double connection_benefit(global_cluster gc, bool connect) const;

  // Inserts operands of the global cluster gc recursively into ps
  void insert_cluster_operands(global_cluster gc, set<operand> & ps) const;

  // Benefit of allocating global congruence g to atom set A
  double allocation_benefit(global_congruence g, RangeListIter & A) const;

  // Chances of coalescing a copy when its operand p is allocated to A.
  double coalescing_chances(operand p, RangeListIter & A) const;

  // Average cycles saved when operation o is made inactive.
  double inactive_cycle_savings(operation o) const;

  // Gecode space methods

  GlobalModel(Parameters * p_input, ModelOptions * p_options, IntPropLevel p_ipl);

  GlobalModel(GlobalModel& cg);

  GlobalModel* copy(void);

  // Constraints

  void post_secondary_variable_definitions(void);
  void post_operand_allocation_definition(void);
  void post_operand_allocation_set_definition(void);
  void post_alignable_operands_definition(void);
  void post_aligned_congruences_definition(void);

  void post_improved_model_constraints(void);
  void post_disjoint_operand_congruence_constraints(void);
  void post_aligned_neighbor_operand_constraints(void);
  void post_preserved_space_capacity_constraints(void);
  void post_global_cost_domain_constraints(void);

  void post_register_symmetry_breaking_constraints(void);

  void post_active_operation(operation o);
  void post_inactive_operation(operation o);
  void post_lower_bounds(operation o1, operation o2, block b, int lb);
  void post_relaxation_nogood(operation o1, operation o2);
  void post_connection_lower_bound(operand p, bool connect, block b, int lb);
  void post_instruction_nogood(int cost, InstructionAssignment forbidden);
  void post_activation_nogood(operation o, int lb);
  void post_cluster_connection_decision(global_cluster gc, bool connect);
  void post_effective_callee_saved_spilling(operation o);
  void constrain_local_cost(block b, IntRelType irt, int cost);

  void post_different_solution(GlobalModel * g1, bool unsat);
  void post_local_solution_cost(LocalModel * l);

  // Branchers

  void post_branchers(void);
  void post_callee_saved_branchers(void);
  void post_complete_branchers(unsigned int s);

  // Master and slave configuration

  bool master(const MetaInfo& mi);
  bool slave(const MetaInfo& mi);

  // Other methods

  void compare(const Space& s, ostream& os) const;

  // Whether gs contains the same global solution
  bool equal_to(const GlobalModel * gs) const;

  void apply_solution(LocalModel * ls);

  // Activation classes that are not activated by instructions in a solution
  vector<activation_class> unnecessary_activation_classes();

  bool active_class(activation_class ac);

  bool necessary(activation_class ac);

  void apply_solution_and_deactivate(GlobalModel * gs,
                                     vector<activation_class> & acs);

};

#endif
