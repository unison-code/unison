/*
 *  Main authors:
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
 *
 *  Contributing authors:
 *    Mats Carlsson <mats.carlsson@ri.se>
 *    Daniel Lundén <daniel.lunden@sics.se>
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


#ifndef __BASE_MODEL__
#define __BASE_MODEL__

#include <iostream>
#include <iomanip>
#include <vector>
#include <utility>
#include <set>
#include <string>
#include <algorithm>
#include <functional>
#include <assert.h>

#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/gist.hh>

#include "common/definitions.hpp"
#include "common/util.hpp"
#include "common/jsonutil.hpp"
#include "parameters.hpp"
#include "options.hpp"

using namespace std;
using namespace Gecode;
using namespace Iter::Ranges;

class Model : public IntLexMinimizeSpace {

public:

  Parameters * input;
  ModelOptions * options;
  IntPropLevel ipl;

  int n_int_vars;
  int n_bool_vars;
  int n_set_vars;

  // Decision variables

  // r[t]: register to which temporary t is assigned
  IntVarArray v_r;

  // i[o]: instruction that implements operation o
  IntVarArray v_i;

  // c[o]: issue cycle of operation o relative to the beginning of its block
  IntVarArray v_c;

  // y[p]: temporary that is connected to operand p
  IntVarArray v_y;

  // Auxiliary variables

  // x[p]: whether operand p is connected
  BoolVarArray v_x;

  // ry[p]: register to which operand p is assigned
  IntVarArray v_ry;

  // a[o]: whether operation o is active
  BoolVarArray v_a;

  // ls[t]: start of live range of temporary t
  IntVarArray v_ls;

  // ld[t]: duration of live range of temporary t
  IntVarArray v_ld;

  // le[t]: end of live range of temporary t
  IntVarArray v_le;

  // al[t][rs]: whether temporary t is allocated to register space rs
  // (some of the register atoms of t overlap with rs)
  BoolVarArray v_al;

  // u[p][t]: whether operand p is connected to temporary t
  BoolVarArray v_u;

  // us[t]: number of times temporary t is used
  IntVarArray v_us;

  // lt[p]: latency of operand p
  IntVarArray v_lt;

  // lat[p][t]: latency of operand p using temporary t
  IntVarArray v_lat;

  // p[o1][o2]: whether operation o1 precedes operation o2
  BoolVarArray v_p;

  // users[t]: set of operands that use temporary t
  SetVarArray v_users;

  // s[p]: latency slack of operand p
  IntVarArray v_s;

  IntVar zero, one;

  // Useful functions
  // TODO: pass high-order 'instr' and 'temp' methods?

  template<class T>
  IntVarArgs instrs_to_var_args(const IntVarArray& vararray, T& indexes)
    const {
    IntVarArgs varargs;
    for (typename T::iterator i = indexes.begin(); i != indexes.end(); i++) {
      varargs << vararray[instr(*i)];
    }
    return varargs;
  }

  template<class T>
  BoolVarArgs instrs_to_var_args(const BoolVarArray& vararray, T& indexes)
    const {
    BoolVarArgs varargs;
    for (typename T::iterator i = indexes.begin(); i != indexes.end(); i++)
      varargs << vararray[instr(*i)];
    return varargs;
  }

  template<class T>
  IntVarArgs temps_to_var_args(const IntVarArray& vararray, T& indexes)
    const {
    IntVarArgs varargs;
    for (typename T::iterator i = indexes.begin(); i != indexes.end(); i++)
      varargs << vararray[temp(*i)];
    return varargs;
  }

  // Convenience to enforce a global consistency level

  IntVar var(const LinIntExpr &e) {return expr(*this, e, ipl);}

  void assert_bounded(IntVar & v) {
    assert(v.min() > Gecode::Int::Limits::min);
    assert(v.max() < Gecode::Int::Limits::max);
  }

  BoolVar var(const BoolExpr &e) {return expr(*this, e, ipl);}

  IntVarArray int_var_array(int n, int min, int max);

  BoolVarArray bool_var_array(int n, int min, int max);

  SetVarArray set_var_array(int n, const IntSet & glb, const IntSet & lub);

  void constraint(const BoolExpr &e) {rel(*this, e, ipl);}

  BoolVar adhoc_constraint_var(UnisonConstraintExpr & e);

  IntVar slack(operand p);

  // High-level constraints

  // Post distinct propagator with optional elements
  void distinct(Home home, const IntVarArgs & x, const BoolVarArgs & m,
                IntPropLevel ipl = IPL_DEF);

  // Variable accessors

  virtual vector<temporary> & T() const = 0;

  virtual vector<operation> & O() const = 0;

  virtual vector<operand> & P() const = 0;

  int temp(temporary t) const { return t - T()[0]; }

  int instr(operation o) const { return o - O()[0]; }

  int opr(operand p) const { return p - P()[0]; }

  IntVar r(temporary t) const { return v_r[temp(t)]; }

  IntVar i(operation o) const { return v_i[instr(o)]; }

  IntVar c(operation o) const { return v_c[instr(o)]; }

  IntVar y(operand p) const { return v_y[opr(p)]; }

  virtual BoolVar x(operand p) const = 0;

  IntVar ry(operand p) const { return v_ry[opr(p)]; }

  BoolVar a(operation o) const { return v_a[instr(o)]; }

  BoolVar l(temporary t) const { return x(input->definer[t]); }

  IntVar ls(temporary t) const { return v_ls[temp(t)]; }

  IntVar ld(temporary t) const { return v_ld[temp(t)]; }

  IntVar le(temporary t) const { return v_le[temp(t)]; }

  BoolVar al(register_space rs, temporary t) const {
    return v_al[temp(t) * input->RS.size() + rs];
  }

  virtual BoolVar u(operand p, temporary t) const = 0;

  IntVar us(temporary t) const { return v_us[temp(t)]; }

  IntVar lt(operand p) const { return v_lt[opr(p)]; }

  virtual IntVar lat(operand p, temporary t) const = 0;

  virtual BoolVar p(operation o, operation j) = 0;

  SetVar users(temporary t) const { return v_users[temp(t)]; }

  virtual IntVar s(operand p) const = 0;

  // Cost of block b for the nth objective
  virtual IntVar f(block b, unsigned int n) const = 0;

  // Auxiliary methods

  // Whether operation o is implemented by instruction i0
  BoolVar imp(operation o, instruction i0);

  // Start of live range of the temporary that is connected to operand p
  IntVar pls(operand p);

  // End of live range of the temporary that is connected to operand p
  IntVar ple(operand p);

  // Upper bound for the objective function
  int objective_domain(void);

  // Expression constraining t to be allocated to rs
  // (some of the register atoms of t overlap with rs)
  BoolVar partially_in_register_space(temporary t, register_space rs);

  // Expression constraining t to be allocated to rs
  // (all of the register atoms of t overlap with rs)
  BoolVar totally_in_register_space(temporary t, register_space rs);

  // Whether operation o is inactive
  bool is_inactive(operation o) const;

  // Whether operation o is optional
  bool is_optional(operation o) const;

  // Whether temporary t is dead
  bool is_dead(temporary t) const;

  // Whether operand p is disconnected
  bool is_disconnected(operand p) const;

  // Whether operand p must be connected to a non-null temporary
  bool must_connect(operand p) const;

  // Whether operand p can be connected to more than one non-null temporary
  bool multiple_non_null_temps(operand p) const;

  // Source operand of a copy i
  operand src(operation o) const;

  // Destination operand of a copy i
  operand dst(operation o) const;

  // Opposite operand of p in its copy
  operand opposite(operand p) const;

  // Gives set of single-class operands among the given ones
  set<operand> single_class(vector<operand> ps) const;

  // Whether the operands are half-congruent (i.e. low- or high-congruent)
  bool half_congruent(operand p, operand q) const;

  // Ensures that atoms assigned to each pair of operands in ps do not overlap
  void disjoint_operand_registers(vector<temporary> ps);

  // Likelihood of saturation (causing spilling) in block b and atom set A
  // during cycles C.
  double saturation_likelihood(block b, pair<int,int> C,
                               RangeListIter & A) const;

  // Pressure balance between use and definition operands of operation o
  double pressure_balance(operation o) const;

  // Pressure of operand p in register class rc
  double pressure(operand p, register_class rc) const;

  // Worst-case issue cycles that operation o can require exclusively
  int worst(operation o) const;

  // Whether the given atom range may saturate in the given block
  bool may_saturate(block b, register_atom fa, register_atom la) const;

  // Domain of resource consumption cost variable by the given operations
  IntArgs consumption_domain(resource r, vector<operation> & is) const;

  // Constructors

  Model(Parameters * p_input, ModelOptions * p_options, IntPropLevel p_ipl);

  Model(Model& cg);


  // Constraints

  void post_decision_variable_domain_definitions(block b);
  void post_instruction_domains(block b);
  void post_issue_cycle_domains(block b);
  void post_temporary_domains(block b);

  void post_secondary_variable_definitions(block b);
  void post_operand_register_definition(block b);
  void post_live_start_definition(block b);
  void post_live_duration_definition(block b);
  void post_live_end_definition(block b);
  void post_connected_operand_definition(block b);
  void post_allocation_definition(block b);
  void post_use_temporary_definition(block b);
  void post_temporary_uses_definition(block b);
  void post_operand_latency_definition(block b);
  void post_temporary_use_latency_definition(block b);
  void post_precedence_definition(block b);
  void post_temporary_users_definition(block b);

  void post_basic_model_constraints(block b);
  void post_connected_users_constraints(block b);
  void post_active_instructions_constraints(block b);
  void post_register_class_constraints(block b);
  void post_disjoint_live_ranges_constraints(block b);
  void post_preassignment_constraints(block b);
  void post_alignment_constraints(block b);
  void post_packing_constraints(block b);
  void post_extensional_constraints(block b);
  void post_data_precedences_constraints(block b);
  void post_processor_resources_constraints(block b);
  void post_fixed_precedences_constraints(block b);
  void post_prescheduling_constraints(block b);
  void post_bypassing_constraints(block b);
  void post_adhoc_constraints(block b);

  void post_improved_model_constraints(block b);
  void post_null_register_constraints(block b);
  void post_effective_copy_constraints(block b);
  void post_disjoint_operand_constraints(block b);
  void post_maximum_temporary_usage_constraints(block b);
  void post_copy_symmetry_breaking_constraints(block b);
  void post_copy_dominance_constraints(block b);
  void post_non_decreasing_temporary_usage_constraints(block b);
  void post_first_k_copies_constraints(block b);
  void post_reverse_data_precedence_constraints(block b);
  void post_minimum_temporary_duration_constraints(block b);
  void post_define_issue_cycle_constraints(block b);
  void post_kill_issue_cycle_constraints(block b);
  void post_disjoint_congruent_operand_constraints(block b);
  void post_disjoint_component_operand_constraints(block b);
  void post_space_capacity_constraints(block b);
  void post_branch_issue_cycle_constraints(block b);
  void post_redefined_operand_constraints(block b);
  void post_active_first_copy_constraints(block b);
  void post_callee_saved_symmetry_breaking_constraints(block b);
  void post_irreflexive_precedence_constraints(block b);
  void post_transitive_precedence_constraints(block b);
  void post_killed_temporary_precedence_constraints(block b);
  void post_cost_domain_constraints(block b);
  void post_local_congruence_constraints(block b);
  void post_ultimate_source_constraints(block b);

  void post_presolver_constraints(block b);
  void post_minimum_number_of_optional_operations_constraints(block b);
  void post_allowed_activation_constraints(block b);
  void post_allowed_copy_activation_and_dataflow_constraints(block b);
  void post_nogood_constraints(block b);
  void post_conditional_precedence_constraints(block b);
  void post_partially_ordered_live_range_constraints(block b);
  void post_across_call_disjoint_temporary_constraints(block b);
  void post_across_call_disjoint_temporary_set_constraints(block b);
  void post_temporary_symmetry_breaking_constraints(block b);
  void post_dominates_constraints(block b);
  void post_difftemps_constraints(block b);
  void post_diffregs_constraints(block b);
  void post_wcet_constraints(block b);
  void post_predecessors_constraints(block b);
  void post_successors_constraints(block b);
  void post_killer_operand_constraints(block b);
  void post_initial_precedence_constraints(block b);
  void post_mandatory_reuse_constraints(block b);

  void post_before_scheduling_constraints(block b);
  void post_temporary_interference_constraints(block b);
  void post_instruction_assignment_constraints(
       const vector<InstructionAssignment> & forbidden);

  void post_cost_definition(block b);

  // Other methods

  void print(ostream & pOs, block b) const;

  void compare(const Space& s, ostream& os) const;

  template<class Var>
  string compare(const string& var, const string& ent,
                 map<int, unsigned int> index,
                 const VarArgArray<Var>& xs, const VarArgArray<Var>& ys) const {
    assert(xs.size() == ys.size());
    vector<string> ret;
    for (pair<int, unsigned> v : index) {
      Var x = xs[v.second], y = ys[v.second];
      if (!Gist::Comparator::compare("", x, y).empty()) {
        stringstream out;
        out << var << "(" << ent << v.first << ")=" << x << " -> " << y;
        ret.push_back(out.str());
      }
    }
    return sepBy(ret, string(", "));
  }

  void assign_instruction(operation o, int ii);

};

vector<PresolverValuePrecedeChain>
value_precede_chains(Parameters & input, Model * m, bool global, block b);

#endif
