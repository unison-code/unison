/*
 *  Main authors:
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
 *    Mats Carlsson <mats.carlsson@ri.se>
 *    Erik Ekstrom <eeks@sics.se>
 *
 *  Contributing authors:
 *    Noric Couderc <noric@sics.se>
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
 * Auxiliary procedures for presolving.
 */

#ifndef __PRESOLVER_AUXILIARY__
#define __PRESOLVER_AUXILIARY__

#include <iostream>
#include "models/parameters.hpp"
#include "common/util.hpp"
#include "presolver-options.hpp"
#include "digraph.hpp"
#include <vector>

using namespace std;

typedef vector<vector<Temporand> > equality_set; // TODO use vectors instead of set
typedef vector<Temporand> temporand_set;         // TODO use vectors instead of set
typedef vector<Temporand> nogood_cand;
typedef vector<nogood_cand> nogood_cand_set;


// Generate mapping for copyrelated temps (not containing sets with null temporary)
void generate_copyrel_temps_map(const Parameters& input,
				map<temporary, vector<temporary> >& m);

// Generate mapping from operand p to set of operands that are congruent to p
// e.g. congr_map[q] yields the congruent operands of q
void generate_congruence_operands(const Parameters& input,
				  vector<vector<operand> >& congr_map);

// Generate mapping from operand p to set of operands that are copyrelated to p
void generate_copyrel_operands_map(const Parameters& input,
				   vector<vector<operand> >& copyrel_operands);

// Return all operands from C (containing temporands)
vector<operand> strip(const temporand_set& C);

// Returns true if a subsumes b, i.e. iff a is included in b
bool subsumes(const presolver_conj& a, const presolver_conj& b);

// Generates the union of disj and sos with any subsumed nogoods removed
vector<presolver_conj> kernel_set(const vector<presolver_conj>& disj,
			  const vector<presolver_conj>& sos,
			  int cutoff);

// Compute the disjunction of elements of Disj that are not subsumed by any
// element of Nogoods.
presolver_disj filter_condition(const presolver_disj& disj,
				const vector<presolver_conj>& nogoods);

// Returns the intersection of the sorted vectors v1 and v2
template <typename T>
vector<T> ord_intersection(const vector<T>& v1, const vector<T>& v2){
  vector<T> i;
  set_intersection(v1.begin(), v1.end(), v2.begin(), v2.end(),back_inserter(i));
  return i;
}

// Returns whether the sorted vectors v1 and v2 intersect
template <typename T>
bool ord_intersect(const vector<T>& v1, const vector<T>& v2) {
  typename vector<T>::const_iterator i = v1.begin();
  typename vector<T>::const_iterator j = v2.begin();
  while (i != v1.end() && j != v2.end()) {
    if (*i == *j)
      return true;
    else if (*i < *j)
      ++i;
    else
      ++j;
  }
  return false;
}

// Returns the union of the sorted vectors v1 and v2
template <typename T>
vector<T> ord_union(const vector<T>& v1, const vector<T>& v2){
  vector<T> i;
  set_union(v1.begin(), v1.end(), v2.begin(), v2.end(),back_inserter(i));
  return i;
}

// Returns the difference of the sorted vectors v1 and v2
template <typename T>
vector<T> ord_difference(const vector<T>& v1, const vector<T>& v2){
  vector<T> i;
  set_difference(v1.begin(), v1.end(), v2.begin(), v2.end(),back_inserter(i));
  return i;
}

// Check whether ordered v contains e
template<typename T>
bool ord_contains(const vector<T>& v, const T& e){
  return binary_search(v.begin(), v.end(), e);
}

// Inserts e into v, if not already in v.
// v is a set stored in a vector
template<typename T>
void vector_insert(vector<T>& v, const T& e){
  typename vector<T>::iterator it = lower_bound(v.begin(), v.end(), e);
  if(it == v.end() || e < *it){
    v.insert(it, e);
  }
}

// Deletes e from v, if in v.
// v is a set stored in a vector
template<typename T>
void vector_erase(vector<T>& v, const T& e){
  typename vector<T>::iterator it = lower_bound(v.begin(), v.end(), e);
  if(it != v.end()){
    v.erase(it);
  }
}

// return first def operand of operation o
operand first_def(const Parameters& input, operation o);

// return first use operand of operation o
operand first_use(const Parameters& input, operation o);

// return use operands of operation o
vector<operand> oper_uses(const Parameters& input, operation o);

// return def operands of operation o
vector<operand> oper_defs(const Parameters& input, operation o);

//  Symbolic type of operation o
int oper_type(const Parameters& input, operation o);

// Operands of operation o
vector<operand> oper_opnds(const Parameters& input, operation o);

// Domain of instructions of operation o
vector<instruction> oper_insns(const Parameters& input, operation o);

// Operation where operand p occurs
operation opnd_oper(const Parameters& input, operand p);

// Domain of temporaries of operand p
vector<temporary> opnd_temps(const Parameters& input, operand p);

// Domain of temporaries of operand p, but null
vector<temporary> opnd_temps_but_null(const Parameters& input, operand p);

// Min of domain of temporaries of operand p
temporary first_temp(const Parameters& input, operand p);

// Min of domain of temporaries of operand p, except null
temporary first_temp_but_null(const Parameters& input, operand p);

// Operation that defines temporary t
operation temp_oper(const Parameters& input, temporary t);

// The operand that can define temporary t
operand temp_def(const Parameters& input, temporary t);

// The operands that can use temporary t
vector<operand> temp_uses(const Parameters& input, temporary t);

// The register width of temporary t
int temp_width(const Parameters& input, temporary t);

// Compute a digraph of the data dependencies of one basic block.
Digraph dd_graph(const Parameters& input, block b);

// Block containing operand p
block block_containing(const Parameters& input, const vector<operand>& P);

// Is operand not preassigned?
bool p_preassigned_not(const Parameters& input, operand p);

// Is operand preassigned caller-saved?
bool p_preassigned_caller_saved(const Parameters& input, operand p);

// Is operand preassigned callee-saved?
bool p_preassigned_callee_saved(const Parameters& input, operand p);

// Is temporary not preassigned?
bool t_preassigned_not(const Parameters& input, temporary t);

// Is temporary preassigned caller-saved?
bool t_preassigned_caller_saved(const Parameters& input, temporary t);

// Is temporary preassigned callee-saved?
bool t_preassigned_callee_saved(const Parameters& input, temporary t);

// Is operation mandatory?
bool is_mandatory(const Parameters& input, operation o);

void p_finite_register_classes(const Parameters& input, operand p, set<register_class>& RC);

presolver_conj normal_conjunction(const Parameters& input, const presolver_conj& c);

UnisonConstraintExpr conj_to_expr(const presolver_conj& c);

UnisonConstraintExpr disj_to_expr(const presolver_disj& d);

presolver_disj expr_to_disj(const UnisonConstraintExpr& e);

bool disj_is_true(const presolver_disj& d);

bool disj_is_false(const presolver_disj& d);

void deepsort(int);

void deepsort(PresolverBefore&);

void deepsort(PresolverDominates&);

void deepsort(PresolverActiveTable&);

template <typename C>
void deepsort(C& v) {
  for (typename C::iterator it = v.begin(); it != v.end(); ++it) {
    deepsort(*it);
  }
  sort(v.begin(), v.end());
  return;
}

// Note: this function compares a "hash" of expected and actual, some accuracy
// is lost in favor of simplicity (e.g. compares only unique elements in
// 'optional_min')
template <typename C>
void run_test(string name, const C& expected, const C& actual) {
  C expected1 = expected, actual1 = actual;
  // deepsort(expected1);
  // expected1.erase(unique(expected1.begin(),expected1.end()),expected1.end());
  // deepsort(actual1);
  // actual1.erase(unique(actual1.begin(),actual1.end()),actual1.end());
  if (expected1 != actual1) {
    cerr << "TEST FAILED: " << name << endl;
    cerr << "EXPECTED: " << show(expected) << endl;
    cerr << "ACTUAL:   " << show(actual)   << endl;
  }
}

// Prefix for debug output
string pre();

// Checks whether the presolver times out and prints a timeout message. If
// verbose, prints the individual runtime of the pass given by t0
bool timeout(Support::Timer & t, PresolverOptions & options, string pass,
             Support::Timer & t0, bool print_time = true);

#endif
