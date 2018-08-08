/*
 *  Main authors:
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
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


#ifndef __SOLVER_UTIL__
#define __SOLVER_UTIL__

#include <iostream>
#include <iomanip>
#include <vector>
#include <map>
#include <string>
#include <sstream>
#include <algorithm>
#include <limits>
#include <assert.h>
#include <numeric>

#include <gecode/int.hh>
#include <gecode/minimodel.hh>

#include "common/definitions.hpp"
#include "models/parameters.hpp"

using namespace std;
using namespace Gecode;
using namespace Iter::Ranges;

class Parameters;

class UsageTask {
public:
  IntVar c;
  IntVar dur;
  IntVar e;
  int con;
  BoolVar o;
};

typedef
tuple<set<instruction>, set<temporary>, set<operand> >
CopyAttributes;

template<class T>
vector<T> concat(vector<T> v1, vector<T> v2) {
  vector<T> v;
  v.reserve(v1.size() + v2.size());
  v.insert(v.end(), v1.begin(), v1.end());
  v.insert(v.end(), v2.begin(), v2.end());
  return v;
}

template<class T>
unsigned int find_index(const vector<T> & v, T e) {
  for (unsigned int i = 0; i < v.size(); i++)
    if( v[i] == e) return i;
  return std::numeric_limits<unsigned int>::max();
}

template <typename C, class T>
bool contains(const C& c, T e) {
  return (find(c.begin(), c.end(), e) != c.end());
}

template <typename C1, typename C2>
bool subseteq(const C1& s1, const C2& s2) {
  typename C1::const_iterator it = s1.begin();
  for (it = s1.begin(); it != s1.end(); ++it)
    if (!contains(s2, *it)) return false;
  return true;
}

template<class T>
set<T> to_set(const vector<T> & v) {
  return set<T>(v.begin(), v.end());
}

template<class T>
vector<T> to_vector(const set<T> & s) {
  return vector<T>(s.begin(), s.end());
}

template<class T>
T max_of(const vector<T> & v) {
  return *max_element(v.begin(), v.end());
}

template<class T>
T max_of(vector<vector<vector<T> > > & v3) {
  vector<T> v;
  for (vector<vector<T> > v2 : v3)
    for (vector<T> v1 : v2)
      v.insert(v.end(), v1.begin(), v1.end());
  return max_of(v);
}

template<class T>
T maybe_max_of(T e, const vector<T> & v) {
  return v.empty() ? e : max_of(v);
}

template<class T>
T min_of(vector<T> & v) {
  return *min_element(v.begin(), v.end());
}

template<class T>
T min_of(vector<vector<vector<T> > > & v3) {
  vector<T> v;
  for (vector<vector<T> > v2 : v3)
    for (vector<T> v1 : v2)
      v.insert(v.end(), v1.begin(), v1.end());
  return min_of(v);
}

template<class T>
T maybe_min_of(T e, const vector<T> & v) {
  return v.empty() ? e : min_of(v);
}

template<class T>
T sum_of(const vector<T> & v) {
  return std::accumulate(v.begin(), v.end(), 0);
}

template<class T>
T from_singleton(vector<T> & v) {
  assert(v.size() == 1);
  return *(v.begin());
}

template<class T>
void init_vector(vector<T> & v, unsigned int size, T e) {
  for (unsigned int i = 0; i < size; i++) v.push_back(e);
  return;
}

template<class T>
RangeListIter extend(Gecode::Region & r, T & rs, int w) {
  if (w == 1) return NaryUnion(r, &rs, 1);
  Offset<T> * erss = r.alloc<Offset<T> >(w);
  for (int i = 0; i < w; i++) erss[i] = Offset<T>(rs, i);
  return NaryUnion(r, erss, w);
}

template<class T>
unsigned int range_size(const T & r) {
  T r1(r);
  return  Iter::Ranges::size(r1);
}

template<class T>
string show_range(const T & r) {
  stringstream s;
  for (T r1(r); r1(); ++r1) s << "[" << r1.min() << "," << r1.max() << "]";
  return s.str();
}

string show(const int);

string show(const string);

string show(const Temporand);

string show(const UnisonConstraintExpr);

string show(const PresolverBefore);

string show(const PresolverBeforeJSON);

string show(const PresolverDominates);

string show(const PresolverInstrCond);

string show(const PresolverValuePrecedeChain);

string show(const PresolverInsnClass);

string show(const PresolverInsn2Class2);

string show(const PresolverActiveTable);

string show(const PresolverCopyTmpTable);

string show(const PresolverAcrossJSON);

string show(const PresolverAcrossTuple);

string show(const PresolverAcrossItemJSON);

string show(const PresolverSetAcross);

string show(const PresolverPrecedence x);

string show(const PrecedenceEdge);

template <typename C>
string show(const C& container, string l = ",", string pre = "",
            string limits = "[]") {
  if (container.empty()) return limits;
  stringstream s;
  s << limits[0];
  typename C::const_iterator it = container.begin();
  for (unsigned int i = 0; i < container.size() - 1; ++it) {
    s << pre << show(*it) << l;
    i++;
  }
  s << pre << show(*it) << limits[1];
  return s.str();
}

template <typename C1, typename C2>
string show(const pair<C1,C2>& container, string limits = "(,)") {
  stringstream s;
  s << limits[0] << show(container.first) << limits[1] << show(container.second) << limits[2];
  return s.str();
}

string emit_json(const int i);

string emit_json(const bool b);

string emit_json(const double d);

string emit_json(const string s);

string emit_json(const PresolverActiveTable at);

string emit_json(const PresolverCopyTmpTable at);

string emit_json(const PresolverBeforeJSON at);

string emit_json(const PresolverAcrossJSON at);

string emit_json(const PresolverAcrossItemJSON at);

string emit_json(const PresolverSetAcross at);

string emit_json(const PresolverDominates at);

string emit_json(const PresolverInstrCond at);

string emit_json(const PresolverValuePrecedeChain at);

string emit_json(const UnisonConstraintExpr e);

template <typename C>
string emit_json(const C& container) {
  stringstream s;
  s << "[";
  if (!container.empty()) {
    typename C::const_iterator it = container.begin();
    for (unsigned int i = 0; i < container.size() - 1; ++it) {
      s << emit_json(*it) << ",";
      i++;
    }
    s << emit_json(*it);
  }
  s << "]";
  return s.str();
}

template <class T>
string emit_json_line(string e, const T o) {
  return "\"" + e + "\": " + emit_json(o) + ",\n";
}

template <class T>
string emit_json_line_last(string e, const T o) {
  return "\"" + e + "\": " + emit_json(o) + "\n";
}

template <typename C>
bool disjoint_sets(const C& s1, const C& s2) {
  vector<int> in(s1.size() + s2.size());
  typename C::const_iterator it =
    set_intersection (s1.begin(), s1.end(), s2.begin(), s2.end(), in.begin());
  in.resize(it - in.begin());
  return in.empty();
}

template <class B, class I>
class SingleChoice : public Choice {

public:

  I first;

  SingleChoice(const B& b, I first0) : Choice(b, 2), first(first0) {}

  virtual size_t size(void) const { return sizeof(*this); }

  virtual void archive(Archive& e) const {
    Choice::archive(e);
    e << first;
  }

};

template <class B, class I, class J>
class DoubleChoice : public Choice {

public:

  I first;
  J second;

  DoubleChoice(const B& b, I first0, J second0)
    : Choice(b, 2), first(first0), second(second0) {}

  virtual size_t size(void) const { return sizeof(*this); }

  virtual void archive(Archive& e) const {
    Choice::archive(e);
    e << first;
    e << second;
  }

};

bool overlap(int x, int xw, int y, int yw);

vector<register_atom> extend(register_atom ra, int w);

bool all_assigned(const IntVarArgs& x);

bool holds(const BoolVar x);

string show_space(register_space rs, const Parameters * p);

string show_register(register_atom ra, int w, const Parameters * p);

string show_instruction(instruction i, operation o, const Parameters * p);

string showInstructions(operation o, const IntVar& i, const Parameters * p);

int gcd(int a, int b);

void copy_domain(Home h, IntVar s, IntVar d);

void copy_domain(Home h, BoolVar s, BoolVar d);

template<class C, class T>
unsigned int index_of(C list, T element) {
    return std::distance(list.begin(),
            std::find(list.begin(), list.end(), element));
}

template<class S>
S sepBy(vector<S> s, S sep) {
  S r("");
  if (s.size() == 0) return r;
  unsigned int i = 0;
  while (i < (s.size() - 1)) {
    r += (s[i] + sep);
    i++;
  }
  r += s[i];
  return r;
}

template<class S>
S showDomain(vector<S> s) {
  if (s.size() == 1) return s[0];
  else return "{" + sepBy(s, S(", ")) + "}";
}

template <class T>
class disjointSet {
  map<T, T> rep;
public:
  void insert(T e, T r) { rep[e] = r; }
  T find(T e) { return rep[e]; }
  void merge(T e1, T e2) {
    T e1r = find(e1), e2r = find(e2);
    for (typename::map<T, T>::iterator it = rep.begin(); it != rep.end(); ++it)
      if (it->second == e2r) it->second = e1r;
  }
  int count(T e) { return rep.count(e); };
  typename::map<T, T>::iterator begin() { return rep.begin(); }
  typename::map<T, T>::iterator end() { return rep.end(); }
  set<set<T> > classes() {
    map<T, set<T> > rep2class;
    for (auto it : rep) rep2class[it.second].insert(it.first);
    set<set<T> > cs;
    for (auto r2class : rep2class) cs.insert(r2class.second);
    return cs;
  };
};

string init(string s);

bool in_block(PresolverActiveTable & ct, block b, const Parameters * input);

bool in_block(PresolverCopyTmpTable & ctt, block b, const Parameters * input);

bool in_block(PresolverBeforeJSON & bf, block b, const Parameters * input);

bool in_block(UnisonConstraintExpr & e, block b, const Parameters * input);

vector<int> var_vector(const IntVarArgs & v);

#endif
