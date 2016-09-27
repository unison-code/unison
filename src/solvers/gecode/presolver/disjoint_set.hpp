/*
 *  Main authors:
 *    Erik Ekstrom <eeks@sics.se>
 *    Roberto Castaneda Lozano <rcas@sics.se>
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
 * Simple implementation of Disjoint-set.
 */

#ifndef __DISJOINT_SET_H__
#define __DISJOINT_SET_H__

#include <map>

using namespace std;

template <class T>
class Disjoint_set {
private:
  map<T,T> parent;
  map<T,int> rank;
public:
  Disjoint_set() {}

  // Add x to set, as a singleton set
  void make_set(const T& x){
    parent[x] = x;
    rank[x] = 0;
  }

  bool contains(const T& x) {
      return (parent.end() != parent.find(x));
  }

  // Union sets of x and y
  void make_union(const T& x, const T& y) {
    T x_root = find(x);
    T y_root = find(y);

    if(x_root != y_root){
      if(rank[x_root] < rank[y_root]) parent[x_root] = y_root;
      else if (rank[x_root] > rank[y_root]) parent[y_root] = x_root;
      else {
        parent[y_root] = x_root;
        rank[x_root] = rank[x_root]+1;
      }
    }
  }

  // Preconditon: x is in DisjointSet
  T find(const T& x) {
    if(!contains(x)) {
        throw(std::out_of_range("Disjoint_set::find()"));
    }
    // Use path compression
    if(parent[x] != x) {
      parent[x] = find(parent[x]);
    }

    return parent[x];
  }
};

#endif
