/*
 *  Main authors:
 *    Erik Ekstrom <eeks@sics.se>
 *    Mats Carlsson <matsc@sics.se>
 *    Roberto Castaneda Lozano <rcas@sics.se>
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
 * Simple Digraph implementation with functions for computing reachability and
 * SCC.
 */

#ifndef __DIGRAPH_H__
#define __DIGRAPH_H__

#include <iostream>
#include <map>
#include <set>
#include <utility>
#include <vector>
#include <stack>
#include "models/parameters.hpp"

using namespace std;

typedef int vertex;
typedef pair<vertex, vertex> edge;

class Digraph {
private:
  // Since graphs in the presolver tend to be sparse, use a map
  // as data structure for the adjacency list,
  // mapping vertex to adjacent vertices
  map<vertex, vector<vertex> > adjacency_list;

  // Set of vertices in the graph
  vector<vertex> V;

  void bron_kerbosch2(vector<vertex>& R,
		      const vector<vertex>& P,
		      const vector<vertex>& X,
		      vector<vector<vertex>>& cliques);

public:

  // TODO: one of these should be enough...
  Digraph(const vector<vector<vertex> >& edges);
  Digraph(const vector<edge>& edges);

  // Returns all vertices
  vector<vertex> vertices() const;

  // Returns all edges
  vector<edge> edges() const;

  // Returns vertices adjacent to v
  vector<vertex> neighbors(vertex v);

  // Returns a transposed graph of this graph.
  // A transposed graph have the same vertices but all edges are
  // reversed.
  Digraph transpose();

  // Transitive closure of a digraph
  Digraph closure();

  // Product with digraph B
  Digraph product(Digraph& B);

  // Transitive reduction of a digraph
  Digraph reduction();

  // Returns all verteices u reachable from v
  vector<vertex> reachables(const vertex v);

  // Return all maximal cliques
  // Warning: assumes not a digraph, but an undirected graph
  vector<vector<vertex>> max_cliques();

  // Returns all strongly connected components.
  // Each component is represented by a set of its vertices.
  vector<vector<vertex> > scc();

  // DFS in grpah, starting at v. Collecting new visited vertices in a stack.
  void dfs2(vertex v, map<vertex, bool>& visited, stack<vertex>& s);

  // DFS in grpah, starting at v. Collecting new visited vertices in a
  // sorted vector.
  void dfs2(vertex v, map<vertex, bool>& visited, vector<vertex>& s);

  friend ostream& operator<<(ostream& os, const Digraph& g);

};

// Returns the intersection of the sorted vectors v1 and v2
template <typename T>
vector<T> vector_intersection(const vector<T>& v1, const vector<T>& v2){
  vector<T> i;
  set_intersection(v1.begin(), v1.end(), v2.begin(), v2.end(),back_inserter(i));
  return i;
}

// Returns the difference of the sorted vectors v1 and v2
template <typename T>
vector<T> vector_difference(const vector<T>& v1, const vector<T>& v2){
  vector<T> i;
  set_difference(v1.begin(), v1.end(), v2.begin(), v2.end(),back_inserter(i));
  return i;
}

// Check whether ordered v contains e
template<typename T>
bool vector_contains(const vector<T>& v, const T& e){
  return binary_search(v.begin(), v.end(), e);
}

#endif
