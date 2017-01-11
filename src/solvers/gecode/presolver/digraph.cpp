/*
 *  Main authors:
 *    Erik Ekstrom <eeks@sics.se>
 *    Mats Carlsson <matsc@sics.se>
 *    Noric Couderc <noric@sics.se>
 *    Roberto Castaneda Lozano <rcas@sics.se>
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

#include "digraph.hpp"

template<typename T>
void vector_insert(vector<T>& v, const T& e){
  typename vector<T>::iterator it = lower_bound(v.begin(), v.end(), e);
  if(it == v.end() || e < *it){
    v.insert(it, e);
  }
}

template<typename T>
void vector_erase(vector<T>& v, const T& e){
  typename vector<T>::iterator it = lower_bound(v.begin(), v.end(), e);
  if(it != v.end()) {
    v.erase(it);
  }
}


Digraph::Digraph(const vector<vector<vertex> >& edges) {
  for(auto& e : edges) {
    vector_insert(adjacency_list[e[0]], e[1]);

    vector_insert(V, e[0]);
    vector_insert(V, e[1]);
  }
}


Digraph::Digraph(const vector<pair<int,int> >& edges) {
  for(auto& e : edges) {
    vector_insert(adjacency_list[e.first], e.second);

    vector_insert(V, e.first);
    vector_insert(V, e.second);
  }
}


vector<vertex> Digraph::vertices() const {
  return V;
}


vector<edge> Digraph::edges() const {
  vector<edge> edges;

  // Collect edges
  for(auto& a : adjacency_list) {
    vertex v1 = a.first;
    for(vertex v2 : a.second) {
      vector_insert(edges, {v1,v2});
    }
  }
  return edges;
}


vector<vertex> Digraph::neighbors(vertex v) {
  return adjacency_list[v];
}


Digraph Digraph::transpose() {
  vector<edge> edges;

  // Make list of reversed edges
  for(auto& a : adjacency_list) {
    vertex v1 = a.first;
    for(vertex v2 : a.second) {
      edges.push_back({v2,v1});
    }
  }

  // Generate a transpoesed graph fromt the reversed edges.
  return Digraph(edges);
}

Digraph Digraph::closure() {
  vector<vertex> vs = vertices();
  vector<edge> es = edges();

  for(vertex v : vs) {
    // Add edges to all vertices that
    // one can reach from v
    for(vertex u : reachables(v)) {
      if(u != v) {
        es.push_back(make_pair(v,u));
      }
    }
  }
  return Digraph(es);
}

Digraph Digraph::product(Digraph& B) {
  vector<edge> es;

  for(const edge& uv : edges()) {
    vertex u = uv.first;
    vertex v = uv.second;
    for(vertex w : B.neighbors(v))
      es.push_back(make_pair(u,w));
  }
  return Digraph(es);
}

// Computing the reduction using the closure

// To prove that transitive reduction is as easy as transitive
// closure, Aho et al. rely on the already-known equivalence with
// Boolean matrix multiplication. They let A be the adjacency matrix
// of the given graph, and B be the adjacency matrix of its transitive
// closure (computed using any standard transitive closure
// algorithm). Then an edge uv belongs to the transitive reduction if
// and only if there is a nonzero entry in row u and column v of
// matrix A, and there is not a nonzero entry in the same position of
// the matrix product AB. In this construction, the nonzero elements
// of the matrix AB represent pairs of vertices connected by paths of
// length two or more.

Digraph Digraph::reduction() {
  Digraph B = closure();
  Digraph AB = product(B);
  vector<edge> es;

  for(const edge& uv : edges()) {
    vertex u = uv.first;
    vertex v = uv.second;
    if(!vector_contains(AB.neighbors(u), v))
      es.push_back(uv);
  }
  return Digraph(es);
}


vector<vertex> Digraph::reachables(const vertex v) {
  vector<vertex> r;
  map<vertex, bool> visited;
  stack<vertex> s;

  for(vertex u : V) {
    visited[u] = false;
  }

  // skip own vertex by pre adding
  for(vertex w : adjacency_list[v]) {
    s.push(w);
  }


  while(!s.empty()) {
    vertex u = s.top();
    s.pop();

    if(!visited[u]) {
      visited[u] = true;
      vector_insert(r, u);
      // For each edge u -> w, push w.
      for(vertex w : adjacency_list[u]) {
        s.push(w);
      }
    }
  }

  return r;
}

void Digraph::bron_kerbosch2(vector<vertex>& R,
			     const vector<vertex>& P,
			     const vector<vertex>& X,
			     vector<vector<vertex>>& cliques) {
  if (P.size()==0 && X.size()==0) {
    vector<vertex> R1(R.begin(), R.end());
    sort(R1.begin(), R1.end());
    cliques.push_back(R1);
  } else if (P.size()>0) {
    vector<vertex> P1(P.begin(), P.end());
    vector<vertex> X1(X.begin(), X.end());
    for (vertex v : vector_difference(P, neighbors(P[0]))) {
      R.push_back(v);
      bron_kerbosch2(R,
		     vector_intersection(P1, neighbors(v)),
		     vector_intersection(X1, neighbors(v)),
		     cliques);
      R.pop_back();
      vector_erase(P1, v);
      vector_insert(X1, v);
    }
  }
}

// Return all maximal cliques
// Warning: assumes not a digraph, but an undirected graph
vector<vector<vertex>> Digraph::max_cliques() {
  vector<vector<vertex>> cliques;
  vector<vertex> R;

  if (!V.empty()) {
    bron_kerbosch2(R, V, {}, cliques);
    sort(cliques.begin(), cliques.end());
  }
  return cliques;
}


vector<vector<vertex> > Digraph::scc() {
  vector<vector<vertex> > sccomponents;
  // Based on Kosaraju's algorithm

  // No vertices visited yet.
  map<vertex, bool> visited;
  for(vertex v : V) {
    visited[v] = false;
  }

  stack<vertex> s;
  // While s does not contain all vertices
  for(vertex v : V) {
    if(!visited[v]) {
        dfs2(v, visited, s);
    }
  }

  // Reverese edges of graph (*this)
  Digraph t = transpose();

  // Reset visisted
  for(vertex v : V) {
    visited[v] = false;
  }

  while(!s.empty()) {
    vertex v = s.top();
    s.pop();

    if(!visited[v]) {
      vector<vertex> component;

      // extract component of vertex v
      t.dfs2(v, visited, component);

      vector_insert(sccomponents, component);
    }
  }

  return sccomponents;
}


void Digraph::dfs2(vertex v, map<vertex, bool>& visited, stack<vertex>& s) {
  visited[v] = true;
  for(vertex w : adjacency_list[v]) {
    if (!visited[w]) {
        dfs2(w, visited, s);
    }
  }
  s.push(v);
}


void Digraph::dfs2(vertex v, map<vertex, bool>& visited, vector<vertex>& s) {
  visited[v] = true;
  for(vertex w : adjacency_list[v]) {
    if (!visited[w]) {
        dfs2(w, visited, s);
    }
  }
  vector_insert(s, v);
}

ostream& operator<<(ostream& os, const Digraph& g){
  for(auto& a : g.adjacency_list) {
    os << a.first << " -> " << show(a.second) << endl;
  }
  return os;
}
