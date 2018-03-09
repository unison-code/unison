/*
 *  Main authors:
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
#include "precedences.hpp"

void gen_fixed_precedences(const Parameters& input, precedence_set& PI) {
    // For all Dep in JSON.dep, Dist in JSON.dist, in parallel
    assert(input.dep.size() == input.dist.size());
    for(unsigned int i = 0; i < input.dep.size(); ++i) {
        const vector<vector<int>> Dep = input.dep[i];
        const vector<vector<int>> Dist = input.dist[i];
        assert(Dep.size() == Dist.size());
        // For all <o,o'> in Dep, dij in Dist, in parallel
        for (unsigned int j = 0; j < Dep.size(); j++) {
            const int o  = Dep[j][0];
            const int o1 = Dep[j][1];
            vector<int> dij = Dist[j];

            // Io <- OperInsns(o)
            vector<instruction> Io = oper_insns(input, o);
            // Conj <- empty
            presolver_conj Conj;

            // If o is optional
            if(!is_mandatory(input, o)) {
                // Delete first element from Io and dij
                Io.erase(Io.begin());
                dij.erase(dij.begin());
                // Conj <- Conj U {a(o)}
                Conj.push_back(UnisonConstraintExpr(ACTIVE_EXPR, {o}, {}));
            }
            // If o' is optional
            if(!is_mandatory(input, o1)) {
                // Conj <- Conj U {a(o)}
	      Conj.push_back(UnisonConstraintExpr(ACTIVE_EXPR, {o1}, {}));
            }
            // (First, I compute d1 and the condition)
            int d1 = dij.front();
            bool same_all_dij = all_of(dij.begin(), dij.end(),
                    [d1](int v){ return v == d1;});

            // if o is a mandatory branch and all values of dij are the same d1
	    if(is_mandatory(input, o) && oper_type(input, o) == BRANCH &&
               same_all_dij) {
	        // PI <- PI U {<o, o', d', {}>,<o, o',-d', {}}
                PresolverPrecedence pred(o, o1, d1, {{}});
                PresolverPrecedence pred1(o1, o, -d1, {{}});
                PI.push_back(pred);
                PI.push_back(pred1);
            } else if (same_all_dij) {
                // else if all values of dij are the same d'
                PresolverPrecedence pred(o, o1, d1, presolver_disj({Conj}));
                // PI <- PI U {<o, o', d', {Conj}}
                PI.push_back(pred);
            } else {
                // For all i' in Io, d' in dij, in parallel
                assert(Io.size() == dij.size());
                for (unsigned int i = 0; i < Io.size(); i++) {
                    instruction i1 = Io[i];
                    int d1 = dij[i];
                    // {Conj U {o(o, i')}}
                    presolver_conj c = Conj;
		    UnisonConstraintExpr e(IMPLEMENTS_EXPR, {o,i1}, {});
                    vector_insert(c, e);
                    PresolverPrecedence pred(o, o1, d1, presolver_disj({c}));
                    PI.push_back(pred);
                }
            }
        }
    }
}

static bool distinct_cycle(const Parameters& input,
			   operation i,
			   operation j,
			   const vector<vector<vector<int>>>& min_con_erg) {
  for (resource r : input.R)
    if (min_con_erg[i][r][0] + min_con_erg[j][r][0] > input.cap[r])
      return true;
  return false;
}

void gen_min_con_erg(const Parameters& input,
		     vector<vector<vector<int>>>& min_con_erg) {
  for(operation o : input.O) {
    min_con_erg[o] = vector<vector<int>>(input.R.size());
    for(resource r : input.R) {
      int minc = 9999;
      int mine = 9999;
      for(instruction i : input.instructions[o]) {
	if(i != NULL_INSTRUCTION && minc>input.con[i][r]) {
	  minc = input.con[i][r];
	  mine = minc * input.dur[i][r];
	  if(minc==0)
	    break;
	}
      }
      min_con_erg[o][r] = vector<int>(2);
      min_con_erg[o][r][0] = minc;
      min_con_erg[o][r][1] = mine;
    }
  }
}

void gen_precs_precedences(const Parameters& input,
			   const vector<vector<vector<int>>>& min_con_erg,
			   precedence_set& PI) {
  presolver_conj ConjTrue;
  presolver_disj DisjTrue({ConjTrue});

  // FUN-FUN ---> no-op
  // X-FUN ---> X-[TAIL]CALL if X is not [TAIL]CALL and can't be issued with [TAIL]CALL
  // [TAIL]CALL-X ---> FUN-X if X is not FUN
  for(const vector<operation>& edge : input.precs) {
    operation i = edge[0];
    operation j = edge[1];
    operation isucc = i;
    operation jpred = j;
    if (input.type[i] == CALL || input.type[i] == TAILCALL)
      while (input.type[isucc] != FUN)
	isucc++;
    if (input.type[j] == FUN)
      while (input.type[jpred] != CALL && input.type[jpred] != TAILCALL)
	jpred--;
    if (input.type[i] == FUN && input.type[j] == FUN) {
    } else if (input.type[i] != CALL && input.type[i] != TAILCALL && input.type[j] == FUN &&
	       distinct_cycle(input, i, jpred, min_con_erg)) {
      PI.push_back(PresolverPrecedence(i, jpred, 1, DisjTrue));
    } else if ((input.type[i] == CALL || input.type[i] == TAILCALL) && input.type[j] != FUN) {
      PI.push_back(PresolverPrecedence(isucc, j, 1, DisjTrue));
    } else {
      PI.push_back(PresolverPrecedence(i, j, 1, DisjTrue));
    }
  }
}


multimap<operation, instruction> build_oI (const presolver_disj& Y) {
  multimap<operation, instruction> oI;

    for(const presolver_conj& c : Y) {
        for(const UnisonConstraintExpr& l : c) {
            // If I am not processing the right operation, continue
            if(l.id != IMPLEMENTS_EXPR)
	      continue;
            // Insert o, i
            oI.insert(make_pair(l.data[0], l.data[1]));
        }
    }
    return oI;
}

/**************** region precedences ******************/

#define FastPair(o1,o2) (((unsigned long)(o1)<<32) + (unsigned long)(o2))

#define FastPairSource(P) ((int)((P) >> 32))
#define FastPairTarget(P) ((int)((P) & (unsigned long)(0xFFFFFFFF)))

static int min_latency(const Parameters& input,
		       operation o1,
		       operation o2,
		       operand d,
		       operand u) {
  int ld = 1000000000;
  int lu = 1000000000;
  for(unsigned int ii = 0; ii < input.instructions[o1].size(); ++ii) {
    if(input.instructions[o1][ii] != NULL_INSTRUCTION) {
      unsigned int di = index_of(input.operands[o1], d);
      int lat = input.lat[o1][ii][di];
      ld = ld < lat ? ld : lat;
    }
  }
  for(unsigned int ii = 0; ii < input.instructions[o2].size(); ++ii) {
    if(input.instructions[o2][ii] != NULL_INSTRUCTION) {
      unsigned int ui = index_of(input.operands[o2], u);
      int lat = input.lat[o2][ii][ui];
      lu = lu < lat ? lu : lat;
    }
  }

  return max(ld+lu,0);
};

void gen_region_precedences(const Parameters& input,
			    const vector<vector<vector<int>>>& min_con_erg,
			    const precedence_set& PI,
			    precedence_set& PO) {
  set<UnisonConstraintExpr> entailed;
  map<unsigned long,int> pweight;
  map<unsigned long,int> pweight_c;
  presolver_conj ConjTrue;
  presolver_disj DisjTrue({ConjTrue});
  map<UnisonConstraintExpr,vector<PresolverPrecedence>> prec_index;
  gen_region_init(input, entailed, pweight, prec_index, PI);
  for(block b : input.B)
    gen_region_precedences_block(input, b, min_con_erg, pweight, pweight_c, DisjTrue, PO);
  for(operand p : input.P) {
    temporary t0 = input.temps[p][0];
    if(input.use[p] && t0 != NULL_TEMPORARY) {
      for (const auto e : entailed)
	if (e.id == CONNECTS_EXPR && e.data[0] == p)
	  goto nextp;
      for(temporary t : input.temps[p]) {
	if (t != t0)
	  gen_region_precedences_cond(input, p, t, min_con_erg, pweight, prec_index, PO);
      }
    }
  nextp: ;
  }
}

void gen_region_init(const Parameters& input,
		     set<UnisonConstraintExpr>& entailed,
		     map<unsigned long,int>& pweight,
		     map<UnisonConstraintExpr,vector<PresolverPrecedence>>& prec_index,
		     const precedence_set& PI) {
  for (const PresolverActiveTable& pa : input.active_tables) {
    if (pa.tuples.size() == 1) {
      for (unsigned int i=0; i<pa.os.size(); i++)
	if (pa.tuples[0][i]) {
	  operation o = pa.os[i];
	  entailed.insert(UnisonConstraintExpr(ACTIVE_EXPR, {o}, {}));
	  for (operand p : input.operands[o])
	    if (input.use[p] && input.temps[p].size() == 2 && input.temps[p][0] == NULL_TEMPORARY)
	      entailed.insert(UnisonConstraintExpr(CONNECTS_EXPR, {p,input.temps[p][1]}, {}));
	}
    }
  }
  for (const PresolverCopyTmpTable& pa : input.tmp_tables) {
    if (pa.tuples.size() == 1) {
      int no = pa.os.size();
      int np = pa.ps.size();
      for (int i=no; i<no+np; i++) {
	int ti = pa.tuples[0][i];
	entailed.insert(UnisonConstraintExpr(CONNECTS_EXPR, {pa.ps[i-no],ti}, {}));
      }
    }
  }
  for (const PresolverPrecedence& pre : PI) {
    operation src = pre.i;
    operation dest = pre.j;
    if (src < dest && pre.n >= 0) {
      if (disj_is_true(pre.d)) {
	unsigned long key = FastPair(src,dest);
	if (pweight.find(key) == pweight.end() || pweight[key] < pre.n)
	  pweight[key] = pre.n;
      } else if (pre.d.size()==1 && pre.d[0].size()==1) {
	prec_index[pre.d[0][0]].push_back(pre);
      }
    }
  }
  for (const auto e : entailed)
    if (prec_index.find(e) != prec_index.end())
      for (const PresolverPrecedence& pre : prec_index[e]) {
	operation src = pre.i;
	operation dest = pre.j;
	unsigned long key = FastPair(src,dest);
	if (pweight.find(key) == pweight.end() || pweight[key] < pre.n)
	  pweight[key] = pre.n;
      }
  // simulate data precedences: KILL precedences are redundant
  for (operand p : input.P) {
    if (input.use[p]) {
      operation o2 = input.oper[p];
      temporary t = -1;
      for (const auto e : entailed) {
	if (e.id == CONNECTS_EXPR && e.data[0] == p) {
	  t = e.data[1]; break;
	}
      }
      if (t < 0 && (is_mandatory(input, o2) || entailed.count(UnisonConstraintExpr(ACTIVE_EXPR, {o2}, {})))) {
	t = first_temp_but_null(input, p);
      }
      if (t >= 0) {
	operation o1 = input.def_opr[t];
	operand d = input.definer[t];
	if (o1 < o2 && !input.global_operand[d] && !input.global_operand[p] && input.type[o2] != KILL) {
	  unsigned long key = FastPair(o1,o2);
	  int distance = min_latency(input, o1, o2, d, p);
	  if (pweight.find(key) == pweight.end() || pweight[key] < distance)
	    pweight[key] = distance;
	}
      }
    }
  }  
}

void gen_region_precedences_cond(const Parameters& input,
				 const operand p,
				 const temporary t,
				 const vector<vector<vector<int>>>& min_con_erg,
				 map<unsigned long,int>& pweight,
				 map<UnisonConstraintExpr,vector<PresolverPrecedence>>& prec_index,
				 precedence_set& PO) {
  block b = input.pb[p];
  set<UnisonConstraintExpr> entailed_c;
  map<unsigned long,int> pweight_c;
  vector<int> def_use_c;
  UnisonConstraintExpr lit = UnisonConstraintExpr(CONNECTS_EXPR, {p,t}, {});
  presolver_disj cond({{lit}});
  // cerr << "% region precedences assuming " << show(lit) << endl;
  entailed_c.insert(lit);
  entailed_c.insert(UnisonConstraintExpr(ACTIVE_EXPR, {input.def_opr[t]}, {}));
  for (const PresolverCopyTmpTable& pa : input.tmp_tables) {
    int no = pa.os.size();
    int np = pa.ps.size();
    int nt = pa.tuples.size();
    int pix = -1;    
    for (int ii=0; ii<np && pix<0; ii++)
      if (pa.ps[ii] == p)
	pix = ii;
    if (pix >= 0) {
      int tuno = -1;
      for (int jj=0; jj<nt && tuno<nt; jj++)
	if (pa.tuples[jj][no+pix] == t)
	  tuno = (tuno == -1 ? jj : nt);
      if (tuno < 0) {
	// cerr << "%    no support" << endl;
	return;
      } else if (tuno >= 0 && tuno < nt) {
	// cerr << "%    single support" << endl;
	for (int jj=0; jj<no; jj++)
	  if (pa.tuples[tuno][jj]) {
	    operation o1 = pa.os[jj];
	    entailed_c.insert(UnisonConstraintExpr(ACTIVE_EXPR, {o1}, {}));
	  }
	for (int jj=0; jj<np; jj++) {
	  operand p1 = pa.ps[jj];
	  temporary t1 = pa.tuples[tuno][no+jj];
	  entailed_c.insert(UnisonConstraintExpr(CONNECTS_EXPR, {p1,t1}, {}));
	}
	// } else {
	// cerr << "%    multiple support" << endl;
      }
    }
  }
  for (const auto e : entailed_c)
    if (prec_index.find(e) != prec_index.end())
      for (const PresolverPrecedence& pre : prec_index[e]) {
	operation src = pre.i;
	operation dest = pre.j;
	unsigned long key = FastPair(src,dest);
	if (pweight_c.find(key) == pweight_c.end() || pweight_c[key] < pre.n)
	  pweight_c[key] = pre.n;
      }
  // simulate data precedences: KILL precedences are redundant
  for (const auto e : entailed_c) {
    operand u, d;
    if (e.id == CONNECTS_EXPR) {
      u = e.data[0];
      d = input.definer[e.data[1]];
    } else if (e.id == ACTIVE_EXPR) {
      u = first_use(input, e.data[0]);
      d = input.definer[first_temp_but_null(input, u)];
    } else {
      continue;
    }
    operation o1 = input.oper[d];
    operation o2 = input.oper[u];
    if(o1 < o2 && !input.global_operand[d] && !input.global_operand[u] && input.type[o2] != KILL) {
      unsigned long key = FastPair(o1,o2);
      int distance = min_latency(input, o1, o2, d, u);
      if (pweight_c.find(key) == pweight_c.end() || pweight_c[key] < distance)
	pweight_c[key] = distance;
    }
  }
  gen_region_precedences_block(input, b, min_con_erg, pweight, pweight_c, cond, PO);
}

void gen_region_precedences_block(const Parameters& input,
				  const block b,
				  const vector<vector<vector<int>>>& min_con_erg,
				  map<unsigned long,int>& pweight,
				  map<unsigned long,int>& pweight_c,
				  const presolver_disj& cond,
				  precedence_set& PO) {
  vector<vector<operation>> edges;
  for(const auto kv : pweight)
    if(input.oblock[FastPairSource(kv.first)] == b)
      edges.push_back({FastPairSource(kv.first), FastPairTarget(kv.first)});
  for(const auto kv : pweight_c)
    edges.push_back({FastPairSource(kv.first), FastPairTarget(kv.first)});
  vector<operation> pnodes;
  Digraph G = Digraph(edges);
  partition_nodes(G, pnodes);
  gen_region_per_partition(input, G, pnodes, min_con_erg, pweight, pweight_c, cond, PO);
}

void partition_nodes(Digraph& G,
		     vector<operation>& pnodes) {
  operation l = G.vertices()[0];
  for(operation v : G.vertices()) {
    if (v==l)
      pnodes.push_back(v);
    for(operation n : G.neighbors(v))
      l = max(n,l);
  }
}

#if 0
// naive depth-first search
static int longest_path(Digraph& G,
			map<unsigned long,int>& pweight,
			map<unsigned long,int>& pweight_c,
			operation src,
			operation sink,
			int len) {
  if (src > sink) {
    return 0;
  } else if (src == sink) {
    return len;
  } else {
    int lp = len;
    vector<operation> ns = G.neighbors(src);
    for (operation n : ns) {
      unsigned long key = FastPair(src,n);
      int weight = (pweight.find(key) != pweight.end() ? pweight[key] : 0);
      int weight_c = (pweight_c.find(key) != pweight_c.end() ? pweight_c[key] : 0);
      int nl = longest_path(G, pweight, pweight_c, n, sink, len+max(weight,weight_c));
      lp = max(nl,lp);
    }
    return lp;
  }
}
#endif

static bool has_edge_inside(map<unsigned long,int>& pweight_c,
			    operation lb,
			    operation ub) {
  for (const auto kv : pweight_c) {
    operation src = FastPairSource(kv.first);
    operation tgt = FastPairTarget(kv.first);
    if (lb <= src && tgt <= ub)
      return true;
  }
  return false;
}

void gen_region_per_partition(const Parameters& input,
			      Digraph& G, // "precs" edges for 1 block
			      const vector<operation>& pnodes,
			      const vector<vector<vector<int>>>& min_con_erg,
			      map<unsigned long,int>& pweight,
			      map<unsigned long,int>& pweight_c,
			      const presolver_disj& cond,
			      precedence_set& PO) {
  map<operation,vector<pair<operation,operation>>> M;
  int multiplier = input.O.size();

  for(const pair<operation,operation>& edge : G.edges()) {
    for(operation b : pnodes) {
      if(b >= edge.second) {
	M[b].push_back(edge);
	break;
      }
    }
  }

#if 0
  if (!ass.empty()) {
    operand p = ass[0];
    temporary t = ass[1];
    operation src = focus[0];
    operation sink = focus[1];
    int lat = focus[2];
    int lp = longest_path(G, pweight, pweight_c, src, sink, 0);

    // vector<operation> region = ord_union(G.reachables(src), {src});
    // map<operation,int> src_cps = dag_longest_paths_fwd(region, pweight);
    // by construction, sink is reachable from src
    if(lp<lat) {
      UnisonConstraintExpr e(CONNECTS_EXPR, {p,t}, {});
      presolver_conj Conj({e});
      PresolverPrecedence pred(src, sink, lat, presolver_disj({Conj}));
      PO.push_back(pred);
      // pweight[FastPair(src,sink)] = lat; // only holds under assumption p...=t...
    }
  }
#endif
  
  for(const pair<operation,vector<pair<operation,operation>>>& b_edges : M) {
    Digraph G = Digraph(b_edges.second);
    Digraph H = G.transpose();
    map<operation,operation> R;
    for(operation vi : H.vertices()) {
      for(operation vj : H.vertices()) {
	if (vj >= vi) break;
	operation vji = multiplier*vj+vi;
	operation r1 = -1;
	operation r2 = -1;
	for(operation vk : H.neighbors(vi)) {
	  if (r2 > -1) break;
	  operation vjk = multiplier*vj+vk;
	  if(R.find(vjk) != R.end()) {
	    operation Rjk = R[vjk];
	    if (r1 == -1 || r1 == Rjk)
	      r1 = Rjk;
	    else
	      r2 = Rjk;
	  }
	}
	if(r1 == -1) {
	  if(ord_contains(H.neighbors(vi),vj)) {
	    R[vji] = vi;
	  }
	} else if(r2 == -1) {
	  R[vji] = r1;
	} else {
	  R[vji] = vi;
	  if (pweight_c.empty() || has_edge_inside(pweight_c, vj, vi))
	    gen_region(input, vj, vi, G, H, min_con_erg, pweight, pweight_c, cond, PO);
	}
      }
    }
  }
}

/* - use the Van Beek approx: max_r (r1(src,sink,r) + r2(src,sink,r) + r3(src,sink,r) - 1)
 * 
 * - onpath(src,sink,r) = {i | i uses resource r and is in src-sink region}
 * 
 * - r1(src,sink,r) = min{cp(src,k) | k in onpath(src,sink,r)}
 * 
 * - r3(src,sink,r) = min{cp(k,sink) | k in onpath(src,sink,r)}
 * 
 * - r2(src,sink,r) = min #cycles to issue onpath(src,sink,r) =
 *     G <- onpath(src,sink,r) as a digraph
 *     F <- finishers({},{},0,G,r)
 *     return ceiling((sum(k in F)(con_r(k)) + sum(k in V(G)\F)(dur_r(k)*con_r(k))) / cap_r)
 *
 * - finishers(G,r) =
 *     FF <- finishers({},{},0,G,r)
 *     return F in FF with max sum(k in F)(dur_r(k)*con_r(k) - con_r(k))
 *
 * - finishers(I,O,C,G,r) =
 *     find k in V(G) | k not_in I /\ k not_in O /\ G.succ(k) subset I /\ C+con_r(k) <= cap_r
 *     if (no such k)
 *       return {I}
 *     else
 *       return finishers(I+k,O,C+con_r(k),G,r) union finishers(I,O+k,C,G,r)
 *
 */

void gen_region(const Parameters& input,
		operation src,
		operation sink,
		Digraph& G, // forward
		Digraph& H, // backward
		const vector<vector<vector<int>>>& min_con_erg,
		map<unsigned long,int>& pweight,
		map<unsigned long,int>& pweight_c,
		const presolver_disj& cond,
		precedence_set& PO) {
  int glb = 0;
  vector<operation> inside = ord_intersection(G.reachables(src), H.reachables(sink)); // excludes src, sink
  vector<operation> region = ord_union(inside, {src,sink});
  vector<operation> reverse = vector<operation>(region.size());
  unsigned int rs = region.size();

  for (unsigned int ii = 0; ii < rs; ii++)
    reverse[rs-ii-1] = region[ii];

  map<operation,int> src_cps = dag_longest_paths_fwd(region, pweight, pweight_c);
  map<operation,int> sink_cps = dag_longest_paths_bwd(reverse, pweight, pweight_c);
  vector<resource> subsumed = input.subsumed_resources[input.oblock[src]];
  for (resource r : input.R) {
    if (!ord_contains(subsumed, r)) {
      vector<pair<int,int>> edges;
      for (operation o1 : region)
	if (min_con_erg[o1][r][1]>0)
	  for (operation o2 : G.reachables(o1))
	    if (ord_contains(region,o2) && min_con_erg[o2][r][1]>0)
	      edges.push_back(make_pair(o1,o2));
      Digraph R = Digraph(edges);
      vector<operation> RV = R.vertices();
      if (RV.size()>0) {
	int r1=0, r2=0, r3=0;
	vector<operation> F;
	if (min_con_erg[src][r][1]==0) {
	  r1 = 1000000000;
	  for (operation o : RV) {
	    int cp = src_cps[o];
	    r1 = cp < r1 ? cp : r1;
	  }
	}
	if (min_con_erg[sink][r][1]==0) {
	  r3 = 1000000000;
	  for (operation o : RV) {
	    int cp = sink_cps[o];
	    r3 = cp < r3 ? cp : r3;
	  }
	}
	for (operation o : RV)
	  if (min_con_erg[o][r][0] < min_con_erg[o][r][1]) {
	    F = region_finishers(R, r, input.cap[r], min_con_erg);
	    break;
	  }
	for (operation o : RV)
	  if (ord_contains(F,o))
	    r2 = r2 + min_con_erg[o][r][0];
	  else
	    r2 = r2 + min_con_erg[o][r][1];
	r2 = (r2-1)/input.cap[r]+1;
	r2 = r1+r2+r3-1;
	glb = r2 > glb ? r2 : glb;
      }
    }
  }
  if (glb <= src_cps[sink]) {
  } else if (disj_is_true(cond)) {
    PresolverPrecedence pred(src, sink, glb, cond);
    PO.push_back(pred);
    pweight[FastPair(src,sink)] = glb;
    // cerr << "%    NEW c(" << src << ") + " << glb << " <= c(" << sink << ")" << endl;
  } else {
    PresolverPrecedence pred(src, sink, glb, cond);
    PO.push_back(pred);
    pweight_c[FastPair(src,sink)] = glb;
    // cerr << "%    NEW c(" << src << ") + " << glb << " <= c(" << sink << ")" << endl;
  }
}

// Find all longest paths from implicit src, assuming we have a DAG, assuming ascending vertices by top sort
map<operation,int> dag_longest_paths_fwd(vector<operation>& region,
					 map<unsigned long,int>& pweight,
					 map<unsigned long,int>& pweight_c) {
  map<operation,int> L;

  for(operation v : region)
    L[v] = 0;
  for(operation b : region)
    for(operation v : region)
      if(b<v) {
	unsigned long key = FastPair(b,v);
	int w = -1;
	int w_c = -1;
	if(pweight.find(key) != pweight.end())
	  w = pweight[key];
	if(pweight_c.find(key) != pweight_c.end())
	  w_c = pweight_c[key];
	if(max(w,w_c) >= 0)
	  L[v] = max(L[v],L[b]+max(w,w_c));
      }
  return L;
}

// Find all longest paths from implicit src, assuming we have a DAG, assuming ascending vertices by top sort
map<operation,int> dag_longest_paths_bwd(vector<operation>& region,
					 map<unsigned long,int>& pweight,
					 map<unsigned long,int>& pweight_c) {
  map<operation,int> L;

  for(operation v : region)
    L[v] = 0;
  for(operation b : region)
    for(operation v : region)
      if(b>v) {
	unsigned long key = FastPair(v,b);
	int w = -1;
	int w_c = -1;
	if(pweight.find(key) != pweight.end())
	  w = pweight[key];
	if(pweight_c.find(key) != pweight_c.end())
	  w_c = pweight_c[key];
	if(max(w,w_c) >= 0)
	  L[v] = max(L[v],L[b]+max(w,w_c));
      }
  return L;
}

vector<operation> region_finishers(Digraph& R,
				   resource r, int cap,
				   const vector<vector<vector<int>>>& min_con_erg) {
  vector<operation> In;
  vector<operation> Out;
  vector<operation> Empty;
  pair<int,vector<operation>> incumbent;
  incumbent = make_pair(-1,Empty);
  region_finishers_rec(In, Out, 0, 0, incumbent, R, r, cap, min_con_erg);
  return incumbent.second;
}

void region_finishers_rec(vector<operation>& In,
			  vector<operation>& Out,
			  int load,
			  int decr,
			  pair<int,vector<operation>>& incumbent,
		          Digraph& R,
			  resource r, int cap,
			  const vector<vector<vector<int>>>& min_con_erg) {
  if (incumbent.first<decr) {
    incumbent.first = decr;
    incumbent.second.clear();
    for (operation o : In)
      incumbent.second.push_back(o);
  }
  for (operation o : R.vertices()) {
    int inc = min_con_erg[o][r][0];
    if (load+inc <= cap &&
	!ord_contains(In,o) && !ord_contains(Out,o) && ord_difference(R.neighbors(o),In).size()==0) {
      vector_insert(In,o);
      region_finishers_rec(In, Out, load+inc, decr + min_con_erg[o][r][1] - min_con_erg[o][r][0], incumbent, R, r, cap, min_con_erg);
      vector_erase(In,o);
      vector_insert(Out,o);
      region_finishers_rec(In, Out, load, decr, incumbent, R, r, cap, min_con_erg);
      vector_erase(Out,o);
      return;
    }
  }
}

void normalize_precedences(const Parameters& input, const precedence_set& P, vector<UnisonConstraintExpr>& P1) {
    // M <- P' <- empty
    map<PrecedenceEdge, presolver_disj> M;
    // For all <src, dest, d, D> in P
    for(unsigned int ii=0; ii<P.size(); ii++) {
	const PresolverPrecedence& p = P[ii];
	operation src = p.i;
	operation dest = p.j;
	int d = p.n;
	presolver_disj D = p.d;
	// check first whether we are subsumed by an uncond precedence with greater latency
	for(unsigned int jj=ii+1; jj<P.size(); jj++) {
	  const PresolverPrecedence& p2 = P[jj];
	  if (p2.i!=src || p2.j!=dest) break;
	  if (p2.n>=d && disj_is_true(p2.d)) {
	    goto next;
	  }
	}
        // For all C in D
        for(const presolver_conj& C : D) {
            // C <- { L in C | L != o(o,i) || OperInsns(o) != i}
            presolver_conj conj;

            std::copy_if(C.begin(), C.end(), std::back_inserter(conj),
                    [&input](const UnisonConstraintExpr L) {
                        const int o = L.data[0];
                        const int i = L.data[1];
                        return L.id != IMPLEMENTS_EXPR ||
                            (oper_insns(input,o) != vector<instruction>({i}));
                    });

            PrecedenceEdge e;
	    e.i = src;
	    e.j = dest;
	    e.n = d;
            M[e].push_back(conj);
        }
    next: ;
    }
    // For all <src, dest, d> -> D in M
    for(const pair<PrecedenceEdge, presolver_disj>& ed : M) {
      PrecedenceEdge e = ed.first;
      presolver_disj D = ed.second;
      // if {a(o)} in D
      presolver_disj::iterator it = std::find_if(D.begin(), D.end(),
						 [](presolver_conj c){
						   return c.size()==1 && c[0].id == ACTIVE_EXPR;
						 });

      if(it != D.end()) {
	// D <- {{a(o)}}
	D = {*it};
      } else {
	// D <- KernelSet(D, empty);
	D = kernel_set(D, presolver_disj(), -1);
	// K <- intersection of all conjunctions in D
	// i.e. the set of literals present in every conjunction in D
	vector<UnisonConstraintExpr> K = std::accumulate(D.begin(), D.end(), D.front(),
						  [](vector<UnisonConstraintExpr> acc, const presolver_conj c) {
						    // Return intersection of accumulator w/ the conjunction
						    // c : filter acc from elements not in c;
						    vector<UnisonConstraintExpr> res;
						    std::copy_if(acc.begin(), acc.end(), std::back_inserter(res),
								 [&c](UnisonConstraintExpr lit){ return ord_contains(c, lit); });
						    return res; });

	// Y <- { C \ K | C in D }
	presolver_disj Y;
	// Remove elements existing in K from every element C in D;
	std::transform(D.begin(), D.end(), std::back_inserter(Y),
		       [&K](const presolver_conj C) {
			 // diff <- C \ K
			 presolver_conj diff;
			 std::copy_if(C.begin(), C.end(), std::back_inserter(diff),
				      [&K](UnisonConstraintExpr lit){ return !ord_contains(K, lit); });
			 return diff;
		       });

	multimap<operation, instruction> oI = build_oI(Y);
	for(operation o : input.O) {
	  auto range = oI.equal_range(o);
	  // If there are is a set I for o
	  if(std::distance(range.first, range.second) > 0) {
	    // Build it
	    vector<instruction> I, oper_instr;
	    std::transform(range.first, range.second,
			   std::back_inserter(I),
			   [](pair<operation, instruction> p){ return p.second; });

	    // OpenInsns(o)
	    oper_instr = oper_insns(input, o);

	    // Y == {{o(o,i)} | i in I}
	    bool Y_matches_I = I.size() == Y.size(); // Because I was build using Y
	    if(subseteq(I, oper_instr) && Y_matches_I) {
	      // D <- { K U {!o(o,i) | i in OperInsns(o) \ I in i != (-)}}
	      //
	      // Generate the set of OperInsns(o) \ I
	      // where i != NULL_INSTRUCTION
	      vector<instruction> S;
	      std::copy_if(oper_instr.begin(), oper_instr.end(),
			   std::back_inserter(S),
			   [&I](instruction i) {
			     return !ord_contains(I, i) && i != NULL_INSTRUCTION;
			   });

	      presolver_conj conj;
	      // Union with K
	      for(const UnisonConstraintExpr& k : K) {
		conj.push_back(k);
	      }
	      // Negate o(o,i) where i in S and make it a conjunction
	      for(instruction i : S) {
		UnisonConstraintExpr nlit(IMPLEMENTS_EXPR, {o,i}, {});
		UnisonConstraintExpr lit(NOT_EXPR, {}, {nlit});
		conj.push_back(lit);
	      }
	      conj = normal_conjunction(input, conj);
	      D = { conj };
	    }
	  }
	}
      }
      // P' <- P' U {<src, dest, d, D>}
      UnisonConstraintExpr expr(DISTANCE_EXPR, {e.i,e.j,e.n}, {});
      if (!disj_is_true(D)) {
	expr = UnisonConstraintExpr(IMPLIES_EXPR, {}, {disj_to_expr(D),expr});
      } 
      P1.push_back(expr);
    }
    sort(P1.begin(), P1.end()); // canonicalize
}

void gen_before_precedences(const Parameters& input,
                            PresolverOptions & options,
			    const vector<PresolverBeforeJSON>& before,
			    const vector<vector<vector<int>>>& min_con_erg,
			    precedence_set& PI,
                            Support::Timer & t) {
    // M <- empty
    map<PrecedenceEdge, presolver_disj> M;
    unsigned int i = 0;
    // For all <p,q,Disj> in Before
    for(const PresolverBeforeJSON& b : before) {
        // M <- M U GenBeforePrecedences1(p,q,Disj)
        gen_before_precedences1(input, b.p, b.q, expr_to_disj(b.e), min_con_erg, M);
        if ((i % 16 == 0) &&
            timeout(t, options, "gen_before_precedences (1st loop)", t, false))
          return;
        i++;
    }
    i = 0;
    // return {<p,s,n,KernelSet(Disj, empty)> | <p,s,n> -> Disj in M }
    for(auto pd : M) {
      PrecedenceEdge k = pd.first;
      presolver_disj disj = pd.second;
      sort(disj.begin(), disj.end());
      disj.erase(unique(disj.begin(), disj.end()), disj.end());
      PresolverPrecedence e(k.i, k.j, k.n, kernel_set(disj, {}, -1));
      PI.push_back(e);
      if ((i % 16 == 0) &&
	  timeout(t, options, "gen_before_precedences (2nd loop)", t, false))
          return;
      i++;
    }
}

static void before_rule(const Parameters& input,
			const presolver_conj& conj1,
			const operation o,
			const operation o1,
			const presolver_conj& conj,
			const vector<vector<vector<int>>>& min_con_erg,
			map<PrecedenceEdge, presolver_disj>& M) {
    if(o != o1) {
        PrecedenceEdge e;
        e.i = o;
	e.j = o1;
	e.n = distinct_cycle(input, o, o1, min_con_erg) ? 1 : 0;
        presolver_conj u;
        for(const UnisonConstraintExpr& l : conj1) {
	  if (l.id != CONNECTS_EXPR || opnd_temps(input, l.data[0]).size() > 1) // not entailed?
	    vector_insert(u, l);
	}
        for(const UnisonConstraintExpr& l : conj) {
	  if (l.id != CONNECTS_EXPR || opnd_temps(input, l.data[0]).size() > 1) // not entailed?
	    vector_insert(u, l);
	}
        M[e].push_back(u);
    }
}

void gen_before_precedences1(const Parameters& input,
			     operand p, operand q,
			     const presolver_disj& disj,
			     const vector<vector<vector<int>>>& min_con_erg,
			     map<PrecedenceEdge, presolver_disj>& M) {
  operand o_p = opnd_oper(input, p);
  operand o_q = opnd_oper(input, q);
  for(const presolver_conj& conj : disj) {
    before_rule(input, { }, o_p, o_q, conj, min_con_erg, M);
  }
  if(!input.use[p]) {
    temporary t = first_temp_but_null(input, p);
    for(operand r : temp_uses(input, t)) {
      operation o_r = opnd_oper(input, r);
      for(const presolver_conj& conj : disj) {
	UnisonConstraintExpr lit1(CONNECTS_EXPR, {r,t}, {});
	before_rule(input, { lit1 }, o_r, o_q, conj, min_con_erg, M);
      }
    }
  }
  if(input.use[q]) {
    for(temporary t : opnd_temps(input, q)) {
      if(t!=NULL_TEMPORARY) {
	operation o_r = input.def_opr[t];
	for(const presolver_conj& conj : disj) {
	  UnisonConstraintExpr lit1(CONNECTS_EXPR, {q,t}, {});
	  before_rule(input, { lit1 }, o_p, o_r, conj, min_con_erg, M);
	}
      }
    }
  }
}

void gen_long_latency(Parameters& input) {

  // Phase 1: collect relevant def-use pairs where at least one operand is global and may require nonzero slack
  
  set<vector<operand>> seen;
  vector<operand> queue;
  for(operation o : input.O) {
    block b = input.oblock[o];
    vector<operand> operands = input.operands[o];
    vector<instruction> instructions = input.instructions[o];
    for(unsigned int i = 0; i < instructions.size(); ++i) {
      for(unsigned int pp = 0; pp < operands.size(); ++pp) {
	operand p = operands[pp];
	if(!input.use[p] && input.lat[o][i][pp] > 1) {
	  temporary t = input.single_temp[p];
	  operation o2 = input.out[b];
	  for(operand q : input.operands[o2]) {
	    if(ord_contains(input.temps[q],t)) {
	      vector<operand> pq = {p,q};
	      if (seen.find(pq) == seen.end()) {
		seen.insert(pq);
		queue.push_back(q);
		input.long_latency_def_use.push_back(pq);
	      }
	    }
	  }
	} else if(input.use[p] && input.lat[o][i][pp] > 0) {
	  temporary t = input.real_temps[p][0];
	  operation o2 = input.in[b];
	  for(operand q : input.operands[o2]) {
	    if(input.single_temp[q]==t) {
	      vector<operand> pq = {q,p};
	      if (seen.find(pq) == seen.end()) {
		seen.insert(pq);
		queue.push_back(q);
		input.long_latency_def_use.push_back(pq);
	      }
	    }
	  }
	}
      }
    }
  }
  while(!queue.empty()) {
    operand q = queue.back();
    operation o0 = input.oper[q];
    queue.pop_back();
    for(operand p2 : input.congr[input.operand_congruence[q]]) {
      operation o = input.oper[p2];
      if(input.type[o0] == OUT && input.type[o] == IN) {
	temporary t = input.single_temp[p2];
	block b = input.oblock[o];
	operation o2 = input.out[b];
	for(operand q2 : input.operands[o2]) {
	  if(ord_contains(input.temps[q2],t)) {
	    vector<operand> pq2 = {p2,q2};
	    if (seen.find(pq2) == seen.end()) {
	      seen.insert(pq2);
	      queue.push_back(q2);
	      input.long_latency_def_use.push_back(pq2);
	    }
	  }
	}
      } else if(input.type[o0] == IN && input.type[o] == OUT) {
	temporary t = input.real_temps[p2][0];
	block b = input.oblock[o];
	operation o2 = input.in[b];
	for(operand q2 : input.operands[o2]) {
	  if(input.single_temp[q2]==t) {
	    vector<operand> pq2 = {q2,p2};
	    if (seen.find(pq2) == seen.end()) {
	      seen.insert(pq2);
	      queue.push_back(q2);
	      input.long_latency_def_use.push_back(pq2);
	    }
	  }
	}
      }
    }
  }

  // Phase 2: build index

  //  forall(opnds in congr)(
  //    let {set of int: inps = {p | p in opnds where op_type[operand_definer[p]] = TYPE_IN},
  //        set of int: inix = {i | i in index_set(long_latency_def) where long_latency_def[i] in inps},
  //         set of int: outps = {p | p in opnds where op_type[operand_definer[p]] = TYPE_OUT},
  //        set of int: outix = {i | i in index_set(long_latency_use) where long_latency_use[i] in outps},

  int nll = input.long_latency_def_use.size();
  for(vector<operand>& ps : input.congr) {
    vector<operand> inps, outps;
    vector<int> inix, outix;
    for(operand p : ps)
      if(input.global_operand[p])
	switch(input.type[input.oper[p]]) {
	case IN:
	  inps.push_back(p);
	  break;
	case OUT:
	  outps.push_back(p);
	  break;
	}
    if (!inps.empty() && !outps.empty()) {
      for(int ii = 0; ii < nll; ii++) {
	vector<operand>& du = input.long_latency_def_use[ii];
	if(ord_contains(inps, du[0]))
	  inix.push_back(ii);
	if(ord_contains(outps, du[1]))
	  outix.push_back(ii);
      }
      input.long_latency_index.push_back({inps, inix, outps, outix});
    }
  }  
}


bool analyzable(UnisonConstraintExpr& nogood) {
  switch (nogood.id) {
  case AND_EXPR:
    for (UnisonConstraintExpr& ch : nogood.children) {
      switch (ch.id) {
      case CONNECTS_EXPR:
      case IMPLEMENTS_EXPR:
      case CALLER_SAVED_EXPR:
	continue;
      default:
	return false;
      }
    }
    return true;
  case CONNECTS_EXPR:
  case IMPLEMENTS_EXPR:
  case CALLER_SAVED_EXPR:
    return true;
  default:
    return false;
  }
}
  
void test_redundancy(Parameters & input, GlobalModel * gm) {
  for(block b : input.B) {
    vector<UnisonConstraintExpr> nogoods = input.bnogoods[b];
    input.bnogoods[b].clear();
    LocalModel * lm = new LocalModel(gm->input, gm->options, gm->ipl, gm, b);
    lm->status();

    for (UnisonConstraintExpr& nogood : nogoods) {
      if (analyzable(nogood)) {
	bool redundant = true;
	cerr << "ANALYZING " << show(nogood) << endl;
	switch (nogood.id) {
	case AND_EXPR:
	  {
	    int nch = nogood.children.size();
	    for (int i=0; i<nch; i++) {
	      cerr << "  pivot is " << i << endl;
	      LocalModel * clone = (LocalModel *)lm->clone();
	      for (int j=0; j<nch; j++)
		if (j!=i)
		  clone->constraint(clone->adhoc_constraint_var(nogood.children[j]));
	      Gecode::SpaceStatus ss = clone->status();
	      if (ss == SS_FAILED) {
		cerr << "    nonpivots caused failure" << endl;
	      } else {
		BoolVar reif = clone->adhoc_constraint_var(nogood.children[i]);
		cerr << "    pivot's value is " << reif.min() << ".." << reif.max() << endl;
		if (!reif.assigned())
		  redundant = false;
	      }
	      delete clone;
	    }
	  }
	  break;
	case CONNECTS_EXPR:
	case IMPLEMENTS_EXPR:
	case CALLER_SAVED_EXPR:
	  {
	    BoolVar reif = lm->adhoc_constraint_var(nogood);
	    cerr << "  value is " << reif.min() << ".." << reif.max() << endl;
	    if (!reif.assigned())
	      redundant = false;
	  }
	  break;
	default:
	  break;
	}
	if (redundant)
	  cerr << "REDUNDANT " << show(nogood) << endl;
      }
    }
    delete lm;
    input.bnogoods[b] = nogoods;
  }
}

// given block b and resource r, is the resource constraint entailed up front?
void subsumed_resources(Parameters& input) {
  for(block b : input.B) {
    vector<resource> entailed;
    vector<set<operation>> r2o(input.R.size());
    set<instruction> bI;
    for(operation o : input.ops[b]) {
      for(instruction i : input.instructions[o]) {
	bI.insert(input.instructions[o].begin(), input.instructions[o].end());
	if(i != NULL_INSTRUCTION)
	  for(resource r : input.R)
	    if (input.con[i][r]>0 && input.dur[i][r]>0)
	      r2o[r].insert(o);
      }
    }
    for(resource r : input.R) {
      if(r2o[r].size() <= 1) {
	entailed.push_back(r);
      } else {
	for(resource s : input.R) {
	  if(s != r && input.cap[r] >= input.cap[s] && !ord_contains(entailed, s)) {
	    for(instruction i : bI)
	      if(i != NULL_INSTRUCTION &&
		 (input.dur[i][r] > input.dur[i][s] ||
		  input.con[i][r] > input.con[i][s] ||
		  input.off[i][r] != input.off[i][s]))
		goto next_s;
	    entailed.push_back(r);
	    goto next_r;
	  }
	next_s: ;
	}
      }
    next_r: ;
    }
    input.subsumed_resources[b] = entailed;
  }
}
    
#if 0

/*****************************************************************************
    Code related to:
    - JSON.predecessors
    - JSON.successors
*****************************************************************************/
void gen_predecessors_successors(Parameters& input) {
  Digraph G = Digraph(input.precs);
  Digraph H = G.reduction();
  Digraph T = H.transpose();

  for(operation v : T.vertices()) {
    vector<operation> N = T.neighbors(v);
    if(N.size() >= 2) {
      int span = makespan(input, N);
      if(span > 1) {
	PresolverPred PPi;
	PPi.p = N;
	PPi.q = v;
	PPi.d = span;
	input.predecessors.push_back(PPi);
      }
    }
  }
  for(operation v : H.vertices()) {
    vector<operation> N = H.neighbors(v);
    if(N.size() >= 2) {
      int span = makespan(input, N);
      if(span > 1) {
	PresolverSucc PSi;
	PSi.p = v;
	PSi.q = N;
	PSi.d = span;
	input.successors.push_back(PSi);
      }
    }
  }
}

int makespan(Parameters& input, const vector<operation>& ops) {
  ModelOptions options;
  MakeSpanModel * base = new MakeSpanModel(&input, &options, IPL_DOM);
  int span = 1;

  base->post(ops);
  base->status();

  // Set timeout = 3s
  Search::Stop * relaxedStop = new Search::TimeStop(3000);
  Search::Options opt;
  opt.stop = relaxedStop;
  DFS<MakeSpanModel> e(base, opt);
  if (MakeSpanModel *r = e.next()) {
    span = r->v_span.val();
    delete r;
  }
  delete relaxedStop;
  delete base;
  return span;
}

MakeSpanModel::MakeSpanModel(Parameters * p_input, ModelOptions * p_options, IntPropLevel p_ipl) :
  input(p_input),
  options(p_options),
  ipl(p_ipl) {
}

MakeSpanModel::MakeSpanModel(MakeSpanModel& m) :
  Space(m),
  input(m.input),
  options(m.options),
  ipl(m.ipl)
{
  v_c.update(*this, m.v_c);
  v_i.update(*this, m.v_i);
  v_span.update(*this, m.v_span);
}

MakeSpanModel* MakeSpanModel::copy(void) {
  return new MakeSpanModel(*this);
}

void MakeSpanModel::post(const vector<operation>& ops) {
  int n = ops.size();
  v_c = IntVarArray(*this, n, 0, n);
  v_i = IntVarArray(*this, n, 0, input->I.size() - 1);
  v_span = IntVar(*this, 1, n);
  for(int oi=0; oi<n; oi++)
    constraint(v_c[oi] + 1 <= v_span);

  // ripping off Model::post_processor_resources_constraints(block b)

  // The capacity of processor resources cannot be exceeded at any issue cycle:

  map<resource, vector<UsageTask> > r2tasks;

  for(int oi=0; oi<n; oi++) {
    operation o = ops[oi];

    // Map from consumption to tasks for each resource
    vector<map<int, vector<pair<int, int> > > > ru2tasks;
    map<int, vector<pair<int, int> > > emptyMap;
    init_vector(ru2tasks, input->R.size(), emptyMap);

    // Complete map with tasks grouped by consumption
    for (unsigned int ii = 0; ii < input->instructions[o].size(); ii++) {
      instruction i = input->instructions[o][ii];
      for (resource r : input->R) {
        int con = input->con[i][r],
            dur = input->dur[i][r];
        if (con > 0 && dur > 0) ru2tasks[r][con].push_back(make_pair(ii, dur));
      }
    }

    // For each operation, resource and consumption, define a task (possibly
    // related to several instructions)
    for (resource r : input->R) {
      map<int, vector<pair<int, int> > >::iterator it;
      for (it = ru2tasks[r].begin(); it != ru2tasks[r].end(); it++) {
        int maxdur = -1;
        IntArgs durs = IntArgs::create(input->instructions[o].size(), 0, 0);
        IntArgs iis;
        typedef pair<int, int> OpDur;
        for (const OpDur& ou : it->second) {
          durs[ou.first] = ou.second;
          iis << ou.first;
          if (ou.second > maxdur) maxdur = ou.second;
        }
        UsageTask task;
        task.c = v_c[oi];
        IntVar dur(*this, 0, maxdur);
        element(*this, durs, v_i[oi], dur);
        task.dur = dur;
        task.e = var(v_c[oi] + task.dur);
        task.con = it->first;
        BoolVar opc(*this, 0, 1);
        Gecode::dom(*this, v_i[oi], IntSet(iis), opc);
        task.o = opc;
        r2tasks[r].push_back(task);
      }
    }
  }

  for (resource r : input->R) {

    IntVarArgs rc; // Start time of each task
    IntVarArgs rdur; // Duration of each task
    IntVarArgs re; // End time of each task
    IntArgs rcon; // Consumption of each task
    BoolVarArgs ro; // Whether each task is scheduled

    for (const UsageTask& task : r2tasks[r]) {
      rc << task.c;
      rdur << task.dur;
      re << task.e;
      rcon << task.con;
      ro << task.o;
    }

    cumulative(*this, input->cap[r], rc, rdur, re, rcon, ro);
  }

  // branching rule
  IntVarArgs as;
  as << v_span;
  for(int i=0; i<n; i++) {
    as << v_c[i];
    as << v_i[i];
  }
  branch(*this, as, INT_VAR_NONE(), INT_VAL_MIN());
}

#endif
