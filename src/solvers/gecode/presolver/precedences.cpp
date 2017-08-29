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

void gen_data_precedences(const Parameters& input,
			  map<operand,map<instruction,latency>>& opnd_to_lat, 
			  precedence_set& PI) {
    // For all o in JSON.O
    for(operation o : input.O) {
        // For all q in OperOpnds(o) where q is use
        for(operand q : oper_opnds(input, o)) {
	  if(input.use[q]) {
	    // T <- OpndTemps(q)
	    vector<temporary> T = opnd_temps(input, q);
	    // For all t in OpndTempsButNull(q)
	    for(temporary t : T) {
	      if(t!=NULL_TEMPORARY) {
		// d <- TempOper(t)
		// p <- TempDef(t)
		operation d = temp_oper(input, t);
		operand   p = temp_def(input, t);
		bool o_is_kill   = oper_type(input, o) == KILL;
		bool d_is_define = oper_type(input, d) == DEFINE;
		bool q_in_last_use = ord_contains(input.last_use, q);
                // if o is (kill) or (d is (define) and q in JSON.last_use)
		if(o_is_kill || (d_is_define && q_in_last_use)) {
		  // l <- JSON.minlive[t]
		  int l = input.minlive[t];
		  // PI <- PI U {<d, o, l, {}>,<d, o,-l, {}}
		  PresolverPrecedence pred(d, o, l, {{}});
		  PresolverPrecedence pred1(o, d, -l, {{}});

		  PI.push_back(pred);
		  PI.push_back(pred1);
		} else if (t == T[0]) {
		  gen_data_precedences1(d, o, p, q, {}, opnd_to_lat, PI);
		} else {
		  // PI <- PI U
		  // GenDataPrecedences1(d, o, p, q, {eq(p(q), t(t))},
		  // OpndToLat)
		  UnisonConstraintExpr lit(CONNECTS_EXPR, {q,t}, {});
		  gen_data_precedences1(d, o, p, q, {lit}, opnd_to_lat, PI);
		}
	      }
            }
	  }
        }
    }
}

void gen_data_precedences1(operation d, operation o,
			   operand p, operand q,
			   const presolver_conj& Conj,
			   map<operand,map<instruction,latency>>& opnd_to_lat,
			   precedence_set& PI) {
    map<instruction, latency> Lp = opnd_to_lat[p];
    map<instruction, latency> Lq = opnd_to_lat[q];

    auto comp = [](pair<instruction, latency> p1,
                   pair<instruction, latency> p2) {
                    return p1.second < p2.second;
            };
    latency min_Lp = min_element(Lp.begin(), Lp.end(), comp)->second;
    latency min_Lq = std::min_element(Lq.begin(), Lq.end(), comp)->second;
    latency l = min_Lp + min_Lq;

    PresolverPrecedence pd(d,o,l,{Conj});
    PI.push_back(pd);

    for(const pair<instruction,latency>& mp : Lp) {
        instruction ip = mp.first;
        latency lp     = mp.second;
        for(const pair<instruction,latency>& mq : Lq) {
            instruction iq = mq.first;
            latency lq     = mq.second;
            if(lp + lq > l) {
	      UnisonConstraintExpr lit1(IMPLEMENTS_EXPR, {d,ip}, {});
	      UnisonConstraintExpr lit2(IMPLEMENTS_EXPR, {o,iq}, {});
                presolver_conj conj = Conj;
                // conj.push_back(lit1);
		// conj.push_back(lit2);
		vector_insert(conj, lit1);
		vector_insert(conj, lit2);

                PresolverPrecedence pd(d, o, lp + lq, {conj});
                PI.push_back(pd);
            }
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

static bool distinct_cycle(const Parameters& input,
			   operation i,
			   operation j,
			   const vector<vector<vector<int>>>& min_con_erg) {
  for (resource r : input.R)
    if (min_con_erg[i][r][0] + min_con_erg[j][r][0] > input.cap[r])
      return true;
  return false;
}

#define FastPair(o1,o2) (((o1)<<20) + (o2))

void gen_region_init(const Parameters& input,
		     map<block,vector<vector<operation>>>& edgeset_map,
		     vector<vector<vector<int>>>& min_con_erg,
		     map<int,int>& pweight,
		     precedence_set& PI) {
  presolver_conj ConjTrue;
  presolver_disj DisjTrue({ConjTrue});
    
  for(operation o : input.O) {
    min_con_erg[o] = vector<vector<int>>(input.R.size());
    for(resource r : input.R) {
      int minc = 9999;
      int mine = 9999;
      for(instruction i : input.instructions[o]) {
	if(minc>input.con[i][r]) {
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
  // FUN-FUN, DEFINE-*, and *-KILL precedences are useless here
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
      edgeset_map[input.oblock[j]].push_back({i,jpred});
      PI.push_back(PresolverPrecedence(i, jpred, 1, DisjTrue));
    } else if ((input.type[i] == CALL || input.type[i] == TAILCALL) && input.type[j] != FUN) {
      edgeset_map[input.oblock[j]].push_back({isucc,j});
      PI.push_back(PresolverPrecedence(isucc, j, 1, DisjTrue));
    } else if (input.type[i] != DEFINE && input.type[j] != KILL) {
      edgeset_map[input.oblock[j]].push_back(edge);
      PI.push_back(PresolverPrecedence(i, j, 1, DisjTrue));
    }
  }

  for (const PresolverPrecedence& p : PI) {
    operation src = p.i;
    operation dest = p.j;
    int n = p.n;
    presolver_disj D = p.d;
    if (src<dest && disj_is_true(D)) {
      int key = FastPair(src,dest);
      if (pweight.find(key) == pweight.end() || pweight[key] < n)
	pweight[key] = n;
    }
  }
}

void gen_region_precedences(const Parameters& input,
			    const map<block,vector<vector<operation>>>& edgeset_map,
			    const vector<vector<vector<int>>>& min_con_erg,
			    map<int,int>& pweight,
			    precedence_set& PI) {
  for(const pair<block,vector<vector<operation>>>& edgeset_pair : edgeset_map) {
    vector<operation> pnodes;
    Digraph G = Digraph(edgeset_pair.second).reduction();
    partition_nodes(G, pnodes, {});
    gen_region_per_partition(input, G, pnodes, {}, {}, min_con_erg, pweight, PI);
  }
}

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

  return ld+lu;
};

void gen_region_precedences(const Parameters& input,
			    map<block,vector<vector<operation>>>& edgeset_map,
			    const vector<vector<vector<int>>>& min_con_erg,
			    map<int,int>& pweight,
			    const vector<int>& ass,
			    precedence_set& PI) {
  // add edges induced by assumption pi = tj
  operand p = ass[0];
  temporary t = ass[1];
  operation o3 = opnd_oper(input, p);
  operation o2 = temp_oper(input, t);
  operation o1 = input.type[o2] != COPY ? -1 : temp_oper(input, first_temp_but_null(input, first_use(input, o2)));
  block b = input.oblock[o3];
  
  if (o1>=0) {
    operand d2 = first_def(input, o2);
    operand u2 = first_use(input, o2);
    temporary t2 = first_temp_but_null(input, u2);
    operand d1 = temp_def(input, t2);
    int d12 = min_latency(input, o1, o2, d1, u2);
    int d23 = min_latency(input, o2, o3, d2, p);
    if (d12>0 && d23>0) {
      vector<vector<operation>> edgeset;
      edgeset.insert(edgeset.end(), edgeset_map[b].begin(), edgeset_map[b].end());
      vector<operation> pnodes;
      
      edgeset.push_back({o1,o2});
      edgeset.push_back({o2,o3});
      Digraph G = Digraph(edgeset).reduction();
      // cerr << "ASSUMING p" << p << "=t" << t << " o1=" << o1 <<" o2=" << o2 <<" o3=" << o3 << endl;
      partition_nodes(G, pnodes, {o1,o3});
      gen_region_per_partition(input, G, pnodes, {o1,o3,d12+d23}, ass, min_con_erg, pweight, PI);
    }
  }
}

void partition_nodes(Digraph& G,
		     vector<operation>& pnodes,
		     const vector<int>& focus) {
  operation l = G.vertices()[0];
  for(operation v : G.vertices()) {
    if (v==l && (focus.empty() || v <= focus[0] || v >= focus[1])) // no partition nodes inside the focus
      pnodes.push_back(v);
    for(operation n : G.neighbors(v))
      l = n > l ? n : l;
  }
}

// naive depth-first search
static int longest_path(Digraph &G, map<int,int>& pweight, operation src, operation sink, int len) {
  if (src > sink) {
    return 0;
  } else if (src == sink) {
    return len;
  } else {
    int lp = len;
    vector<operation> ns = G.neighbors(src);
    for (operation n : ns) {
      int key = FastPair(src,n);
      int weight = (pweight.find(key) != pweight.end() ? pweight[key] : 0);
      int nl = longest_path(G, pweight, n, sink, len+weight);
      lp = (nl > lp ? nl : lp);
    }
    return lp;
  }
}

void gen_region_per_partition(const Parameters& input,
			      Digraph& G, // "precs" edges for 1 block
			      const vector<operation>& pnodes,
			      const vector<int>& focus,
			      const vector<int>& ass,
			      const vector<vector<vector<int>>>& min_con_erg,
			      map<int,int>& pweight,
			      precedence_set& PI) {
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

  if (!ass.empty()) {
    operand p = ass[0];
    temporary t = ass[1];
    operation src = focus[0];
    operation sink = focus[1];
    int lat = focus[2];
    int lp = longest_path(G, pweight, src, sink, 0);

    // vector<operation> region = ord_union(G.reachables(src), {src});
    // map<operation,int> src_cps = dag_longest_paths_fwd(region, pweight);
    // by construction, sink is reachable from src
    if(lp<lat) {
      UnisonConstraintExpr e(CONNECTS_EXPR, {p,t}, {});
      presolver_conj Conj({e});
      PresolverPrecedence pred(src, sink, lat, presolver_disj({Conj}));
      // cerr << "  PAIR PRECEDENCE: " << show(pred) << endl;
      PI.push_back(pred);
      // pweight[FastPair(src,sink)] = lat; // only holds under assumption p...=t...
    }
  }
  
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
	  if (focus.empty() || (vj <= focus[0] && focus[1] <= vi)) 
	    gen_region(input, vj, vi, G, H, ass, min_con_erg, pweight, PI);
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
		operation src, operation sink,
		Digraph& G, // forward
		Digraph& H, // backward
		const vector<int>& ass,
		const vector<vector<vector<int>>>& min_con_erg,
		map<int,int>& pweight,
		precedence_set& PI) {
  int glb = 0;
  vector<operation> inside = ord_intersection(G.reachables(src), H.reachables(sink)); // excludes src, sink
  vector<operation> region = ord_union(inside, {src,sink});
  vector<operation> reverse = vector<operation>(region.size());
  unsigned int rs = region.size();

  for (unsigned int ii = 0; ii < rs; ii++)
    reverse[rs-ii-1] = region[ii];

  map<operation,int> src_cps = dag_longest_paths_fwd(region, pweight);
  map<operation,int> sink_cps = dag_longest_paths_bwd(reverse, pweight);
  for (resource r : input.R) {
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
  if (glb <= src_cps[sink]) {
  } else if (ass.empty()) {
    presolver_conj Conj;
    PresolverPrecedence pred(src, sink, glb, presolver_disj({Conj}));
    // cerr << "  REGION PRECEDENCE: " << show(pred) << endl;
    PI.push_back(pred);
    pweight[FastPair(src,sink)] = glb;
  } else {
    operand p = ass[0];
    temporary t = ass[1];
    presolver_conj Conj({UnisonConstraintExpr(CONNECTS_EXPR, {p,t}, {})});
    PresolverPrecedence pred(src, sink, glb, presolver_disj({Conj}));
    // cerr << "  REGION PRECEDENCE: " << show(pred) << endl;
    PI.push_back(pred);
    // pweight[FastPair(src,sink)] = glb; // only holds under assumption p...=t...
  }
}

// Find all longest paths from implicit src, assuming we have a DAG, assuming ascending vertices by top sort
map<operation,int> dag_longest_paths_fwd(vector<operation>& region, map<int,int>& pweight) {
  map<operation,int> L;

  for(operation v : region)
    L[v] = 0;
  for(operation b : region)
    for(operation v : region)
      if(b<v) {
	int key = FastPair(b,v);
	if(pweight.find(key) != pweight.end()) {
	  int w = pweight[key];
	  if(L[v] < L[b]+w)
	    L[v] = L[b]+w;
	}
      }
  return L;
}

// Find all longest paths from implicit src, assuming we have a DAG, assuming ascending vertices by top sort
map<operation,int> dag_longest_paths_bwd(vector<operation>& region, map<int,int>& pweight) {
  map<operation,int> L;

  for(operation v : region)
    L[v] = 0;
  for(operation b : region)
    for(operation v : region)
      if(b>v) {
	int key = FastPair(v,b);
	if(pweight.find(key) != pweight.end()) {
	  int w = pweight[key];
	  if(L[v] < L[b]+w)
	    L[v] = L[b]+w;
	}
      }
  return L;
}

vector<operation> region_finishers(Digraph &R,
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
		          Digraph &R,
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

map<operand, map<instruction, latency>> compute_opnd_to_lat(const Parameters& input) {
    map<operand, map<instruction, latency>> M;
    for(operation o : input.O) {
        vector<operand> operands = oper_opnds(input, o);
        for(unsigned int p = 0; p < operands.size(); ++p) {
            map<instruction, latency> M1;
            vector<instruction> instructions =
                oper_insns(input, o);
            for(unsigned int i = 0; i < instructions.size(); ++i) {
                if(instructions[i] != NULL_INSTRUCTION) {
                    M1.insert(make_pair(instructions[i], input.lat[o][i][p]));
                }
            }
            M.insert(make_pair(operands[p], M1));
        }
    }
    return M;
}

void gen_before_precedences(const Parameters& input,
                            PresolverOptions & options,
			    const vector<PresolverBeforeJSON>& before,
			    precedence_set& PI,
                            Support::Timer & t) {
    // M <- empty
    multimap<PrecedenceEdge, presolver_conj> M;
    vector<PrecedenceEdge> M_keys; // Storing keys for convenience
    unsigned int i = 0;
    // For all <p,q,Disj> in Before
    for(const PresolverBeforeJSON& b : before) {
        // M <- M U GenBeforePrecedences1(p,q,Disj)
        multimap<PrecedenceEdge, presolver_conj> M1 =
	  gen_before_precedences1(input, b.p, b.q, expr_to_disj(b.e));
        for(auto p : M1) {
	  M_keys.push_back(p.first);
	  M.insert(p);
	}
        if ((i % 16 == 0) &&
            timeout(t, options, "gen_before_precedences (1st loop)", t, false))
          return;
        i++;
    }
    i = 0;
    // return {<p,s,n,KernelSet(Disj, empty)> | <p,s,n> -> Disj in M }
    for(const PrecedenceEdge& k : M_keys) {
        // Get Disj
        auto range = M.equal_range(k);
        presolver_disj disj;
        for(auto it = range.first; it != range.second; ++it) {
            disj.push_back(it->second);
        }
        PresolverPrecedence e(k.i, k.j, k.n, kernel_set(disj, {}, -1));
        PI.push_back(e);
        if ((i % 16 == 0) &&
            timeout(t, options, "gen_before_precedences (2nd loop)", t, false))
          return;
        i++;
    }
}

void before_rule(const Parameters& input,
		 const presolver_conj& conj1,
		 const operation o,
		 const operation o1,
		 const presolver_conj& conj,
		 std::multimap<PrecedenceEdge, presolver_conj>& map) {
    // if o != o'
    if(o != o1) {
        // AddToMap(<o,o',0> -> Conj' U Conj)
        PrecedenceEdge e;
        e.i = o;
	e.j = o1;
	e.n = 0;
        presolver_conj u;
        for(const UnisonConstraintExpr& l : conj1) {
	  if (l.id != CONNECTS_EXPR || opnd_temps(input, l.data[0]).size() > 1) // not entailed?
	    vector_insert(u, l);
	}
        for(const UnisonConstraintExpr& l : conj) {
	  if (l.id != CONNECTS_EXPR || opnd_temps(input, l.data[0]).size() > 1) // not entailed?
	    vector_insert(u, l);
	}
        map.insert(make_pair(e,u));
    }
}

multimap<PrecedenceEdge, presolver_conj> gen_before_precedences1(const Parameters& input,
								 operand p, operand q,
								 const presolver_disj& disj) {

    // M <- empty
    multimap<PrecedenceEdge, presolver_conj> M;
    // if p is def and q is def, then
    if(!input.use[p] && !input.use[q]) {
        // t <- min(OpndTempsButNull(p))
        temporary t = first_temp_but_null(input, p);
        // For all r in TempUses(t)
        for(operand r : temp_uses(input, t)) {
            // Or <- OpndOper(r)
            operation o_r = opnd_oper(input, r);
            // For all Conj in Disj
            for(const presolver_conj& conj : disj) {
                // M <- BeforeRule({eq(p(r), t(t))}, Or, OpndOper(q), Conj, M)
	      UnisonConstraintExpr lit1(CONNECTS_EXPR, {r,t}, {});
	      before_rule(input, { lit1 }, o_r, opnd_oper(input, q), conj, M);
            }
        }
    } else if (!input.use[p] && input.use[q]) {
        // t <- min(OpndTempsButNull(p))
        temporary t = first_temp_but_null(input, p);
        // for all r in TempUses(t)
        for(operand r : temp_uses(input, t)) {
            // or <- OpndOper(r)
            operation o_r = opnd_oper(input, r);
            // for all t' in OpndTempsButNull(q)
            for(temporary t1 : opnd_temps(input, q)) {
	      if(t1!=NULL_TEMPORARY) {
                // for all conj in disj
                for(const presolver_conj& conj : disj) {
                    // M <- before_rule(input, {eq(p(r), t(t)), eq(p(q), t(t'))},
                    // or, TempOper(t'), Conj, M)
		  UnisonConstraintExpr lit1(CONNECTS_EXPR, {r,t}, {});
		  UnisonConstraintExpr lit2(CONNECTS_EXPR, {q,t1}, {});
		  before_rule(input, {lit1, lit2}, o_r, temp_oper(input, t1), conj, M);
                }
	      }
            }
        }
    } else if (input.use[p] && !input.use[q]) {
        // for all r in TempUses(min(OpndTempsButNull(p)))
        temporary min_t = first_temp_but_null(input, p);
        for(temporary r : temp_uses(input, min_t)) {
            // or <- OpndOper(r)
            operation o_r = opnd_oper(input, r);

            // Compute OpndTempsButNull(p) intersecting
            // OpndTempsButNull(r)
            vector<temporary> inter = ord_intersection(opnd_temps(input, p), opnd_temps(input, r));

            // For all t in intersection
            for(temporary t : inter) {
	      if(t!=NULL_TEMPORARY) {
                // For all Conj in Disj
                for(const presolver_conj& conj : disj) {
                    // M <- BeforeRule({eq(p(r), t(t)), eq(p(p), t(t))}, or,
                    // OpndOper(q), Conj, M)
		  UnisonConstraintExpr lit1(CONNECTS_EXPR, {r,t}, {});
		  UnisonConstraintExpr lit2(CONNECTS_EXPR, {p,t}, {});
		  before_rule(input, {lit1, lit2}, o_r, opnd_oper(input, q), conj, M);
                }
	      }
            }
        }
    } else if (input.use[p] && input.use[q]) {
        // For all r in TempUses(min(OpndTempsButNull(p)))
        temporary min_t = first_temp_but_null(input, p);
        for(operand r : temp_uses(input, min_t)) {
            // or <- OpndOper(r)
            operation o_r = opnd_oper(input, r);
            // For all t' in OpndTempsButNull(q)
            for(temporary t1 : opnd_temps(input, q)) {
	      if(t1!=NULL_TEMPORARY) {
                // ot' <- TempOper(t')
                operation o_t1 = temp_oper(input, t1);
                // Compute OpndTempsButNull(p) intersecting
                // OpndTempsButNull(r)
		vector<temporary> inter = ord_intersection(opnd_temps(input, p), opnd_temps(input, r));
                // for all t in intersection
                for(temporary t : inter) {
		  if(t!=NULL_TEMPORARY) {
		    // for all Conj in Disj
		    for(const presolver_conj& conj : disj) {
		      // {eq(p(r), t(t))}
		      UnisonConstraintExpr lit1(CONNECTS_EXPR, {r,t}, {});
		      // {eq(p(p), t(t))}
		      UnisonConstraintExpr lit2(CONNECTS_EXPR, {p,t}, {});
		      // {eq(p(q), t(t'))}
		      UnisonConstraintExpr lit3(CONNECTS_EXPR, {q,t1}, {});
		      before_rule(input, {lit1, lit2, lit3}, o_r, o_t1, conj, M);
		    }
		  }
		}
	      }
	    }
        }
    }
    return M;
}

void gen_long_latency(Parameters& input) {
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
		input.long_latency.push_back(pq);
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
		input.long_latency.push_back(pq);
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
	      input.long_latency.push_back(pq2);
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
	      input.long_latency.push_back(pq2);
	    }
	  }
	}
      }
    }
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
