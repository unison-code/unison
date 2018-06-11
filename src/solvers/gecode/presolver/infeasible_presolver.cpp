/*
 *  Main authors:
 *    Erik Ekstrom <eeks@sics.se>
 *    Mats Carlsson <mats.carlsson@ri.se>
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
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
 * Presolver for generating nogoods constraints data. Part of C++ Presolver.
 */

#include "infeasible_presolver.hpp"

using namespace std;

// constructor
InfeasiblePresolver::InfeasiblePresolver(PresolverAsserts& PA,
					 Parameters& input,
					 Support::Timer & t,
					 PresolverOptions &options) :
  PA(PA),
  input(input),
  param_overstrict(false),
  options(options),
  timer(t) {
};

void InfeasiblePresolver::setup(void) {

  // Prebuild congr congr_map
  generate_congruence_operands(input, congr_map);

  // Prebuild copyrelated temps
  generate_copyrel_temps_map(input, copy_rel_temps);

  // Generate and cache odd/even_operand
  gen_odd_even_operands(odd_operand, even_operand);

  // Generate and cache opnd_temps_but_null
  vector<temporary> notemps;
  temps_but_null = vector<vector<temporary>>(input.P.size(), notemps);
  for(operand p : input.P)
    temps_but_null[p] = opnd_temps_but_null(input, p);

  // Compute equality set U of mandatory and possible temporary assignments; D,
  // the sets of mutually non-overlapping operands populated by temporaries with
  // nonempty live ranges; and V, the set of all temporands that occur in U.

  for(operation o : input.O) {

    temporand_set Du, Dd;

    for (operand p : input.operands[o]) {
      vector<temporary> T = opnd_temps(input, p);
      temporary t1 = first_temp_but_null(input, p);
      int s = T.size();
      if(T[0]==NULL_TEMPORARY)
	s--;
      int f = s == 1 ? 0 : 1;
      for (temporary t : T) {
	if(t!=NULL_TEMPORARY)
	  U.push_back({Temporand::p(p), Temporand::t(t), Temporand::n(f)});
      }
      if (input.minlive[t1] > 0 && // and p is mandatory [MC]
	  T[0] != NULL_TEMPORARY) {
        if (!input.use[p]) {
          Dd.push_back(Temporand::p(p));
        } else {
          Du.push_back(Temporand::p(p));
        }
      }
    }
    vector<operand> ods = oper_defs(input, o),
                    ous = oper_uses(input, o);
    if (oper_type(input, o) == LOW) {

      vector_insert(U, {Temporand::p(ods[0]),
	                Temporand::p(ous[0]),
	                Temporand::n(0)});
    } else if (o != 0 &&
               oper_type(input, o) != COPY &&
               oper_type(input, o) != FUN &&
               oper_type(input, o) != KILL) {
      auto U_D = alldiffs_for_co_use(Du,Dd,U,D);
      U = U_D.first;
      D = U_D.second;
    }
  }

  // U <- U union {<p(p),r,0> | <p,r> in  input.preassign}
  for(const vector<int>& pr : input.preassign) {
    vector_insert(U, {Temporand::p(pr[0]), Temporand::r(pr[1]), Temporand::n(0)});
  }

  // U <- U union {<p(p), p(q), 0> | q is lexical successor of p in
  //                                 some congurence }
  for(const vector<operand>& C : input.strictly_congr) {
    for(unsigned i = 1; i < C.size(); i++) {
      vector_insert(U, {Temporand::p(C[i-1]),
	                Temporand::p(C[i]),
	                Temporand::n(0)});
    }
  }

  temporand_set V;
  // V <- {v | <v', v'',_> in U and (v = v' || v = v'')}
  for(const vector<Temporand>& u : U){
    if(u.size() == 3) {
      vector_insert(V, u[0]);
      vector_insert(V, u[1]);
    }
  }

  for(const Temporand& v : V) {
    ds_set.make_set(v);
  }


  for(const vector<Temporand>& u : U) {
    if((u.size() == 3) && (u[2] == Temporand::n(0))) {
      ds_set.make_union(u[0], u[1]);
    }
  }

  vector<operand> P_c;
  vector<pair<operand, operand> > E;

  // P_c <- U{c in input.strictly_congr | |c| >= 2}, all ps involved in congurences
  for(const vector<operand>& c : input.strictly_congr) {
    if(c.size() >= 2) {
      for(const operand p : c) {
	vector_insert(P_c, p);
      }
    }
  }

  // E <- {<p,q> | <p,q> in JSON.before &&
  //               (r = q || (q is mandatory def and copy related to r))
  for(const PresolverBeforeJSON& b : input.before) {
    if(b.e.id==AND_EXPR && b.e.children.empty()) { // b.d = {{}}
      operand p = b.p;
      operand q = b.q;
      E.push_back(make_pair(p,q));

      // Check if q is mandatory def operand
      if(!input.use[q] && input.temps[q].size() == 1) {
	// Add edge from p to each operand copyrelated to q
	for(const vector<operand>& qs : input.copyrel) {
	  // Find copyrel class of q
	  if(qs[0] == q){
	    for(unsigned i = 1; i < qs.size(); i++) {
	      vector_insert(E, make_pair(p,qs[i]));
	    }
	    break;
	  }
	}
      }
    }
  }

  Digraph G(E);
  // Gen reachables R
  vector<operand> nops;
  R = vector<vector<operand>>(input.P.size(), nops);
  for(operand p : G.vertices()) {
    R[p] = G.reachables(p);
  }

  for (const block b : input.B) {
    for (const operand p :
	   ord_intersection(input.operands[input.ops[b].front()], P_c)) {
      for (const operand q :
	     ord_intersection(input.operands[input.ops[b].back()], P_c)) {

	if(!ord_intersect(input.temps[p], input.temps[q])) {
	  D_7.push_back({Temporand::p(p), Temporand::p(q)});
	}
      }
    }

    temporand_set _D_5;
    // D_5 <- {{t(t)| t in input.tmp[b]}}
    for(const temporary t : input.tmp[b]) {
      _D_5.push_back(Temporand::t(t));
    }
    D_5.push_back({_D_5});


    // Precompute candidate nogoods temporands for D_5
    D_5_Cands.push_back(gen_candidates({_D_5}, U));
  }

  temporand_set _rs;
  for(const vector<int>& pr : input.preassign) {
    vector_insert(_rs, Temporand::r(pr[1]));
  }

  D_4.push_back(_rs);
  for(const temporand_set& A : D) {
    if(A.size() >= 2) {
      vector_insert(D_4, A);
    }
  }

  // [MC] 2016-02-22 modulo_alldiffs
  // Copy related operands (indexed with operand)
  vector<vector<operand> > copy_rel_operands;
  generate_copyrel_operands_map(input, copy_rel_operands);

  for(operand p : input.P) {
    if (even_operand[p]) {
      for(operand q : copy_rel_operands[p]) {
	if (p<q && odd_operand[q]) {
	  temporand_set _PQ = {Temporand::p(p), Temporand::p(q)};
	  vector_insert(D_4, _PQ);
	}
      }
    } else if (odd_operand[p]) {
      for(operand q : copy_rel_operands[p]) {
	if (p<q && even_operand[q]) {
	  temporand_set _PQ = {Temporand::p(p), Temporand::p(q)};
	  vector_insert(D_4, _PQ);
	}
      }
    }
  }

  // Precompute candidate nogoods temporands for D_4
  D_4_Cands = gen_candidates(D_4, U);

  // Precompute candidate nogoods temporands for D_7
  D_7_Cands = gen_candidates(D_7, U);
}

void InfeasiblePresolver::pass1(vector<temporand_set>& Alldiffs, vector<presolver_conj>& Nogoods) {
  // F <- F union redefined_operand_nogoods()
  redefined_operand_nogoods(Nogoods);

  // F <- F union before_in_nogoods()
  before_in_nogoods(Nogoods);

  // F <- F union xchg_nogoods()
  xchg_nogoods(Nogoods);

  // F <- F union regdomain_nogoods()
  regdomain_nogoods(Nogoods);

  // F <- F union dominsn_nogoods()
  dominsn_nogoods(Nogoods);

  // sort and purge duplicates
  sort(Nogoods.begin(), Nogoods.end());
  Nogoods.erase(unique(Nogoods.begin(), Nogoods.end()), Nogoods.end());

  // Generate set of all A in D where |A| >= 2 and A consists of obligatory operands [MC]
  for(const temporand_set& A : D) {
    if(A.size() >= 2) {
      bool skip = false;
      for(const Temporand& a : A) {
	if(!a.is_operand() || input.temps[a.id()][0] == NULL_TEMPORARY) {
	  skip = true;
	  break;
	}
      }
      if(!skip) {
	vector_insert(Alldiffs, A);
      }
    }
  }
}

void InfeasiblePresolver::pass2(vector<presolver_conj>& Nogoods) {

  cutoff = (options.timeout() + timer.stop()) / 2;

  for (const block b : input.B) {
    // F <- F union single_nogoods(D_5, U,[G])
    single_nogoods(D_5[b], D_5_Cands[b], &R, true, Nogoods);
  }
  // F <- F union single_nogoods(D_4, U, true)
  single_nogoods(D_4, D_4_Cands, nullptr, false, Nogoods);

  // F <- F union single_nogoods(D_7, U,[G])
  single_nogoods(D_7, D_7_Cands, &R, false, Nogoods);

  for (const block b : input.B) {
    // F <- F union double_nogoods(D_5, U,[G])
    double_nogoods(D_5[b], D_5_Cands[b], &R, true, Nogoods);
  }

  // F <- F union double_nogoods(D_4, U, true)
  double_nogoods(D_4, D_4_Cands, nullptr, false, Nogoods);

  // F <- F union double_nogoods(D_7, U,[G])
  double_nogoods(D_7, D_7_Cands, &R, false, Nogoods);

  // sort and purge duplicates
  sort(Nogoods.begin(), Nogoods.end());
  Nogoods.erase(unique(Nogoods.begin(), Nogoods.end()), Nogoods.end());
}

void InfeasiblePresolver::redefined_operand_nogoods(vector<presolver_conj>& Nogoods) {
  vector<operation> Rs;
  for(const operation o : input.O) {
    if(!input.redefined[o].empty()) {
      Rs.push_back(o);
    }
  }
  for (operation o1 : Rs)
    for (operation o2 : Rs)
      if (o1 < o2)
        for (pair<operand, operand> pp : input.redefined[o1])
          for (pair<operand, operand> qq : input.redefined[o2]) {
            operand p = pp.first, p2 = pp.second,
                    q = qq.first, q2 = qq.second;
	    unsigned int d1i = find_index(input.operands[o1], p2);
	    unsigned int d2i = find_index(input.operands[o2], q2);
	    if(ord_intersect(input.temps[p], input.temps[q])) {
	      vector<presolver_conj> D;
	      unsigned int n = 0;
	      for (unsigned int i1i = 0; i1i < input.instructions[o1].size(); i1i++) {
		for (unsigned int i2i = 0; i2i < input.instructions[o2].size(); i2i++) {
		  n++;
		  if(input.lat[o1][i1i][d1i]>0 && input.lat[o2][i2i][d2i]>0) {
		    instruction i1 = input.instructions[o1][i1i];
		    instruction i2 = input.instructions[o2][i2i];
		    presolver_conj c;
		    UnisonConstraintExpr e1(SHARE_EXPR, {p,q}, {});
		    UnisonConstraintExpr e2(IMPLEMENTS_EXPR, {o1,i1}, {});
		    UnisonConstraintExpr e3(IMPLEMENTS_EXPR, {o2,i2}, {});
		    c.push_back(e1);
		    c.push_back(e2);
		    c.push_back(e3);
		    D.push_back(c);
		  }
		}
	      }
	      if(D.size()==n)
		vector_insert(PA.cur_difftemp, {p,q});
	      else
		Nogoods.insert(Nogoods.end(), D.begin(), D.end());
	    }
	  }
}

void InfeasiblePresolver::before_in_nogoods(vector<presolver_conj>& Nogoods) {
  for(const PresolverBeforeJSON& b : input.before) {
    vector<temporary> Ts = input.temps[b.q];
    temporary T1 = Ts[0];
    if(b.e.id==AND_EXPR && b.e.children.empty() && // b.d = {{}}
       T1 >= 0 &&
       input.type[input.oper[b.q]] == OUT &&
       input.type[input.def_opr[T1]] == IN) {
      UnisonConstraintExpr e(CONNECTS_EXPR, {b.q,T1}, {});
      presolver_conj c = {e};
      Nogoods.push_back(c);
    }
  }
}

void InfeasiblePresolver::xchg_nogoods(vector<presolver_conj>& Nogoods) {
  // Xchg basically reconstructs "interchangeable", which currently seems inaccurate
  map<vector<vector<int>>,vector<operation>> Xchg;
  for(const operation o : input.O) {
    if(input.type[o]==COPY) {
      vector<vector<int>> key;
      key.push_back(input.instructions[o]);
      for(operand p : input.operands[o])
	if(input.use[p])
	  key.push_back(input.temps[p]);
      Xchg[key].push_back(o);
    }
  }
  for(const pair<vector<vector<int>>,vector<operation>>& k_os : Xchg) {
    if(k_os.second.size() > 1) {
      vector<temporary> ts;
      for(operation o : k_os.second)
	ts.push_back(first_temp_but_null(input, first_def(input, o)));
      unsigned int i = 0;
      for(operand p : temp_uses(input, ts[0])) {
	i++;
	for(unsigned int j=i; j<ts.size(); j++) {
	  if(ord_contains(input.temps[p], ts[j])) {
	    UnisonConstraintExpr e(CONNECTS_EXPR, {p,ts[j]}, {});
	    presolver_conj c = {e};
	    Nogoods.push_back(c);
	  }
	}
      }
    }
  }
}

void InfeasiblePresolver::regdomain_nogoods(vector<presolver_conj>& Nogoods) {
  map<vector<vector<register_class>>,vector<operation>> M;
  map<operand,vector<register_atom>> P2D;
  map<temporary,vector<register_atom>> T2D;
  // group operations by insn set and use, def counts
  for(operation o : input.O) {
    vector<vector<register_class>> key(input.rclass[o]);
    M[key].push_back(o);
  }
  // for each group, for each operand, compute its MGRD (most general reg domain) from atoms
  // for each temp, get its MGRD from its definer
  for(const pair<vector<vector<register_class>>,vector<operation>>& is_os : M) {
    map<unsigned int,vector<register_atom>> MGRD;
    // mgrd_per_operand(is_os.second[0], MGRD);
    operation o1 = is_os.second[0];
    unsigned int nbinsn = input.instructions[o1].size();
    unsigned int nbopnd = input.operands[o1].size();
    for(unsigned int i=0; i<nbopnd; i++) {
      MGRD[i] = {};
      for(unsigned int j=0; j<nbinsn; j++) {
	if(input.instructions[o1][j] != 0) {
	  register_class rc = input.rclass[o1][j][i];
	  MGRD[i] = ord_union(MGRD[i], input.atoms[rc]);
	}
      }
    }
    for(operation o : is_os.second) {
      for(unsigned int i=0; i<nbopnd; i++) {
	operand p = input.operands[o][i];
	if(input.use[p]) {
	  P2D[p] = MGRD[i];
	} else {
	  T2D[first_temp_but_null(input, p)] = MGRD[i];
	}
      }
    }
  }
  // replace MGRD values by singletons for preassignments
  for(const vector<int>& pr : input.preassign) {
    if(input.use[pr[0]]) {
      P2D[pr[0]] = {pr[1]};
    } else {
      T2D[first_temp(input, pr[0])] = {pr[1]};
    }
  }
  // for each use operand p, for each t in temps(p), if MGRD(p) disjoint MGRD(t), then p != t
  for(operand p : input.P) {
    vector<register_atom> pd = P2D[p];
    if(input.use[p]) {
      for(temporary t : input.temps[p]) {
	if(t != NULL_TEMPORARY && !ord_intersect(pd, T2D[t])) {
	  UnisonConstraintExpr e(CONNECTS_EXPR, {p,t}, {});
	  presolver_conj c = {e};
	  Nogoods.push_back(c);
	}
      }
    }
  }
}

void InfeasiblePresolver::dominsn_nogoods(vector<presolver_conj>& Nogoods) {
  // exclude instructions that imply alignment and the null instruction
  vector<int> A;
  A.push_back(NULL_INSTRUCTION);
  for(const vector<int>& tuple : input.aligned) {
      instruction i = tuple[1], j = tuple[3];
      A.push_back(i);
      A.push_back(j);
  }
  sort(A.begin(), A.end());
  A.erase(unique(A.begin(), A.end()), A.end());
  // build M: potential alt. insns, their reg classes, and operations in which they occur
  map<PresolverInsn2Class2,vector<operation>> M;
  for(operation o : input.O) {
    vector<instruction> is(input.instructions[o]);
    for(unsigned ii=0; ii<is.size(); ii++)
      if(!ord_contains(A, is[ii]))
	for(unsigned jj=ii+1; jj<is.size(); jj++)
	  if(!ord_contains(A, is[jj]))
	    if(input.lat[o][ii] == input.lat[o][jj]) {
	      PresolverInsn2Class2 iicc;
	      iicc.insn1 = is[ii];
	      iicc.insn2 = is[jj];
	      iicc.class1 = input.rclass[o][ii];
	      iicc.class2 = input.rclass[o][jj];
	      M[iicc].push_back(o);
	    }
  }
  // build R: actual dominated insn, their reg classes, and operations in which they occur
  vector<pair<PresolverInsnClass,vector<operation>>> R;
  for(const pair<PresolverInsn2Class2,vector<operation>>& t_os : M) {
    instruction i1 = t_os.first.insn1;
    instruction i2 = t_os.first.insn2;
    vector<register_class> c1 = t_os.first.class1;
    vector<register_class> c2 = t_os.first.class2;
    PresolverInsnClass ic;
    for(resource r : input.R)
      if(input.con[i1][r] > input.con[i2][r] || input.dur[i1][r] > input.dur[i2][r])
	goto next1;
    for(unsigned jj=0; jj<c1.size(); jj++)
      if(!subseteq(input.atoms[c1[jj]], input.atoms[c2[jj]]))
	goto next1;
    ic.insn = i2;
    ic.rclass = c1;
    R.push_back(make_pair(ic,t_os.second));
    goto next2;
  next1:
    for(resource r : input.R)
      if(input.con[i2][r] > input.con[i1][r] || input.dur[i2][r] > input.dur[i1][r])
	goto next2;
    for(unsigned jj=0; jj<c1.size(); jj++)
      if(!subseteq(input.atoms[c2[jj]], input.atoms[c1[jj]]))
	goto next2;
    ic.insn = i1;
    ic.rclass = c2;
    R.push_back(make_pair(ic,t_os.second));
  next2:
    ;
  }
  // emit nogoods
  for(const pair<PresolverInsnClass,vector<operation>>& ic_os : R) {
    for(operation o : ic_os.second) {
      unsigned ii;
      vector<instruction> is(input.instructions[o]);
      for(ii=0; ii<is.size(); ii++)
	if (is[ii]==ic_os.first.insn)
	  break;
      vector<operand> ps = input.operands[o];
      vector<register_class> rc = ic_os.first.rclass;
      UnisonConstraintExpr e1(IMPLEMENTS_EXPR, {o, ic_os.first.insn}, {});
      presolver_conj c = {e1};
      for(unsigned jj=0; jj<ps.size(); jj++)
	if (rc[jj]!=input.rclass[o][ii][jj]) {
	  UnisonConstraintExpr e2(ALLOCATED_EXPR, {ps[jj], rc[jj]}, {});
	  c.push_back(e2);
	}
      Nogoods.push_back(c);
    }
  }
}

pair<vector<vector<Temporand> >, vector<temporand_set > >
InfeasiblePresolver::alldiffs_for_co_use(const temporand_set& D_u,
					 const temporand_set& D_d,
					 const vector<vector<Temporand> >& U,
					 const vector<temporand_set >& D) {
  vector<vector<Temporand> > _U = U;
  vector<temporand_set > _D = D;
  map<vector<temporary>, temporand_set > M;

  for(const Temporand& p : D_u) {
    if(p.is_operand()) {
      M[temps_but_null[p.id()]].push_back(p);
    }
  }

  //L <- split_chunks_if_alldiff(M)
  const vector<temporand_set >& L = split_chunks_if_alldiff(M);

  // _U <- _U union {<x, y, 1> | {x,y} subset of C}
  for(const temporand_set& C : L) {
    if(C.size() >= 2) {
      for(auto it = C.begin(); it != C.end(); it++) {
  	for(auto it2 = next(it); it2 != C.end(); it2++) {
  	  vector_insert(_U, {*it, *it2, Temporand::n(1)});
  	}
      }
    }
  }


  for(const vector<Temporand>& v : cartesian_product(L)) {
    temporand_set s;
    for(const Temporand& t : v) {
      vector_insert(s, t);
    }
    vector_insert(_D, s);

  }

  // _D <- _D union {D_d}
  vector_insert(_D, D_d);

  return make_pair(_U, _D);
}


vector<temporand_set >
InfeasiblePresolver::split_chunks_if_alldiff(const map<vector<temporary>, temporand_set >& M) {
  vector<temporand_set > L;
  for(auto it = M.begin();
      it != M.end(); it++) {
    temporand_set C = (*it).second;
    if(C.size() >= 2 && (param_overstrict || chunk_is_alldiff(C))) {
      // Assert.cur_difftemp <- Assert_cur_difftemp union {Strip(C)}
      PA.cur_difftemp.push_back(strip(C));

      for(const Temporand& c : C) {
	vector_insert(L, {c});
      }
    }
    else {
      // L <- L union {C}
      vector_insert(L, C);
    }
  }
  return L;
}


bool InfeasiblePresolver::chunk_is_alldiff(const temporand_set& C) {
  // Precond: C contains only operands
  for(const operation o : input.O) {
    if(has_def_proxy(C,o)) return true;
    else if(has_use_proxy(C,o)) return true;
  }
  return false;
}


bool InfeasiblePresolver::has_def_proxy(const temporand_set& C, const operation o) {
  for(const Temporand& p : C) {
    bool has_congruent_def = false;
    const vector<operand>& p_congrs = congr_map[p.id()];
    for(const operand q : oper_defs(input, o)) {
      if(ord_contains(p_congrs, q)) { // q is congruent to p
	has_congruent_def = true;
	break;
      }
    }
    if(!has_congruent_def) return false;
  }
  return true;
}


bool InfeasiblePresolver::has_use_proxy(const temporand_set& C, const operation o) {
  vector<operand> qs;
  for(const Temporand& p : C) {
    bool has_congruent_use = false;
    const vector<operand>& p_congrs = congr_map[p.id()];
    for(const operand q : oper_uses(input, o)) {
      if(ord_contains(p_congrs, q)) { // q is congruent to p
	has_congruent_use = true;
	qs.push_back(q);
	break;
      }
    }
    if(!has_congruent_use) return false;
  }
  vector<vector<temporary> > qstemps;
  for(const operand q : qs) {
    vector_insert(qstemps, input.temps[q]);
  }
  if(qstemps.size() == C.size()) return true;
  return false;
}


bool InfeasiblePresolver::has_disjoint_temporary_domains(const vector<operand>& P) {
  for(unsigned i = 0; i < P.size()-1; i++) {
    for(unsigned j = i+1; j < P.size(); j++) {
      if(ord_intersect(input.temps[P[i]], input.temps[P[j]])) {
	return false;
      }
    }
  }
  return true;
}


void InfeasiblePresolver::single_nogoods(const vector<temporand_set >& D,
					 const vector<vector<nogood_cand_set> >& DCands,
					 const vector<vector<operand> >* R,
					 const bool filter_temps,
					 vector<presolver_conj>& nogoods) {
  vector<temporand_set> DD(D.begin(), D.end());

  // for(int i = 0; i < B7.size(); i++){
  for(unsigned i = 0; i < DCands.size(); i++) {
    const temporand_set& AD1s = DD[i];
    const vector<Temporand> AD1(AD1s.begin(),AD1s.end());
    const vector<nogood_cand_set>& AD3 = DCands[i];

    for(unsigned j = 0; j < AD1.size(); j++) {
      const Temporand& V1 = AD1[j];
      nogood_cand_set U1 = AD3[j];

      for(unsigned k = j+1; k < AD1.size(); k++) {
  	const Temporand& V2 = AD1[k];

	if(!filter_temps ||
 	   !(V1.is_temporary() && V2.is_temporary()) ||
 	   !(copy_rel_temps[V1.id()] == copy_rel_temps[V2.id()])) {

	  nogood_cand_set U2 = AD3[k];
	  sort(U2.begin(), U2.end());
	  sort(U1.begin(), U1.end());
	  const nogood_cand_set U12 = ord_intersection(U1,U2);

	  for(const nogood_cand& V34 : U12) {
	    if(timer.stop() >= cutoff)
	      return;
	    if(!V34.empty()) {
	      const Temporand& V3 = V34[0];
	      const Temporand& V4 = V34[1];

	      enum UnisonConstraintExprId tag = (V4.is_operand() ?
						 SHARE_EXPR :
						 CONNECTS_EXPR);
	      UnisonConstraintExpr e(tag, {V3.id(), V4.id()}, {});
	      presolver_conj c = {e};

	      emit_nogood(R, c, V1, V2, nogoods);
	    }
	  }
	}
      }
    }
  }
}


void InfeasiblePresolver::double_nogoods(const vector<temporand_set >& D,
					 const vector<vector<nogood_cand_set> >& DCands,
					 const vector<vector<operand> >* R,
					 const bool filter_temps,
					 vector<presolver_conj>& nogoods) {
  // By construction are the legnths of D and DCands equal, even for embedded sets/vectors.
  // This is a preconditon.

  // If G is null it is considered true!
  vector<temporand_set> DD(D.begin(), D.end());

  for(unsigned i = 0; i < D.size(); i++) {
    const temporand_set& ds = DD[i];
    const vector<Temporand> d(ds.begin(), ds.end());
    const vector<nogood_cand_set> dcands =  DCands[i];

    for(unsigned j = 0; j < d.size(); j++) {
      for(unsigned k = j+1; k < d.size(); k++) {
	const Temporand& v1 = d[j];
	const nogood_cand_set& u1 = dcands[j];

	const Temporand& v2 = d[k];
	const Temporand& w2 = ds_set.find(v2);
	const nogood_cand_set& u2 = dcands[k];

	// If filtering active, check so that v1 and v2 are not copy related temporaries
	if(!filter_temps ||
	   !(v1.is_temporary() && v2.is_temporary()) ||
	   !(copy_rel_temps[v1.id()] == copy_rel_temps[v2.id()])) {

	  for(const nogood_cand& c1 : u1) {
	    const Temporand& v3 = c1[0];
	    const Temporand& v4 = c1[1];
	    const Temporand& w3 = c1[2];
	    const Temporand& w4 = c1[3];

	    for(const nogood_cand& c2 : u2) {
	      const Temporand& v5 = c2[0];
	      const Temporand& v6 = c2[1];
	      const Temporand& w5 = c2[2];
	      const Temporand& w6 = c2[3];

	      if(timer.stop() >= cutoff)
		return;
	      if((w2 == w6 || w2 == w5) && (v3 != v5)) {

		if(w3 == w5 || w3 == w6 || w4 == w5 || w4 == w6) {
		  enum UnisonConstraintExprId tag34 = (v4.is_operand() ?
						       SHARE_EXPR :
						       CONNECTS_EXPR);
		  enum UnisonConstraintExprId tag56 = (v6.is_operand() ?
						       SHARE_EXPR :
						       CONNECTS_EXPR);
		  UnisonConstraintExpr lit34(tag34, {v3.id(), v4.id()}, {});
		  UnisonConstraintExpr lit56(tag56, {v5.id(), v6.id()}, {});
		  presolver_conj c;
		  if (lit34 < lit56)
		    c = {lit34,lit56};
		  else
		    c = {lit56,lit34};
		  emit_nogood(R, c, v1,v2, nogoods);
		}
	      }
	    }
	  }
	}
      }
    }
  }
}


void InfeasiblePresolver::emit_nogood(const vector<vector<operand> >* R,
				      const presolver_conj& Conj,
				      const Temporand& v1,
				      const Temporand& v2,
				      vector<presolver_conj>& Nogoods) {
  // G is true iff it is nullptr!
  if (R == nullptr) {
    Nogoods.push_back(Conj);
  } else {
    // M <- P <- {}
    map<vector<temporary>, vector<operand> > M;
    vector<operand> P, Q;

    // Collect operands for temps and operands in v1,v2 and Conj
    if(v1.is_operand() && v2.is_operand()) {
      vector_insert(P, v1.id());
      vector_insert(P, v2.id());
    }

    else if(v1.is_temporary() && v2.is_temporary()) {
      vector_insert(P, input.definer[v1.id()]);
      vector_insert(P, input.definer[v2.id()]);
    }

    for(const UnisonConstraintExpr& lit : Conj) {
      // P <- P union {p, definer[t] | eq(p(p),t(t)) in Conj}
      if(lit.id == CONNECTS_EXPR) {
	vector_insert(P, lit.data[0]);
	vector_insert(P, input.definer[lit.data[1]]);
      }

      // P <- P union {p, q| eq(p(p),p(q)) in Conj}
      else if(lit.id == SHARE_EXPR) {
	vector_insert(P, lit.data[0]);
	vector_insert(P, lit.data[1]);
      }
    }

    // Group by temps
    for(operand p : P)
      Q = ord_union(Q, congr_map[p]);
    int cookie = 1000000;
    for(operand q : Q) {

      // There exists a t such that p(q)=t(t) in Conj
      for(const UnisonConstraintExpr& lit : Conj) {

	if(lit.id == CONNECTS_EXPR && lit.data[0] == q) {
	  vector_insert(M[{lit.data[1]}], q);
	  goto nextq;
	}
      }

      // Such t does not exist.
      if(temps_but_null[q].size()==1) {
	vector_insert(M[temps_but_null[q]], q);
      } else {
	vector<temporary> Mkey(temps_but_null[q]);
	Mkey.insert(Mkey.end(), cookie++);
	vector_insert(M[Mkey], q);
      }
    nextq: ;
    }

    if(M.size() > 1) {
      for(auto it1 = M.begin(); it1 != M.end(); it1++ ) {
	for(auto it2 = next(it1); it2 != M.end(); it2++ ) {
  	  // Each subset {TP1.T-->TP1.P, TP2.T-->TP2.P} in M
	  vector<temporary> Ts1 = it1->first;
	  vector<temporary> Ts2 = it2->first;
	  vector<operand> Ps1 = it1->second;
	  vector<operand> Ps2 = it2->second;

	  // Ts1 and Ts2 belongs to same basic block and are disjoint
	  if(input.tb[Ts1[0]] == input.tb[Ts2[0]] && !ord_intersect(Ts1,Ts2)) {
	    // TODO: this might be a bug, in mpeg2.gethdr.Get_Hdr there is operands
	    //       from different blocks in Ps1 or Ps2, thus the order of them
	    //       in the vector determines whether the cluse evaluates to true
	    //       or false. The same thing is in the Prolog version.

	    operand minPs1 = Ps1[0];
	    operand minPs2 = Ps2[0];

   	    // min(Ps1) must occupy an even register and min(Ps2) must
	    // occupy an odd one, or vice versa.
	    if((odd_operand[minPs1]  && even_operand[minPs2]) ||
	       (even_operand[minPs1] && odd_operand[minPs2])) {
	      Nogoods.push_back(Conj);
	    }
#if 0
// [MC] June 11, 2018
// This idea is probably not worth it, because operands like p3, p4 below
// can have zero live ranges, and so the implied constraints would have to
// carefully check that in OPERAND_OVERLAP_EXPR.
	    else if(!exist_before(*R, Ps1, Ps2)) {
	      operand p3 = min(minPs1, minPs2);
	      operand p4 = max(minPs1, minPs2);

	      UnisonConstraintExpr l(OPERAND_OVERLAP_EXPR, {p3,p4}, {});
	      presolver_conj C(Conj);

	      // Conj union l
	      vector_insert(C,l);
	      Nogoods.push_back(C);
	    }
#endif
	  }
	}
      }
    }
  }
}


vector<vector<nogood_cand_set> > InfeasiblePresolver::gen_candidates(const vector<temporand_set>& D, const vector<vector<Temporand> >& U) {

  // Make map from root (Temporand) in DisjointSet to nogood candidates.
  // Nogoods candidate are non mandatory assignments between
  // temporands. A candidate are a vector of two temporaries
  // and theire respective roots.
  map<Temporand, vector<vector<Temporand> > > M;
  for(const vector<Temporand>& u : U) {

    // if non-mandatory
    if(u.size() == 3 && u[2] == Temporand::n(1)) {
      Temporand v1 = u[0];
      Temporand v2 = u[1];
      Temporand w1 = ds_set.find(v1);
      Temporand w2 = ds_set.find(v2);
      if (w1!=w2) {
	vector<Temporand> cand = {v1,v2,w1,w2};
	vector_insert(M[w1], cand);
	vector_insert(M[w2], cand);
      }
    }
  }

  // Make vector<vector<nogood_cand_set> > index in the order of
  // the temporands in D, meaning that for each temporand_set d
  // in D there is a corresponding (at the same index as d)
  // vector of nogood_cand_set which contains possible
  // nogoods canddates for every element of d.
  //
  // D:        {{{T1, T2},...}, {...,{T5,..}},...}
  //
  //                         |
  //                         V
  //
  // DCands:   {{{Cs1, Cs2},...}, {...,{Cs5,..}},...}
  //
  //
  // For each temporand T in some d of D:
  //    Get candset Cs of T and place it in the position of T
  //    but in the vector DCand.
  //
  // Psuedo code:
  //
  // DCands <- {}
  // For every d in D,
  //   cands <- {}
  //   for every x in d,
  //      if there exist a mapping from root(x) in M,
  //          cand.add(M[root(x)])
  //      else
  //          cand.add({})
  //      end if
  //   end for
  //   DCand.add(cand)
  // end for
  vector<vector<nogood_cand_set> > DCands;
  for(const temporand_set& d : D) {
    vector<nogood_cand_set > cands;

    for(const Temporand& x : d) {
      const Temporand& y = ds_set.find(x);

      if(M.end() != M.find(y)) cands.push_back(M[y]);
      else cands.push_back({});
    }

    DCands.push_back(cands);
  }

  return DCands;
}


bool InfeasiblePresolver::subsumed_nogood(const presolver_conj& conj) {
  if(conj.size() > 6) return true;
  // Exists p,t,t' | {eq(p(p),t(t)),eq(p(p),t(t'))} subset of conj
  // Since conj is sorted, any such pair must be consecetive positioned in conj.
  for(unsigned i = 1; i < conj.size(); i++) {
    if(conj[i-1].id == CONNECTS_EXPR &&
       conj[i].id == CONNECTS_EXPR &&
       conj[i-1].data[0] == conj[i].data[0]) { // Equal p
      return true;
    }
  }

  // Exists a N in assert.new_nogoods | N subset of conj
  for(const presolver_conj& N : PA.new_nogood) {
    if(subsumes(N, conj)) {
      return true;
    }
  }

  return false;
}

void InfeasiblePresolver::cycle_dfs(const operation i,
				    const operation j,
				    vector<vector<operation> > F,
				    const presolver_conj& conj,
				    map<vector<operation>, vector<pair<int,vector<presolver_conj> > > >& M) {
  if(timer.stop() >= cutoff)
    return;
 start:
  if(subsumed_nogood(conj)) {
  } else if(i == j) {
    vector<presolver_conj> RC;
    for(const presolver_conj& c : PA.new_nogood) {
      if(subsumes(conj, c)) {
	vector_insert(RC, c);
      }
    }

    vector_insert(PA.new_nogood, conj);
    vector<presolver_conj> x = ord_difference(PA.new_nogood, RC);
    PA.new_nogood.swap(x);
  } else if(!F.empty()) {
    //    vector<operation> e = F[0]; // Lex. smallest edge of F
    vector<operation> e = F.back(); // Lex. smallest edge of F
    int ei = e[0];
    int ej = e[1];

    // EC <- F \ {e}
    F.pop_back();

    if(i == ei && ej <= j) {
      for(pair<int,vector<presolver_conj> >& v: M[e]) {
	for(const presolver_conj& conj2 : v.second) {
	  presolver_conj conj3 = ord_union(conj, conj2);

	  cycle_dfs(ej, j, F, conj3, M);
	}
      }
    }

    if(i >= ei) {
      goto start;
    }
  }
}


void InfeasiblePresolver::break_cycle(const vector<operation>& scc,
				      const vector<vector<operation> >& E,
				      map<vector<operation>, vector<pair<int,vector<presolver_conj> > > >& M) {

  vector<vector<operation> > F;
  vector<vector<operation> > B;

  // F <- {<i,j> in E where i < j && {i,j} subset of scc}
  // B <- {<i,j> in E where j < i && {j,i} subset of scc}
  for(const vector<operation>& e : E) {
    operation i = e[0];
    operation j = e[1];

    if(i < j) {
      vector<operation> s = {i,j};
      if(includes(scc.begin(),scc.end(),s.begin(), s.end())) {
	vector_insert(F, s);
      }
    }

    else if (j < i) {
      vector<operation> s = {j,i};
      if(includes(scc.begin(),scc.end(),s.begin(), s.end())) {
	vector_insert(B, {i,j});
      }
    }
  }

  reverse(F.begin(), F.end());
  for(const vector<operation>& e : B){
    if (options.verbose()) {
      cerr << "*  breaking cycle from " << e[1] << " to " << e[0] << " and back" << endl;
    }
    for(const pair<int,vector<presolver_conj> >& v: M[e]) {
      for(const presolver_conj& conj : v.second) {
	cycle_dfs(e[1], e[0], F, conj, M);
	if(timer.stop() >= cutoff)
	  return;
      }
    }
  }
}

void InfeasiblePresolver::detect_cycles(void) {
  map<vector<operation>, vector<pair<int,vector<presolver_conj> > > > M;
  presolver_conj T;
  vector<vector<operation> > E;

  cutoff = (options.timeout() + timer.stop()) / 2;

  for(const UnisonConstraintExpr& prec : input.precedences) {
    // T <- {<j,i,-n,d> | <i,j,n,d> in input.precedences && i < j && (d = {{}} || d = {{a(i)}})}

    UnisonConstraintExpr prec_c(AND_EXPR, {}, {});
    vector<int> data = prec.data;
    operation prec_i;
    operation prec_j;
    int prec_n;
    bool prec_d_true_or_active_i = true;
    bool implied = false;

    if (prec.id != DISTANCE_EXPR) {
      implied = true;
      prec_c = prec.children[0];
      data = prec.children[1].data;
      if (prec_c.id != ACTIVE_EXPR || prec_c.data[0] != data[0])
	prec_d_true_or_active_i = false;
    }
    prec_i = data[0];
    prec_j = data[1];
    prec_n = data[2];
    if (prec_i < prec_j && prec_d_true_or_active_i) {
      UnisonConstraintExpr p(DISTANCE_EXPR, {prec_j,prec_i,-prec_n}, {});
      if (implied)
	p = UnisonConstraintExpr(IMPLIES_EXPR, {}, {prec_c,p});
      vector_insert(T,p);
    }
  }

  // TC <- input.precedences \ T
  vector<UnisonConstraintExpr> precedences = input.precedences;
  sort(precedences.begin(), precedences.end());
  vector<UnisonConstraintExpr> TC = ord_difference(precedences, T);

  for(const UnisonConstraintExpr& prec : TC) {
    vector<int> data = prec.data;
    operation prec_i;
    operation prec_j;
    int prec_n;
    presolver_disj prec_d = {{}};

    if (prec.id != DISTANCE_EXPR) {
      prec_d = expr_to_disj(prec.children[0]);
      data = prec.children[1].data;
    }
    prec_i = data[0];
    prec_j = data[1];
    prec_n = data[2];
    vector_insert(M[{prec_i, prec_j}], make_pair(prec_n, prec_d));
    vector_insert(E, {prec_i, prec_j});
  }

  Digraph G(E);
  for(const vector<operation>& scc : G.scc()) {
    if(scc.size() >= 2) {
      if (options.verbose()) {
        cerr << "* cycle detected: " << show(scc) << endl;
      }
      break_cycle(scc, E, M);
      if(timer.stop() >= cutoff)
	return;
    }
  }
}


vector<vector<Temporand> >
InfeasiblePresolver::cartesian_product(const vector<temporand_set >& V) {
  vector<vector<Temporand> > R = {{}};

  for(const temporand_set& v : V) {
    vector<vector<Temporand> > I;
    for(vector<Temporand>& r : R) {
      for(const Temporand& val : v) {
	I.push_back(r);               // Add copy of r to I.
	I.back().push_back(val);      // Add val to end of copy of r.
      }
    }
    R.swap(I);
  }

  return R;
}

void InfeasiblePresolver::gen_odd_even_operands(vector<bool>& odds,
						vector<bool>& evens) {
  // Pre-allocate space
  odds = vector<bool>(input.P.size(), false);
  evens = vector<bool>(input.P.size(), false);

  for(const vector<operand>& C : input.strictly_congr) {
    // Yields -1, 0 or 1
    int i = is_odd_even(C);

    if(i == 1){
      for(operand p : C) {
	odds[p] = true;
      }
    }

    else if(i == 0) {
      for(operand p : C) {
	evens[p] = true;
      }
    }
  }
}


// Returns 1 if all operands of Ps must be in odd registers, 0 if must be
// in even and -1 in other cases.
int InfeasiblePresolver::is_odd_even(const vector<operand>& Ps) {
  int mod = -1;
  for(operand p : Ps) {
    int c_mod = -1;
    int w = input.width[first_temp_but_null(input, p)];
    if(w > 1) {
      c_mod = 0;
    } else {
      if(input.p_preassign[p]>=0) {
	c_mod = (input.p_preassign[p] & 1);
      } else {
	operation o = input.oper[p];

	int type = input.type[o];
	if(type == LOW) {
	  c_mod = 0;
	} else if(type == HIGH) {
	  c_mod = p-input.operands[o][0];
	} else if(type == COMBINE) {
	  c_mod = (p-input.operands[o][0]) & 1;
	}
      }
    }

    if(c_mod != -1) {
      if(mod == -1) {
	mod = c_mod;
      }
      else if (mod != c_mod){
	return -1; // Conflict
      }
    }
  }
  return mod;
}


bool InfeasiblePresolver::exist_before(const vector<vector<operand> >& R,
				       const vector<operand>& P1,
				       const vector<operand>& P2) {
  // There does not exist an p1 in P1, p2 in P2, G | f = [G]
  // and (p2 is rechable from p1 in G ore vice versa, or
  // p1 is a use and p2 is a def of the same
  // (low)/(high)/(combine)  or vice versa).

  for(const operand p1 : P1) {
    for(const operand p2 : P2) {

      operation op1 = input.oper[p1];
      int otype = input.type[op1];

      // p1  is a use and p2 is a def of the same (low)/(high)/(combine)
      if(op1 == input.oper[p2] &&
	 (otype == LOW || otype == HIGH || otype == COMBINE) &&
	 input.use[p1] != input.use[p2]) {

	return true;
      }

      // p2 is reachable from p1
      else if(ord_contains(R[p1], p2)) {
	return true;
      }

      // p1 is reachable from p2
      else if(ord_contains(R[p2], p1)) {
	return true;
      }
    }
  }

  return false;
}
