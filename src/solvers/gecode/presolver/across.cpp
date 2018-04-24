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
#include "./across.hpp"

// JSON.across
// JSON.set_across
// JSON.before2
// JSON.nogoods2
void presolve_across(PresolverAsserts& PA,
		     const Parameters& input,
		     vector<PresolverAcrossTuple>& Across1,
		     vector<PresolverSetAcrossTuple>& AltAcross,
		     vector<PresolverBefore>& CondBefore,
		     vector<presolver_conj>& Nogoods) {
  vector<PresolverAcrossTuple> Across;

  for(block b : input.B) {
    vector<operation> oset;
    for(operation o : input.ops[b]) {
      int ty = input.type[o];
      if (ty==FUN || ty==IN || ty==TAILCALL || ty==OUT)
	oset.push_back(o);
      if (ty==TAILCALL)
	break;
    }
    gen_across_call(PA, input, oset, b, CondBefore, Across);
    alt_across_call(PA, input, oset, b, AltAcross);
  }
  for(const PresolverAcrossTuple& acr : Across)
    nogoods_or_across(input, acr, Nogoods, Across1);
}

void alt_across_to_json(Parameters& input,
			const vector<PresolverSetAcrossTuple>& SetAcross) {
  map<temporary,vector<temporary>> copy_rel_temps;
  map<operation,vector<PresolverSetAcrossTuple>> M;

  generate_copyrel_temps_map(input, copy_rel_temps);
  for(const PresolverSetAcrossTuple& acr : SetAcross)
    M[acr.o].push_back(acr);

  for(const pair<operation,vector<PresolverSetAcrossTuple>>& OT : M) {
    vector<operand> Ps;
    vector<temporary> Ts;
    vector<register_atom> E;
    vector<vector<temporary>> copysets;

    collect_at_call(input, OT.first, Ps, Ts);
    across_extra_regs(input, Ps, E);
    for(const PresolverSetAcrossTuple& acr : OT.second) {
      vector<temporary> copyset;
      if(acr.u!=NULL_TEMPORARY)
	copyset = ord_union(copy_rel_temps[acr.t], copy_rel_temps[acr.u]);
      else
	copyset = copy_rel_temps[acr.t];
      copysets.push_back(copyset);
    }
    PresolverSetAcross sa;
    sa.o = OT.first;
    // sa.ras = E; // [MC] 19 April 2017
    sa.tsets = copysets;
    input.set_across.push_back(sa);
  }
}


void across_to_json(Parameters& input,
		    const vector<PresolverAcrossTuple>& Across,
		    const vector<presolver_conj>& Nogoods) {
  map<operation,vector<PresolverAcrossTuple>> M;

  for(const PresolverAcrossTuple& acr : Across)
    M[acr.o].push_back(acr);
  for(const pair<operation,vector<PresolverAcrossTuple>>& OT : M) {
    vector<operand> Ps;
    vector<temporary> Ts;
    vector<register_atom> E;
    vector<PresolverAcrossItemJSON> F;

    collect_at_call(input, OT.first, Ps, Ts);
    across_extra_regs(input, Ps, E);
    for(const PresolverAcrossTuple& acr : OT.second) {
      if(!ord_contains(Ts, acr.t)) {
	PresolverAcrossItemJSON ai;
	ai.t = acr.t;
	ai.e = disj_to_expr(filter_condition(acr.d, Nogoods));
	F.push_back(ai);
      }
    }
    PresolverAcrossJSON a;
    a.o = OT.first;
    a.ras = E;
    a.as = F;
    sort(a.as.begin(), a.as.end()); // canonicalize
    input.across.push_back(a);
  }
}


void collect_at_call(const Parameters& input, operation o, vector<operand>& P, vector<temporary>& T) {
  for(operand p : oper_uses(input, o)) {
    if(p_preassigned_callee_saved(input, p)) { // reserved: EXCLUDED
      P.push_back(p);
      for(temporary t : opnd_temps(input, p))
	vector_insert(T, t);
    }
  }
}


void across_extra_regs(const Parameters& input, const vector<operand>& P, vector<register_atom>& E) {
  for(operand p : P) {
    int w = temp_width(input, first_temp(input, p));
    for(int reg = input.p_preassign[p]; reg<input.p_preassign[p]+w; reg++)
      vector_insert(E, reg);
  }
}


void gen_across_call(PresolverAsserts& PA,
		     const Parameters& input,
		     const vector<operation>& oset,
		     block b,
		     vector<PresolverBefore>& CondBefore,
		     vector<PresolverAcrossTuple>& Across) {
  Digraph G = PA.dd_graph[b];
  for(unsigned int oi=0; oi+2<oset.size(); oi++) {
    operation o1 = oset[oi+1];
    map<temporary,presolver_disj> Before;
    map<temporary,presolver_disj> After;
    map<temporary,int> keys;

    collect_before_call(PA, input, o1, b, Before);
    collect_after_call(PA, input, o1, b, After);
    cond_before_items(PA, input, Before, After, o1, b, CondBefore);
    
    // rephrased algorithm
    for(const pair<temporary,presolver_disj>& bef : Before) {
      if(After.find(bef.first) != After.end()) {
	PresolverAcrossTuple at;
	at.o = o1;
	at.t = bef.first;
	at.d = merge_disjunctions(input, bef.second, After[bef.first]);
	Across.push_back(at);
	keys[bef.first] = 0;
      }
    }
    for(temporary t : across_candidates(PA, input, o1, b)) {
      if(keys.find(t)==keys.end()) {
	PresolverAcrossTuple at;
	at.o = o1;
	at.t = t;
	Across.push_back(at);
      }
    }
  }
}

void collect_before_call(PresolverAsserts& PA, const Parameters& input, operation o1, block b, map<temporary,presolver_disj>& Before) {
  Digraph G = PA.dd_graph[b];
  Digraph GT = PA.dd_graph_transpose[b];
  vector<pair<operation,presolver_conj>> predecessors = extend_predecessors(input,
									    GT.reachables(o1), // EXCLUDING o1
									    oper_uses(input, o1));
  vector<operation> ops1 = G.reachables(o1);
  vector_insert(ops1, o1);
  vector<operation> nonsuccessors = ord_difference(input.ops[b], ops1);
  for(const pair<operation,presolver_conj>& OC : predecessors) {
    for(operand p : oper_defs(input, OC.first)) {
      temporary tu = first_temp_but_null(input, p);
      Before[tu].push_back(OC.second);
      pair<bool,presolver_conj> BConj1 = cond_caller_saved(input, p, tu, OC.second);
      if(BConj1.first) {
	for(operation o1 : nonsuccessors) {
	  if(oper_type(input, o1)!=FUN) {
	    for(operand q : oper_uses(input, o1)) {
	      if(ord_contains(opnd_temps(input, q), tu)) {
		for(operand q1 : oper_defs(input, o1)) {
		  for(temporary td : opnd_temps(input, q1)) {
		    if(td!=NULL_TEMPORARY) {
		      UnisonConstraintExpr lit(CONNECTS_EXPR, {q,tu}, {});
		      presolver_conj Conj2 = {lit};
		      Conj2.insert(Conj2.end(), BConj1.second.begin(), BConj1.second.end());
		      Before[td].push_back(normal_conjunction(input, Conj2));
		    }
		  }
		}
	      }
	    }
	  }
	}
      }
    }
  }
}


void collect_after_call(PresolverAsserts& PA, const Parameters& input, operation o1, block b, map<temporary,presolver_disj>& After) {
  presolver_conj conj_true;

  Digraph G = PA.dd_graph[b];
  for(operation o : G.reachables(o1)) { // EXCLUDING o1
    for(operand p : oper_uses(input, o)) {
      for(temporary tu : opnd_temps(input, p)) {
	if(tu!=NULL_TEMPORARY) {
	  UnisonConstraintExpr lit(CONNECTS_EXPR, {p,tu}, {});
	  presolver_conj Conj = {lit};
	  After[tu].push_back(Conj);
	}
      }
      if(!p_preassigned_callee_saved(input, p)) { // preassigned: not or caller-saved OR RESERVED
	for(temporary tu : opnd_temps(input, p)) {
	  if(tu!=NULL_TEMPORARY) {
	    if(oper_type(input, temp_oper(input, tu))!=FUN) {
	      pair<bool,presolver_conj> BConj1 = cond_caller_saved(input, p, tu, conj_true);
	      if(BConj1.first) {
		for(operand q : oper_uses(input, temp_oper(input, tu))) {
		  for(temporary td : opnd_temps(input, q)) {
		    if(td!=NULL_TEMPORARY) {
		      UnisonConstraintExpr lit1(CONNECTS_EXPR, {q,td}, {});
		      UnisonConstraintExpr lit2(CONNECTS_EXPR, {p,tu}, {});
		      presolver_conj Conj2 = {lit1,lit2};
		      Conj2.insert(Conj2.end(), BConj1.second.begin(), BConj1.second.end());
		      After[td].push_back(normal_conjunction(input, Conj2));
		    }
		  }
		}
	      }
	    }
	  }
	}
      }
    }
  }
}

vector<pair<operation,presolver_conj>> extend_predecessors(const Parameters input,
							   const vector<operation>& O,
							   const vector<operand>& P) {
  vector<pair<operation,presolver_conj>> R;
  presolver_conj conj_true;
  for(operation o : O)
    R.push_back(make_pair(o,conj_true));
  for(operand p : P) {
    if(input.use[p]) {
      bool first = true;
      for(temporary t : opnd_temps(input, p)) {
	if (first) {
	  first = false;
	} else {
	  operation o = temp_oper(input, t);
	  UnisonConstraintExpr e(CONNECTS_EXPR, {p,t}, {});
	  presolver_conj Conj = {e};
	  R.push_back(make_pair(o,Conj));
	}
      }
    }
  }
  return R;
}

presolver_disj merge_disjunctions(const Parameters& input,
				  const presolver_disj& d1,
				  const presolver_disj& d2) {
  presolver_disj d12;
  for(const presolver_conj& c1 : d1) {
    for(const presolver_conj& c2 : d2) {
      presolver_conj c12;
      c12.insert(c12.end(), c1.begin(), c1.end());
      c12.insert(c12.end(), c2.begin(), c2.end());
      d12.push_back(normal_conjunction(input, c12));
    }
  }
  return kernel_set(d12, {}, -1);
}

void nogoods_or_across(const Parameters& input,
		       const PresolverAcrossTuple& acr,
		       vector<presolver_conj>& Nogoods,
		       vector<PresolverAcrossTuple>& Across) {
  presolver_disj Disj;
  temporary t = acr.t;
  bool cstemp = t_preassigned_caller_saved(input, t);
  for(const presolver_conj& c : acr.d) {
    presolver_conj c2;
    bool csopnd = false;
    for(const UnisonConstraintExpr& l : c) {
      if(l.id==CONNECTS_EXPR) {
	operand p = l.data[0];
	vector<temporary> ts = opnd_temps(input, p);
	if(ts.size()==1 || (ts.size()==2 && ts[0]==NULL_TEMPORARY))
	  continue;
	if(l.data[1]==t && p_preassigned_caller_saved(input, p))
	  csopnd = true;
      }
      c2.push_back(l);
    }
    if (cstemp || csopnd) {
      Nogoods.push_back(c2);
    } else {
      Disj.push_back(c2);
    }
  }
  PresolverAcrossTuple acr1;
  acr1.o = acr.o;
  acr1.t = acr.t;
  acr1.d = kernel_set(Disj, {}, -1);
  Across.push_back(acr1);
}

vector<temporary> across_candidates(PresolverAsserts& PA, const Parameters& input, operation o1, block b) {
  vector<temporary> C, U;
  Digraph G = PA.dd_graph[b];
  Digraph GT = PA.dd_graph_transpose[b];
  vector<operation> predecessors = GT.reachables(o1); // EXCLUDING o1
  vector<operation> successors = G.reachables(o1); // EXCLUDING o1
  for(operation o : input.ops[b]) {
    if(o!=o1 && !ord_contains(successors, o)) {
      for(operand p : oper_defs(input, o))
	if(!p_preassigned_caller_saved(input, p)) { // preassigned: not or callee-saved or reserved
	  for(temporary t : opnd_temps(input, p)) {
	    if(t!=NULL_TEMPORARY) {
	      vector_insert(C, t);
	    }
	  }
	}
    }
  }
  for(operation o : input.ops[b]) {
    if(is_mandatory(input, o) && !ord_contains(predecessors, o)) {
      for(operand p : oper_uses(input, o)) {
	if(o!=o1 || p_preassigned_callee_saved(input, p)) { // preassigned: callee-saved, but not reserved
	  for(temporary t : opnd_temps(input, p)) {
	    if(t!=NULL_TEMPORARY) {
	      vector_insert(U, t);
	    } else {
	      break;
	    }
	  }
	}
      }
    }
  }
  return ord_intersection(C, U);
}

pair<bool,presolver_conj> cond_caller_saved(const Parameters& input,
					    operand p,
					    temporary t,
					    const presolver_conj& c) {
  if (t_preassigned_caller_saved(input, t))
    return make_pair(true, c);
  else if (!t_preassigned_not(input, t))
    return make_pair(false, c);
  else if(p_preassigned_caller_saved(input, p))
    return make_pair(true, c);
  else if(!p_preassigned_not(input, p))
    return make_pair(false, c);
  for(const UnisonConstraintExpr& l : c) {
    if(l.id==CONNECTS_EXPR && l.data[1]==t) {
      operand q = l.data[0];
      if(p_preassigned_caller_saved(input, q))
	return make_pair(true, c);
      else if(!p_preassigned_not(input, q))
	return make_pair(false, c);
    }
  }
  UnisonConstraintExpr e(CALLER_SAVED_EXPR, {t}, {});
  presolver_conj c2 = {e};
  c2.insert(c2.end(), c.begin(), c.end());
  return make_pair(true, normal_conjunction(input, c2));
}

void cond_before_items(PresolverAsserts& PA,
		       const Parameters& input,
		       const map<temporary,presolver_disj>& Before,
		       const map<temporary,presolver_disj>& After,
		       operand o1,
		       block b,
		       vector<PresolverBefore>& CondBefore) {
  map<temporary,presolver_disj> t2before;
  map<temporary,presolver_disj> t2after;
  map<temporary,int> t2either;
  Digraph G = PA.dd_graph[b];
  Digraph GT = PA.dd_graph_transpose[b];
  vector<operation> predecessors = GT.reachables(o1); // EXCLUDING o1
  vector<operation> successors = G.reachables(o1); // EXCLUDING o1
  operand pcs = input.P.size();

  vector_insert(predecessors, o1); // need inclusive version
  vector_insert(successors, o1); // need inclusive version
  for(operation o : successors) {
    int ty = oper_type(input, o);
    if(ty==KILL || ty==OUT) {
      operand fuse = first_use(input, o);
      if (pcs > fuse) pcs = fuse;
    }
  }
  for(const pair<temporary,presolver_disj>& bef : cond_before_filter(input, Before, {})) {
    temporary t = bef.first;
    operand p = temp_def(input, t);
    if(p_preassigned_not(input, p)) { // not preassigned
      t2before[t] = kernel_set(bef.second, {}, -1);
      t2either[t] = 0;
    }
  }
  for(const pair<temporary,presolver_disj>& aft : cond_before_filter(input, After,  successors)) {
    temporary t = aft.first;
    operand p = temp_def(input, t);
    presolver_disj kern = kernel_set(aft.second, {}, -1);
    if(!p_preassigned_not(input, p)) { // preassigned
      goto nexta;
    }
    if (kern.size()==1) {
      for(const UnisonConstraintExpr& l : kern[0]) {
	if(l.id==CONNECTS_EXPR && l.data[1]==aft.first &&
	   !p_preassigned_not(input, l.data[0])) { // eq(p(p),t(t)) where p is preassigned
	  goto nexta;
	}
      }
    }
    t2after[t] = kern;
    t2either[t] = 0;
  nexta: ;
  }
  for(const pair<temporary,int>& key : t2either) {
    temporary t = key.first;
    operand p = temp_def(input, t);
    operation o = temp_oper(input, t);
    PresolverBefore Tb, Ta;

    if(t2before.find(key.first)!=t2before.end()) {
      if(ord_contains(successors, o)) {
	for(const presolver_conj& n : t2before[t])
	  PA.more_nogoods.push_back(n);
      } else {
	Tb.p = p;
	Tb.q = pcs;
	Tb.d = t2before[t];
	CondBefore.push_back(Tb);
      }
    }
    if(t2after.find(key.first)!=t2after.end()) {
      if(ord_contains(predecessors, o)) {
	for(const presolver_conj& n : t2after[t])
	  PA.more_nogoods.push_back(n);
      } else if(!ord_contains(successors, o)) {
	Ta.p = pcs;
	Ta.q = p;
	Ta.d = t2after[t];
	CondBefore.push_back(Ta);
      }
    }
  }
}

map<temporary,presolver_disj> cond_before_filter(const Parameters& input,
						 const map<temporary,presolver_disj>& Before,
						 const vector<operation>& successors) {
  map<temporary,presolver_disj> Filtered;
  for(const pair<temporary,presolver_disj>& kv : Before) {
    temporary t = kv.first;
    if(ord_contains(successors, temp_oper(input, t))) {
      Filtered[t] = {};
    } else {
      for(const presolver_conj& c : kv.second) {
	presolver_conj c2;
	if(t_preassigned_caller_saved(input, t)) {
	} else if(!t_preassigned_not(input, t)) {
	  goto nextc;
	} else {
	  UnisonConstraintExpr e(CALLER_SAVED_EXPR, {t}, {});
	  c2.push_back(e);
	}
	for(const UnisonConstraintExpr& l : c) {
	  if(l.id==CONNECTS_EXPR && l.data[1]==t && // eq(p(p),t(t)) where
	     p_preassigned_callee_saved(input, l.data[0])) { // p preassigned callee-saved, but not reserved
	    goto nextc;
	  }
	}
	c2.insert(c2.end(), c.begin(), c.end());
	Filtered[t].push_back(normal_conjunction(input, c2));
      nextc: ;
      }
    }
  }
  return Filtered;
}

void alt_across_call(PresolverAsserts& PA,
		     const Parameters &input,
		     const vector<operation>& oset,
		     block b,
		     vector<PresolverSetAcrossTuple>& AltAcross) {
  Digraph G = PA.dd_graph[b];
  Digraph GT = PA.dd_graph_transpose[b];
  for(unsigned int oi=0; oi+2<oset.size(); oi++) {
    operation o0 = oset[oi];
    operation o1 = oset[oi+1];
    operation o2 = oset[oi+2];
    vector<temporary> DefBefore;
    vector<temporary> UseAfter;
    vector<temporary> S;
    vector<operation> predecessors = GT.reachables(o1); // EXCLUDING o1
    vector<operation> successors = G.reachables(o1); // EXCLUDING o1
    for(operation o : predecessors) {
      for(operand p : oper_defs(input, o)) {
	vector_insert(DefBefore, first_temp(input, p));
      }
    }
    for(operation o : successors) {
      for(operand p : oper_uses(input, o)) {
	if(!ord_intersect(input.temps[p], PA.remat)) {
	  vector_insert(UseAfter, first_temp(input, p));
	}
      }
    }
    vector<temporary> Across = ord_intersection(DefBefore, UseAfter);
    for(temporary t : Across) {
      if(t!=NULL_TEMPORARY) {	// optional operands can occur
	PresolverSetAcrossTuple T;
	T.o = o1;
	T.t = t;
	T.u = NULL_TEMPORARY;
	AltAcross.push_back(T);
      }
    }
    vector<temporary> DefOnly = ord_difference(DefBefore, Across);
    vector<temporary> UseOnly = ord_difference(UseAfter, Across);
    for(operation o=o0+1; o<o2; o++) {
      if(o!=o1 && is_mandatory(input, o)) {
	for(operand pu : oper_uses(input, o)) {
	  temporary tu = first_temp(input, pu);
	  for(operand pd : oper_defs(input, o)) {
	    temporary td = first_temp(input, pd);
	    if(ord_contains(DefOnly, tu) &&
	       ord_contains(UseOnly, td) &&
	       !ord_contains(S, tu) &&
	       !ord_contains(S, td) &&
	       !ord_intersect(input.temps[pu], PA.remat)) {
	      S.push_back(tu);
	      S.push_back(td);
	      PresolverSetAcrossTuple T;
	      T.o = o1;
	      T.t = tu;
	      T.u = td;
	      AltAcross.push_back(T);
	    }
	  }
	}
      }
    }
  }
}
