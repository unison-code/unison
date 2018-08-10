/*
 *  Main authors:
 *    Mikael Almgren <mialmg@kth.se>
 *    Mats Carlsson <mats.carlsson@ri.se>
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
 *
 *  This file is part of Unison, see http://unison-code.github.io
 *
 *  Copyright (c) 2015-2016, Mikael Almgren
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

#include "dominance.hpp"

/*****************************************************************************
    Helper functions
*****************************************************************************/
// vector<int> sunion(vector<int> s1,
// 		   vector<int> s2) {
//   vector<int> temp(s1.size() + s2.size());
//   vector<int>::iterator it = set_union(s1.begin(), s1.end(),
// 				       s2.begin(), s2.end(),
// 				       temp.begin());
//   temp.resize(it - temp.begin());
//   return temp;
// }

bool compression_1(const vector<vector<int>>& S) {
  if(S[0].size() >= 2) {
    bool ex = false;

    for(const vector<int>& s1 : S) {
      ex = false;

      for(const vector<int>& s2 : S) {
	vector<int> s1_r(next(s1.begin()), s1.end());

	if(s1[0] != s2[0]) {
	  vector<int> s2_r(next(s2.begin()), s2.end());

	  if(s1_r == s2_r)
	    ex = true;
	}
      }
      if(!ex)
	return false;
    }
    return true;
  } else if(S[0].size() == 1) {
    // if the rows in S only has one column, return true if
    // there are one row with 1 and one with 0
    bool ex = false;
    for(const vector<int>& s1 : S) {
      ex = false;
      for(const vector<int>& s2 : S)
	if(s1 != s2)
	  ex = true;

      if(!ex)
	return false;
    }
    return true;
  }
  return false;
}

int compression_2(const vector<vector<int>>& S, const vector<int>& O) {
  int ind = -1;

  for(uint o = 0; o < O.size(); o++) {
    for(const vector<int>& s : S) {
      if(s[o] == 1)
	ind = o;

      else {
	ind = -1;
	break;
      }
    }
    if(ind >= 0)
      break;
  }

  return ind;
}

bool compression_3(const vector<vector<int>>& S) {
  bool b = true;
  for(uint i = 0; i < S.size()-1; i++) {
    if(S[i].back() != 0) {
      b = false;
      break;
    }
  }
  if(b) {
    for(int s : S.back()) {
      if(s != 1) {
	b = false;
	break;
      }
    }
  }
  return b;
}

void sort_increasing_T(map<vector<temporary>,vector<temporary>>& M,
                       vector<vector<temporary>>& ret_keys) {
  for (auto& kv : M)
    ret_keys.push_back(kv.first);

  sort(ret_keys.begin(), ret_keys.end(),
       [M](vector<temporary> a, vector<temporary> b) {
  	 int a_size = M.at(a).size();
	 int b_size = M.at(b).size();

	 if(a_size == b_size) {
	   return a < b;
	 }
	 return a_size < b_size;
      });
}

/*****************************************************************************
    Code related to:
    - JSON.domops
*****************************************************************************/
void temp_domination(Parameters & input) {
  map<tuple<vector<temporary>,vector<instruction>>,vector<temporary>> M;
  map<vector<temporary>,vector<operand>> N;

  for(operation o : input.O) {
    if(input.type[o] == COPY) {
      vector<instruction> instrs = input.instructions[o];
      auto key = make_tuple(opnd_temps_but_null(input, first_use(input, o)), instrs);
      temporary min_tmp = first_temp_but_null(input, first_def(input, o));

      vector_insert(M[key], min_tmp);
    }
  }

  for(operand p : input.P) {
    if(input.use[p]) {
      vector<temporary> T_temp = opnd_temps_but_null(input, p);
      vector<temporary> T = {T_temp.begin(), T_temp.end()};

      if(T.size() >= 3) {
      	for(auto it = M.begin(); it != M.end(); it++) {
	  tuple<vector<temporary>, vector<instruction>> k = it->first;
	  vector<temporary> T2 = get<0>(k);
	  vector<temporary> T1 = M[k];

	  if(*(T2.begin()) == *(T.begin()) && subseteq(T1, T) && T1.size() >= 2)
	    vector_insert(N[T1], p);
	}
      }
    }
  }

  // Generate the return set "domops"
  for (auto it = N.cbegin(); it != N.cend(); it++) {
    vector<int> T1 = it->first;
    vector<int> P = it->second;
    input.domops.push_back({P, T1});
  }
}

/*****************************************************************************
    Code related to:
    - JSON.dominates
*****************************************************************************/
void gen_dominates(Parameters & input) {
  map<temporary, vector<operation>> M;
  for(operation o : input.O) {
    if(input.type[o] == COPY && input.instructions[o][0] == NULL_INSTRUCTION) {
      temporary k = opnd_temps(input, first_use(input, o))[1];
      M[k].push_back(o);
    }
  }

  for(auto it = M.begin(); it != M.end(); it++) {
    for(operation o1 : it->second) {
      vector<instruction> I1 = oper_insns(input, o1);
      vector<temporary> D1 = opnd_temps(input, first_def(input, o1));
      vector<temporary> U1 = opnd_temps(input, first_use(input, o1));
      for(operation o2 : it->second) {
	if(o2 > o1) {
	  vector<instruction> I2 = oper_insns(input, o2);
	  vector<temporary> U2 = opnd_temps(input, first_use(input, o2));
	  PresolverDominates pd;
	  pd.o1 = o1;
	  pd.o2 = o2;
	  pd.ins = ord_difference(I2, I1);
	  pd.temps = ord_difference(ord_difference(U2, U1), D1);
	  input.dominates.push_back(pd);
	  if(subseteq(I1,I2) && subseteq(U2,U1))
	    break;
	}
      }
    }
  }
}

/*****************************************************************************
    Code related to:
    - JSON.active_tables
    - JSON.optional_min
    - JSON.tmp_tables
*****************************************************************************/
void gen_active_tables(Parameters & input, Support::Timer & timer,
                       PresolverOptions & options) {

  ModelOptions moptions;
  RelaxedModel * base = new RelaxedModel(&input, &moptions, IPL_DOM);
  Gecode::SpaceStatus ss = base->status();
  assert(ss != SS_FAILED);
  map<vector<temporary>, vector<temporary>> M;

  for(const vector<operand>& copyrel1 : input.copyrel) {
    vector<temporary> copies;
    for(const operand p : copyrel1) {
      if (!input.use[p] && (copies.size()==0 || !is_mandatory(input, opnd_oper(input, p)))) {
	const temporary tt = first_temp_but_null(input, p);
	copies.push_back(tt);
      }
    }
    if (copies.size() >= 2) {
      temporary t1 = copies[0];
      block b1 = input.oblock[input.def_opr[t1]];
      vector<temporary> tkey = {t1};
      vector<temporary> T1(next(copies.begin()), copies.end());
	for (temporary t : T1)
	  if (input.oblock[input.def_opr[t]] != b1)
	    goto next1;
      M[tkey] = T1;
    }
  next1: ;
  }
  for(const UnisonConstraintExpr& n : input.nogoods) {
    if(n.id == AND_EXPR && n.children[0].id == CONNECTS_EXPR && n.children[1].id == CONNECTS_EXPR) {

      operand p1 = n.children[0].data[0];
      operand p2 = n.children[1].data[0];

      temporary t1 = first_temp_but_null(input, p1);
      temporary t2 = first_temp_but_null(input, p2);
      block b1 = input.oblock[input.def_opr[t1]];
      block b2 = input.oblock[input.def_opr[t1]];

      if(t1 != t2 && b1==b2) {
	int opt1 = (input.temps[p1][0] == NULL_TEMPORARY);
	int opt2 = (input.temps[p2][0] == NULL_TEMPORARY);
      	vector<temporary> T1(next(input.temps[p1].begin(), opt1+1), input.temps[p1].end());
      	vector<temporary> T2(next(input.temps[p2].begin(), opt2+1), input.temps[p2].end());
	vector<temporary> tkey = {t1, t2};
	vector<temporary> T12 = ord_union(T1, T2);
	for (temporary t : T12)
	  if (input.oblock[input.def_opr[t]] != b1)
	    goto next12;
	M[tkey] = T12;
      }
    next12: ;
    }
  }

  vector<vector<temporary>> ret_keys;
  sort_increasing_T(M, ret_keys);

  // Time quantum for assert_active_tables: 50% of time left
  int tqa = (options.timeout() - timer.stop()) / 2;
  assert_active_tables(input, base, M, ret_keys, tqa);

  // Time quantum for assert_tmp_tables: 95% of time left
  int tqt = 19*(options.timeout() - timer.stop()) / 20;
  assert_tmp_tables(input, base, M, ret_keys, tqt);

  delete base;
}

void assert_active_tables(Parameters & input,
			  RelaxedModel * base,
			  map<vector<temporary>,vector<temporary>>& M,
			  vector<vector<temporary>>& ret_keys,
			  int timeout) {

  vector<PresolverActiveTable> active_tables;

  Support::Timer timer;
  for(const vector<temporary>& key : ret_keys) {
    vector<operation> O;
    vector<operand> P;

    if(timeout <= 0)
      break;

    for(temporary t : M[key]) {
      operation o = input.def_opr[t];
      O.push_back(o);
      for(operand p : input.operands[o])
	P.push_back(p);
    }

    timer.start();

    // Solve problem
    ActiveTableResult result = get_labelings(base, O, P, timeout);
    timeout -= (int) timer.stop();

    // Store result of no timeout
    if(result.timeout_status == RELAXED_NO_TIMEOUT) {
      // if no labeling is produced, fail and return
      if (result.labelings.empty()) {
        input.nogoods.clear();
        input.nogoods.push_back(UnisonConstraintExpr(OR_EXPR, {}, {}));
        return;
      } else {
        decompose_copy_set(input, O, result.labelings, active_tables);
      }
    } else if(result.timeout_status == RELAXED_TIMEOUT) {
      break;
    }
  }
  input.active_tables = active_tables;
}

void decompose_copy_set(Parameters & input,
			const vector<operation>& O,
			const vector<vector<int>>& S,
			vector<PresolverActiveTable>& active_tables) {

  if(!(S.size() == 1 && S[0].empty()) && !O.empty()) {
    int ind = -1;	// index for stuck-at-1

    // The first variable is irrelevant
    // compression 1
    if(compression_1(S)) {
      vector<vector<int>> S1;
      vector<int> O1(next(O.begin()), O.end());

      for(const vector<int>& s : S) {
	if(s[0] == 0) {
	  vector<int> row(next(s.begin()), s.end());
	  S1.push_back(row);
	}
      }
      decompose_copy_set(input, O1, S1, active_tables);
    }
    else if((ind = compression_2(S, O)) >= 0) {
    // i:th variable stuck-at 1
    // compression 2
      PresolverActiveTable tmp{{O[ind]}, {{1}}};
      active_tables.push_back(tmp);
      vector<int> O1;

      for(int o : O)
	if(o != O[ind])
	  O1.push_back(o);

      vector<vector<int>> S1;
      for(const vector<int>& vs : S) {
	vector<int> temp_s;
	for(int i = 0; i < (int) vs.size(); i++)
	  if(i != ind)
	    temp_s.push_back(vs[i]);

	S1.push_back(temp_s);
      }
      decompose_copy_set(input, O1, S1, active_tables);
    } else if(O.size() >= 3 && compression_3(S)) {
    // The last variable implies the other variables
    // compression 3
      for(uint o = 0; o < O.size()-1; o++){
	PresolverActiveTable tmp{{O[o], O[O.size()-1]},
	                        {{0,0}, {1,0}, {1,1}}};
	active_tables.push_back(tmp);
      }
      vector<int> O1(O.begin(), O.end()-1);
      vector<vector<int>> S1;

      for(const vector<int>& vs : S) {
	if(vs != *(S.end()-1)) {
	  vector<int> temp_s(vs.begin(), vs.end()-1);
	  S1.push_back(temp_s);
	}
      }

      decompose_copy_set(input, O1, S1, active_tables);
    } else if(O.size() >= 1) {
      PresolverActiveTable tmp{O, S};
      active_tables.push_back(tmp);
    }
  }
}

void assert_tmp_tables(Parameters & input,
		       RelaxedModel * base,
		       map<vector<temporary>,vector<temporary>>& M,
		       vector<vector<temporary>>& ret_keys,
		       int timeout) {

  map<temporary, vector<operand>> P2U;
  vector<PresolverCopyTmpTable> tmp_tables;

  for(operand p : input.P) {
    if(input.use[p]) {
      if(!(input.temps[p].size() == 2 && input.temps[p][0] == -1)) {
	temporary tmin = first_temp_but_null(input, p);
	P2U[tmin].push_back(p);
      }
    }
  }

  Support::Timer timer;
  for(const vector<temporary>& key : ret_keys) {
    if(key.size() == 1) {
      if(timeout <= 0)
	break;

      temporary k = key[0];
      vector<operation> O;
      vector<operand> P = P2U[k];

      for(temporary t : M[key])
	O.push_back(input.def_opr[t]);

      timer.start();

      // Solve problem
      TmpTableResult result = get_labelings(input, base, O, P, timeout);
      timeout -= (int) timer.stop();

      if(result.timeout_status == RELAXED_NO_TIMEOUT) {
        // if no labeling is produced, fail and return
        if (result.labelings.empty()) {
	  input.nogoods.clear();
	  input.nogoods.push_back(UnisonConstraintExpr(OR_EXPR, {}, {}));
          return;
        } else {
          PresolverCopyTmpTable
            tmp_table{O, P, trim_tmp_tables(result.labelings, k)};
          tmp_tables.push_back(tmp_table);
        }
      } else if(result.timeout_status == RELAXED_TIMEOUT) {
	break;
      }
    }
  }
  input.tmp_tables = tmp_tables;
}

// remove subsumed elements of nogoods, domops, domuses, difftemps
void tidy(Parameters & input) {
  vector<UnisonConstraintExpr> nogoods_ref = input.nogoods;
  vector<UnisonConstraintExpr> nogoods2_ref = input.nogoods2;
  vector<PresolverBeforeJSON> before2_ref = input.before2;
  vector<UnisonConstraintExpr> precedences_ref = input.precedences;
  vector<UnisonConstraintExpr> precedences2_ref = input.precedences2;
  vector<PresolverAcrossJSON> across_ref = input.across;
  vector<vector<operand> > difftemps_ref = input.difftemps;
  vector<vector<vector<int> > > domops_ref = input.domops;
  input.nogoods.clear();
  input.nogoods2.clear();
  input.before2.clear();
  input.precedences.clear();
  input.precedences2.clear();
  input.across.clear();
  input.difftemps.clear();
  input.domops.clear();
  
  map<operand, int> P2Table;
  int id = 0;
  for(const PresolverCopyTmpTable& tt : input.tmp_tables) {
    for(operand p : tt.ps)
      P2Table[p] = id;
    id++;
  }

  for(const UnisonConstraintExpr& n : nogoods_ref) {
    vector<operand> ps;
    expr_operands(n, ps);
    sort(ps.begin(), ps.end());
    ps.erase(unique(ps.begin(), ps.end()), ps.end());
    if(already_tabled(ps, P2Table)) {
    } else {
      UnisonConstraintExpr n2 = simplify_expr(input, n, P2Table);
      if (n2 != UnisonConstraintExpr(OR_EXPR, {}, {})) {
	input.nogoods.push_back(n2);
      }
    }
  }
  for(const UnisonConstraintExpr& n : nogoods2_ref) {
    vector<operand> ps;
    expr_operands(n, ps);
    sort(ps.begin(), ps.end());
    ps.erase(unique(ps.begin(), ps.end()), ps.end());
    if(!already_tabled(ps, P2Table)) {
      UnisonConstraintExpr n2 = simplify_expr(input, n, P2Table);
      if (n2 != UnisonConstraintExpr(OR_EXPR, {}, {})) {
	input.nogoods.push_back(n2);
      }
    }
  }
  for(const PresolverBeforeJSON& bf : before2_ref) {
    UnisonConstraintExpr e2 = simplify_expr(input, bf.e, P2Table);
    if (e2 != UnisonConstraintExpr(OR_EXPR, {}, {})) {
      PresolverBeforeJSON bf2 = PresolverBeforeJSON(bf.p, bf.q, e2);
      input.before2.push_back(bf2);
    }
  }
  for(const UnisonConstraintExpr& pr : precedences_ref) {
    UnisonConstraintExpr pr2 = simplify_expr(input, pr, P2Table);
    if (pr2 != UnisonConstraintExpr(AND_EXPR, {}, {})) {
      input.precedences.push_back(pr2);
    }
  }
  for(const UnisonConstraintExpr& pr : precedences2_ref) {
    UnisonConstraintExpr pr2 = simplify_expr(input, pr, P2Table);
    if (pr2 != UnisonConstraintExpr(AND_EXPR, {}, {})) {
      input.precedences2.push_back(pr2);
    }
  }
  for(const PresolverAcrossJSON& pr : across_ref) {
    PresolverAcrossJSON pr2;
    pr2.o = pr.o;
    pr2.ras = pr.ras;
    for (PresolverAcrossItemJSON ai : pr.as) {
      UnisonConstraintExpr e2 = simplify_expr(input, ai.e, P2Table);
      if (e2 != UnisonConstraintExpr(OR_EXPR, {}, {}))
	pr2.as.push_back(PresolverAcrossItemJSON(ai.t,e2));
    }
    if (!pr2.as.empty())
      input.across.push_back(pr2);
  }
  for(const vector<operand>& d : difftemps_ref) {
    if(!already_tabled(d, P2Table))
      input.difftemps.push_back(d);
  }
  for(const vector<vector<int>>& d : domops_ref) {
    if(!already_tabled(d[0], P2Table))
      input.domops.push_back(d);
  }
  
}
  
vector<vector<int>> trim_tmp_tables(at_map S, temporary k) {

  vector<vector<int>> S1;
  for(auto it = S.begin(); it != S.end(); it++)
    for(vector<int> T : trim_clump(it->second, k))
      vector_insert(S1, {concat(it->first, T)});

  return S1;
}

vector<vector<int>> trim_clump(set<vector<int>> C, temporary k) {
  set<vector<int>> C_ret = C;

  for(auto it1 = C.begin(); it1 != C.end(); it1++)
    for(auto it2 = next(it1); it2 != C.end(); it2++)
      if(tmp_subsumes(*it1, *it2, k))
	C_ret.erase(*it2);

  return to_vector(C_ret);
}

bool tmp_subsumes(vector<int> T1, vector<int> T2, temporary k) {

  vector<int> T1_sorted = T1;
  vector<int> T2_sorted = T2;

  sort(T1_sorted.begin(), T1_sorted.end());  // If sorted are equal ->
  sort(T2_sorted.begin(), T2_sorted.end());  // permutation

  for(uint o = 0; o < T1.size(); o++)
    if(!((T1.at(o) == k) == (T2.at(o) == k)))
      return false;

  return T1_sorted == T2_sorted;
}

int optional_min_active_tables(Parameters& input, block b) {
  vector<operation> O = input.ops[b];
  int count = 0;

  for(const PresolverActiveTable& SR : input.active_tables) {
    if(subseteq(SR.os, O)) {
      O = ord_difference(O, SR.os);

      vector<int> sum_vector;

      for(const vector<int>& R : SR.tuples) {
	int temp_sum = 0;

	for(int r : R)
	  temp_sum += r;

	sum_vector.push_back(temp_sum);
      }
      count += min_of(sum_vector);
    }
  }
  return count;
}


void filter_active_tables(Parameters & input) {
  vector<PresolverActiveTable> filtered_active_tables;
  vector<operation> forced;
  vector<vector<operation>> pairs;
  vector<vector<int>> vi =  { {0, 0}, {1, 0}, {1, 1} };
  map<vector<operation>,vector<operation>> M;

  for(operation o : input.O) {
    const vector<operation>& act = input.activators[o];
    if (!act.empty())
      M[act].push_back(o);
  }
  for(const pair<vector<operation>,vector<operation>>& act_os : M) {
    for(operation o : input.O) {
      if(input.instructions[o][0]>0 && subseteq(input.instructions[o], act_os.first)) {
	for(block b : input.B) {
	  vector<operation> OSb = ord_intersection(act_os.second, input.ops[b]);
	  if(!OSb.empty()) {
	    vector<int> ones;
	    init_vector(ones, OSb.size(), 1);
	    filtered_active_tables.push_back({OSb,{ones}});
	  }
	}
	break;
      }
    }
  }

  for(const PresolverActiveTable& pa : input.active_tables) {
    if(pa.os.size() == 1) {
      filtered_active_tables.push_back(pa);
      vector_insert(forced, pa.os[0]);
    } else if(pa.tuples != vi) {
      filtered_active_tables.push_back(pa);
    } else {
      vector_insert(pairs, pa.os);
    }
  }

  vector<PresolverDominates> filtered_dominates;

  for(const PresolverDominates& pd : input.dominates) { // can be unsorted
    vector<operation> o12 = {pd.o1,pd.o2};
    if(!ord_contains(forced, pd.o1) && !ord_contains(pairs, o12))
      vector_insert(filtered_dominates, pd);
  }

  Digraph G = Digraph(pairs);
  Digraph GR = G.reduction();
  for(const edge& e : GR.edges()) {
    PresolverDominates pd;
    pd.o1 = e.first;
    pd.o2 = e.second;
    vector_insert(filtered_dominates, pd);
  }

  sort(filtered_active_tables.begin(), filtered_active_tables.end());
  filtered_active_tables.erase(unique(filtered_active_tables.begin(), filtered_active_tables.end()), filtered_active_tables.end());
  input.active_tables = filtered_active_tables;
  input.dominates = filtered_dominates;
}

void expr_operands(const UnisonConstraintExpr& e, vector<operand>& ps) {
  switch (e.id) {
  case OR_EXPR:
  case AND_EXPR:
  case XOR_EXPR:
  case IMPLIES_EXPR:
  case NOT_EXPR:
    for (UnisonConstraintExpr e0 : e.children)
      expr_operands(e0, ps);
    break;
  case CONNECTS_EXPR:
    ps.push_back(e.data[0]);
    break;
  case SHARE_EXPR:
    ps.push_back(e.data[0]);
    ps.push_back(e.data[1]);
    break;
  default:
    ps.push_back(-1);
    break;
  }
}
  
bool already_tabled(const vector<operand>& ps, map<operand, int>& P2Table) {
  int common_id = -2;
  for(operand p : ps) {
    if(P2Table.count(p)==0) {
      return false;
    } else {
      int id = P2Table[p];
      if (common_id > -2 && common_id != id)
	return false;
      common_id = id;
    }
  }
  return true;
}

UnisonConstraintExpr simplify_expr(const Parameters& input,
				   const UnisonConstraintExpr& n,
				   map<operand, int>& P2Table) {
  UnisonConstraintExpr FalseExpr = UnisonConstraintExpr(OR_EXPR, {}, {});
  map<int, vector<UnisonConstraintExpr>> ID2Lits;
  if (n.id == IMPLIES_EXPR) {
    UnisonConstraintExpr ifpart = simplify_expr(input, n.children[0], P2Table);
    UnisonConstraintExpr thenpart = n.children[1];
    if (ifpart == FalseExpr)
      return UnisonConstraintExpr(AND_EXPR, {}, {});
    else
      return UnisonConstraintExpr(IMPLIES_EXPR, {}, {ifpart,thenpart});
  } else if (n.id == OR_EXPR) {
    vector<UnisonConstraintExpr> disjuncts;
    for (const UnisonConstraintExpr& c : n.children) {
      UnisonConstraintExpr c2 = simplify_expr(input, c, P2Table);
      if (c2 != FalseExpr)
	disjuncts.push_back(c2);
    }
    if (disjuncts.empty())
      return FalseExpr;
    else if (disjuncts.size()==1)
      return disjuncts[0];
    else
      return UnisonConstraintExpr(OR_EXPR, {}, disjuncts);
  } else if (n.id == AND_EXPR) {
    if (noop_copy_conjunction(input, n.children)) {
      return FalseExpr;
    }
    for (const UnisonConstraintExpr& c : n.children)
      if (c.id == CONNECTS_EXPR) {
	if (noop_copy_literal(input, c)) {
	  return FalseExpr;
	}
	operand p = c.data[0];
	if(P2Table.count(p)>0)
	  ID2Lits[P2Table[p]].push_back(c);
      }
  } else if (n.id == CONNECTS_EXPR) {
    if (noop_copy_literal(input, n)) {
      return FalseExpr;
    }
    operand p = n.data[0];
    if(P2Table.count(p)>0)
      ID2Lits[P2Table[p]].push_back(n);
  } else {
    return n;
  }
  for (pair<int, vector<UnisonConstraintExpr>> IdLits : ID2Lits) {
    PresolverCopyTmpTable tt = input.tmp_tables[IdLits.first];
    vector<UnisonConstraintExpr> lits = IdLits.second;
    vector<int> key;
    sort(lits.begin(), lits.end());
    auto litp = lits.begin();
    for(operand p : tt.ps) {
      if (litp != lits.end() && p == (*litp).data[0])
	key.push_back((*litp++).data[1]);
      else
	key.push_back(-1);
    }
    for(const vector<int>& tu : tt.tuples) {
      int i = tt.os.size();
      for(int k : key)
	if (k != -1 && k != tu[i])
	  goto next_t;
        else
	  i++;
      goto next_g;
    next_t: ;
    }
    return FalseExpr;
  next_g: ;
  }
  return n;
}

bool noop_copy_literal(const Parameters& input, const UnisonConstraintExpr& c) {
  operand p1 = c.data[0];
  temporary t1 = c.data[1];
  int r1 = input.p_preassign[p1];
  operation o0 = input.def_opr[t1];
  if (input.use[p1] && r1>=0 && input.type[o0]==COPY) {
    operand p0 = first_use(input, o0);
    if (input.temps[p0][0]==NULL_TEMPORARY && input.temps[p0].size()==2) {
      temporary t0 = input.temps[p0][1];
      int r0 = input.t_preassign[t0];
      if (r0==r1)
	return true;
    }
  }
  return false;
}

bool noop_copy_conjunction(const Parameters& input, const presolver_conj& c) {
  if (c.size()==2 && c[0].id==CONNECTS_EXPR && c[1].id==CONNECTS_EXPR) {
    operand p0 = c[0].data[0];
    temporary t0 = c[0].data[1];
    operation o0 = opnd_oper(input, p0);
    int pr0 = input.p_preassign[p0];
    int tr0 = input.t_preassign[t0];
    operand p1 = c[1].data[0];
    temporary t1 = c[1].data[1];
    int pr1 = input.p_preassign[p1];
    if (pr0!=pr1 && pr0>=0 && pr1>=0 && t0==t1)
      return true; // DOES OCCUR
    if (input.use[p0] && input.type[o0]==COPY && tr0==pr1 && tr0>=0 &&
	first_temp_but_null(input, first_def(input, o0))==t1)
      return true;
  } else if (c.size()==3 && c[0].id==CONNECTS_EXPR && c[1].id==CONNECTS_EXPR && c[2].id==CONNECTS_EXPR) {
    operand p0 = c[0].data[0];
    temporary t0 = c[0].data[1];
    int r0 = input.p_preassign[p0];
    operation o0 = opnd_oper(input, p0);
    operand p1 = c[1].data[0];
    temporary t1 = c[1].data[1];
    int r1 = input.p_preassign[p1];
    operation o1 = opnd_oper(input, p1);
    operand p2 = c[2].data[0];
    temporary t2 = c[2].data[1];
    int r2 = input.p_preassign[p2];
    if (input.use[p0] && input.type[o0]==COPY && t0==t1 && r1==r2 && r2>=0 &&
	first_temp_but_null(input, first_def(input, o0))==t2)
      return true;
    if (input.use[p1] && input.type[o1]==COPY && t0==t1 && r0==r2 && r2>=0 &&
	first_temp_but_null(input, first_def(input, o1))==t2)
      return true;
  }
  return false;
}
