/*
 *  Main authors:
 *    Erik Ekstrom <eeks@sics.se>
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
 *    Mats Carlsson <mats.carlsson@ri.se>
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
 * Generator of data to be used with before constraints. Part of C++ Presolver.
 */

#include "before_presolver.hpp"
#include "unsafe_temp.hpp"


BeforePresolver::BeforePresolver(Parameters& input) :
  input(input) { };

void BeforePresolver::presolve(vector<presolver_conj>& Nogoods) {

  // Prebuild congr congr_map
  generate_congruence_operands(input, congr_map);

  // Prebuild copyrelated operands maping
  generate_copyrel_operands_map(input, copy_rel_operands);

  beforeset b;
  gen_before(b);


  // TODO: should maybe not do it like this, instead
  //       have an invariant that everything is sorted during derivation.
  //  [MC] No, it's more convenient like this.
  sort(b.begin(),b.end());

  b.erase(unique(b.begin(), b.end()), b.end());

  before_vs_nogoods(b, Nogoods);
}

void BeforePresolver::gen_before(beforeset& B) {
   for(const vector<operand>& C : input.strictly_congr) {
    for(unsigned i = 0; i < C.size(); i++) {
      for(unsigned j = i+1; j < C.size(); j++) {
	// {i, j} -> All pairs in C
	operand p = C[i];
	operand q = C[j];

	// p are in and q are out of the same block and are not copy related
	if(input.pb[p] == input.pb[q] &&
	   input.type[input.oper[p]] == IN &&
	   input.type[input.oper[q]] == OUT &&
	   !ord_contains(copy_rel_operands[p],q)) {
	  // B <- B union Before1(p,q)
	  before1(p,q,B);
	}
      }
    }
  }

   // Map block --> vector<p, r, w>
   map<block, vector<vector<int> > > M;
   for(const vector<int>& pr : input.preassign) {
     if(pr.size() == 2) {
       int p = pr[0];
       int r = pr[1];

       // width of min(temps_but_null(p))
       int w = input.width[first_temp_but_null(input, p)];
       block b = input.pb[p];

       // map b -> <p,r,w> in M
       vector_insert(M[b], {p, r, w});
     }
   }

  for(auto const& it : M) {
    const vector<vector<int> >& C = it.second;
    const vector<vector<int> >& P = emit_before(C);
    Digraph G = Digraph(P);
    Digraph GR = G.reduction();

   // B <- B union {<p,q,{ø}> | <p,q> in R}
    for(const edge& e : GR.edges()) {
      PresolverBefore pb;
      pb.p = e.first;
      pb.q = e.second;
      B.push_back(pb);
    }
  }

  // B <- B union {<p,q,{ø}> | p in input.last_use && the temporary of
  //                      p can't be live past p, and q is def of the
  //                      same operation as p.}
  for(operand p : input.last_use) {
    if(input.use[p] && !input.temps[p].empty()) {
      operation o = input.oper[p];
      unsigned int pi = find_index(input.operands[o], p);
      bool negative_use_lat = false;
      for (unsigned int ii = 0; ii < input.instructions[o].size(); ii++) {
        if (input.lat[o][ii][pi] < 0) {
          negative_use_lat = true;
          break;
        }
      }
      if(!negative_use_lat) {
        for(operand q : oper_defs(input, o)) {
          // q is def of the same operation as p is def in.
          PresolverBefore pb;
          pb.p = p;
          pb.q = q;
          B.push_back(pb);
        }
      }
    }
  }

  // B <- B union {<p,q,{ø}> | p defines t, which is not unsafe and has its single use in the operation defining q.}
  for(operand p : input.P) {
    if(!input.use[p]) {
      vector<temporary> ts = input.temps[p];

      if(ts.size() == 1 && !temp_is_unsafe(input, ts[0])) {
	vector<operand> uses = input.users[ts[0]];
	if(uses.size() == 1) {
          for(operand q : oper_defs(input, input.oper[uses[0]])) {
	      PresolverBefore pb;
	      pb.p = p;
	      pb.q = q;
	      B.push_back(pb);
	  }
	}
      }
    }
  }

  // B <- B union {<p,q,{{eq(p(r),t(t)}}> | p,q are def, r is use,
  //                     t is a temp, and HighCombine(p,q,r,t)}
  //
  for (operation o : input.O) if (input.type[o] == COMBINE) {
      for (operand r : oper_uses(input, o)) {
        for (temporary t : input.temps[r]) {
          operation h = input.def_opr[t];
          if(input.type[h] == HIGH) {
            // p is the use operand of HIGH
            // FIXME: fix spec (GENBEFORE()) when it says "p, q are def"!
            operand p = first_use(input, h);
            // q is the first use operand of COMBINE (according to
            // high_combine_before).
            // FIXME: fix spec (GENBEFORE()) when it says "p, q are def"!
            operand q = first_use(input, o);
            PresolverBefore pb;
	    UnisonConstraintExpr e(CONNECTS_EXPR, {r,t}, {});
            pb.p = p;
            pb.q = q;
            pb.d.push_back({e});
            B.push_back(pb);
          }
        }
      }
    }
}



void BeforePresolver::before1(operand p, operand q, vector<PresolverBefore>& B) {

  // LH <- {OperOpnds(o) | o in Input.O and OperType(o) in {LOW, HIGH, SPLIT2, SPLIT4}}
  vector<vector<operand> > LH;
  for(const operation o : input.O) {
    int type = input.type[o];
    if(type == LOW || type == HIGH || type == SPLIT2 || type == SPLIT4) {
      vector_insert(LH, input.operands[o]);
    }
  }

  presolver_conj C;
  const vector<pair<operand, presolver_conj> >& P = lh_descendants(p, C, LH);
  const vector<pair<operand, presolver_conj> >& Q = lh_descendants(q, C, LH);

  for(const pair<operand, presolver_conj>& pc : P) {
    for(const pair<operand, presolver_conj>& qc : Q) {
      PresolverBefore pb;
      pb.p = pc.first;
      pb.q = qc.first;

      // Only add non-empty
      presolver_conj conj;
      for(const UnisonConstraintExpr& lit : pc.second)
	/* FIXME if(!lit.empty())*/ vector_insert(conj, lit);
      for(const UnisonConstraintExpr& lit : qc.second)
	/* FIXME if(!lit.empty())*/ vector_insert(conj, lit);

      if(!conj.empty()) {
	pb.d.push_back(conj);
      }

      B.push_back(pb);
    }
  }
}


vector<pair<operand, presolver_conj> >
BeforePresolver::lh_descendants(const operand p,
				const presolver_conj& C,
				const vector<vector<operand> >& LH) {
  vector<pair<operand, presolver_conj> > V;

  // V <- {<p,C>}
  vector_insert(V, make_pair(p, C));

  for(const vector<operand>& ud : LH) {
    if(ud.size() == 2) {
      operand u = ud[0];
      operand d = ud[1];

      const vector<temporary>& tp = input.temps[p];
      const vector<temporary>& tu = input.temps[u];

      presolver_conj _C = C;

      if(ord_intersect(tp,tu)) {
	if(tp != tu){
	  UnisonConstraintExpr _l(SHARE_EXPR, {u,p}, {});
	  _C.push_back(_l);
	}

	for(const pair<operand, presolver_conj>& lh : lh_descendants(d,_C,LH)) {
	  vector_insert(V, lh);
	}
      }
    }
  }
  return V;
}


vector<vector<operand> > BeforePresolver::emit_before(const vector<vector<int> >& C) {
  vector<vector<operand> > V;

  for(auto it1 = C.begin(); it1 != C.end(); it1++) {
    for(auto it2 = next(it1); it2 != C.end(); it2++) {
      const vector<int>& c1 = *it1;   // <p, r, w>
      const vector<int>& c2 = *it2;
      int p  = c1[0]; int r  = c1[1]; int w  = c1[2];
      int pp = c2[0]; int rr = c2[1]; int ww = c2[2];

      if(((r + w) > rr) &&
	 ((rr + ww) > r) &&
	 !input.use[pp]) {
	vector_insert(V, {p,pp});
      }

      else if(((r + w) > rr) &&
	      ((rr + ww) > r) &&
	      !ord_intersect(input.temps[p], input.temps[pp])) {
	vector_insert(V, {p,pp});
      }
    }
  }

  return V;
}


void BeforePresolver::before_vs_nogoods(beforeset& T, vector<presolver_conj>& Nogoods) {
  for(PresolverBefore& t : T) {
    // before <- {t | t = <p,q,{ø}> in T}
    if(t.d.empty()) {
      UnisonConstraintExpr e(AND_EXPR, {}, {});
      PresolverBeforeJSON u(t.p, t.q, e);
      input.before.push_back(u);
    }

    // nogoods <- {Conj union {overlap(p(p), p(q))} | <p,q,{conj}> in T
    //                                                      and conj != ø}
    else {
      UnisonConstraintExpr e(OPERAND_OVERLAP_EXPR, {t.p,t.q}, {});
      presolver_conj _conj = {e};

      for(const presolver_conj& c : t.d) {
	_conj.insert(_conj.end(), c.begin(), c.end());
      }
      Nogoods.push_back(normal_conjunction(input, _conj));
    }
  }
}


void BeforePresolver::presolve(Parameters& input, vector<presolver_conj>& Nogoods) {
  BeforePresolver bg(input);
  bg.presolve(Nogoods);
}


