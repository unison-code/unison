/*
 *  Main authors:
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
 *    Erik Ekstrom <eeks@sics.se>
 *    Mats Carlsson <mats.carlsson@ri.se>
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
 * Auxiliary procedures for presolving.
 */

#include "auxiliary.hpp"


void generate_copyrel_temps_map(const Parameters& input, map<temporary, vector<temporary> >& m) {
  m.clear();

  // Build copy-related-temps map for all non null temp sets.
  for(const vector<temporary>& ts : input.temps) {
    if(!ts.empty() && ts[0] != NULL_TEMPORARY) {
      for(temporary t : ts) {
	vector_insert(m[ts[0]], t);
      }
    }
  }

  // Extend mapping so that each temporary is a key and maps to the
  // ts it belongs to.
  for(auto& kv : m) {
    for(temporary i : kv.second) {
      for(temporary y : kv.second) {
	vector_insert(m[i], y);
      }
    }
  }
}


void generate_congruence_operands(const Parameters& input, vector<vector<operand> >& congr_map) {
  // Pre-allocate space
  congr_map = vector<vector<operand> >(input.P.size());

  // Map operand p to set of congruent operands of p.
  for(const vector<operand>& congr : input.strictly_congr) {
    for(operand p : congr) {
      congr_map[p] = congr;
    }
  }
}


void generate_copyrel_operands_map(const Parameters& input, vector<vector<operand> >& copyrel_operands) {
  // Pre-allocate space
  copyrel_operands = vector<vector<operand> >(input.P.size());

  // For each operand, make a vector in copyrel_operands
  // containing the operands copy related operands.
  for(const vector<operand>& copy_rels : input.copyrel) {
    for(operand p : copy_rels) {
      copyrel_operands[p] = copy_rels;
    }
  }
}


vector<operand> strip(const temporand_set& C){
  vector<operand> R;

  for(const Temporand& p : C){
    if(p.is_operand()) R.push_back(p.id());
  }
  return R;
}


bool subsumes(const presolver_conj& a, const presolver_conj& b) {
  return includes(b.begin(), b.end(), a.begin(), a.end());
}


vector<presolver_conj> kernel_set(const vector<presolver_conj>& disj,
			  const vector<presolver_conj>& sos,
			  int cutoff) {
  vector<presolver_conj> d(disj.begin(), disj.end());
  vector<presolver_conj> s(sos.begin(), sos.end());
  Support::Timer t;
  t.start();

  while (!d.empty()) {
    if(cutoff >= 0 && t.stop() >= cutoff)
      return ord_union(d,s);

    presolver_conj conj = d.back();
    bool is_subsumed = false;
    vector<presolver_conj> RC;
    
    d.pop_back();
    for(const presolver_conj& c : s) {
      if(subsumes(c, conj)) {
  	is_subsumed = true;
  	break;
      } else if(subsumes(conj, c)) {
	RC.push_back(c);
      }
    }

    if(!is_subsumed) {
      vector_insert(s, conj);
      if (!RC.empty()) {
	vector<presolver_conj> x = ord_difference(s, RC);
	s.swap(x);
      }
    }
  }

  return s;
}

presolver_disj filter_condition(const presolver_disj& disj,
       const vector<presolver_conj>& nogoods) {
    presolver_disj result;
    std::copy_if(disj.begin(), disj.end(),
            std::back_inserter(result),
            [&nogoods](const presolver_conj& conj) {
                return none_of(nogoods.begin(), nogoods.end(),
                    [&conj](const presolver_conj& N) {
                        return subseteq(N, conj);
                    });
            });
    return result;
}

operand first_use(const Parameters& input, operation o) {
  operand p = -1;
  for (operand p0 : input.operands[o])
    if (input.use[p0]) {
      p = p0;
      break;
    }
  return p;
}

operand first_def(const Parameters& input, operation o) {
  operand p = -1;
  for (operand p0 : input.operands[o])
    if (!input.use[p0]) {
      p = p0;
      break;
    }
  return p;
}

vector<operand> oper_uses(const Parameters& input, operation o) {
  vector<operand> us;
  for (operand p : input.operands[o])
    if (input.use[p]) us.push_back(p);
  return us;
}

vector<operand> oper_defs(const Parameters& input, operation o) {
  vector<operand> ds;
  for (operand p : input.operands[o])
    if (!input.use[p]) ds.push_back(p);
  return ds;
}

int oper_type(const Parameters& input, operation o) {
    return input.type[o];
}

vector<operand> oper_opnds(const Parameters& input, operation o) {
    return input.operands[o];
}

vector<instruction> oper_insns(const Parameters& input, operation o) {
    return input.instructions[o];
}

operation opnd_oper(const Parameters& input, operand p) {
    return input.oper[p];
}

vector<temporary> opnd_temps(const Parameters& input, operand p) {
    return input.temps[p];
}

// TO BE WEEDED OUT, BECAUSE IT CONSES
vector<temporary> opnd_temps_but_null(const Parameters& input, operand p) {
  if(input.temps[p][0] == NULL_TEMPORARY) {
    vector<int> v_ret(next(input.temps[p].begin()), input.temps[p].end());
    return v_ret;
  } else {
    return input.temps[p];
  }
}

temporary first_temp(const Parameters& input, operand p) {
  return input.temps[p][0];
}

temporary first_temp_but_null(const Parameters& input, operand p) {
  temporary t0 = input.temps[p][0];
  if (t0==NULL_TEMPORARY)
    t0 = input.temps[p][1];
  return t0;
}

operation temp_oper(const Parameters& input, temporary t) {
    return input.def_opr[t];
}

operand temp_def(const Parameters& input, temporary t) {
    return input.definer[t];
}

vector<operand> temp_uses(const Parameters& input, temporary t) {
  return input.users[t];
}

int temp_width(const Parameters& input, temporary t) {
    return input.width[t];
}

Digraph dd_graph(const Parameters& input, block b) {
    std::vector<edge> E;
    std::vector<edge> fun_kill_edges;
    // E <- { e in JSON.dep[b] |
    // e = <o, o'> and o < o' and both o and o' are mandatory}
    for(const vector<int>& e : input.dep[b]) {
        // Urgh, why not using pairs.
        int o = e[0];
        int o1= e[1];
        if(o < o1 && is_mandatory(input, o) && is_mandatory(input, o1)) {
	  E.push_back(std::make_pair(o, o1));
        }
    }

    for(operation o : input.ops[b]) {
        for(operand p : oper_opnds(input, o)) {
            if(input.use[p] == 1) {
                // d <- TempOper(min(OpndTempsButNull(p)))
                temporary min_temp = first_temp_but_null(input, p);
                operation d = temp_oper(input, min_temp);
                // E U {<d, o>}
		edge e = std::make_pair(d, o);
                E.push_back(e);
		if(oper_type(input, d) == FUN && oper_type(input, o) == KILL)
		  vector_insert(fun_kill_edges, e);
            }
        }
    }

    // E' <- empty set
    std::vector<edge> E1;
    // For all <f,k> in E
    // where OperType(f) == (function) and OperType(k) == (kill)
    for(const edge& e : fun_kill_edges) {
        vertex f = e.first;
        vertex k = e.second;

	// E' <- E' U { <k,s> | <f,s> in E where k != s}
	for(const edge& e : E) {
	  if(e.first == f && e.second != k) {
	    E1.push_back(std::make_pair(k, e.second));
	  }
	}
    }

    // Return MakeDigraph(E U E'), which takes care of sorting and duplicated entries
    E.insert(E.end(), E1.begin(), E1.end());
    return Digraph(E);
}

block block_containing(const Parameters& input, const vector<operand>& P) {
    block b = -1; // No block
    // For each block
    for(block block : input.B) {
        // For each operation in the block
        for(operation o : input.ops[block]) {
            bool is_subset = std::any_of(P.begin(), P.end(),
                    [&input, o](operand p) {
                        return ord_contains(input.operands[o], p);
                    });
            if(is_subset) {
                return block;
            }
        }
    }
    return b;
}

bool is_preassigned_not(const Parameters& input, operand p) {
  int r = input.p_preassign[p];
  return r<0;
}

bool is_preassigned_caller_saved(const Parameters& input, operand p) {
  int r = input.p_preassign[p];
  return (r>=0 && !input.r_calleesaved[r]);
}

bool is_preassigned_callee_saved(const Parameters& input, operand p) {
  int r = input.p_preassign[p];
  return (r>=0 && input.r_calleesaved[r]);
}

bool is_mandatory(const Parameters& input, operation o) {
    return input.instructions[o][0] != NULL_INSTRUCTION;
#if 0
    return std::any_of(
            input.mandatory.begin(),
            input.mandatory.end(),
            [o](vector<operation> block_mandatory_ops) {
                return ord_contains(block_mandatory_ops, o);
            });
#endif
}

void p_finite_register_classes(const Parameters& input, operand p, set<register_class>& RC) {
  operation o = input.oper[p];
  unsigned int nbinsn = input.instructions[o].size();
  int prel = p - input.operands[o][0];
  for(unsigned int j=0; j<nbinsn; j++) {
    register_class rc = input.rclass[o][j][prel];
    if (!input.infinite[input.space[rc]])
      RC.insert(rc);
  }
}  

presolver_conj normal_conjunction(const Parameters& input, const presolver_conj& c) {
  presolver_conj normal;

  for(const UnisonConstraintExpr& l : c) {
    if (l.id != CONNECTS_EXPR || opnd_temps(input, l.data[0]).size() > 1) // not entailed?
      vector_insert(normal, l);
  }
  return normal;
}

bool sorted_exprs(const presolver_conj& exprs) {
  for (unsigned int i=1; i<exprs.size(); i++)
    if (!(exprs[i-1] < exprs[i]))
      return false;
  return true;
}

UnisonConstraintExpr conj_to_expr(const presolver_conj& conjuncts) {
  if (conjuncts.size()==1) {
    return conjuncts[0];
  }
  assert(sorted_exprs(conjuncts));
  return UnisonConstraintExpr(AND_EXPR, {}, conjuncts);
}

UnisonConstraintExpr disj_to_expr(const presolver_disj& d) {
  vector<UnisonConstraintExpr> disjuncts;
  for(const presolver_conj& c : d)
    disjuncts.push_back(conj_to_expr(c));
  if (disjuncts.size()==1) {
    return disjuncts[0];
  }
  sort(disjuncts.begin(), disjuncts.end()); // canonicalize - sorted d does not mean sorted disjuncts
  return UnisonConstraintExpr(OR_EXPR, {}, disjuncts);
}

presolver_disj expr_to_disj(const UnisonConstraintExpr& e) {
  presolver_disj d;
  vector<UnisonConstraintExpr> disjuncts;
  if (e.id == OR_EXPR) {
    disjuncts = e.children;
  } else {
    disjuncts.push_back(e);
  }
  for (const UnisonConstraintExpr& e1 : disjuncts) {
    if (e1.id == AND_EXPR) {
      d.push_back(e1.children);
    } else {
      d.push_back({e1});
    }
  }
  return d;
}

bool disj_is_true(const presolver_disj& d) {
  return d.size()==1 && d[0].empty();
}

bool disj_is_false(const presolver_disj& d) {
  return d.empty();
}

void deepsort(int) {return;};

void deepsort(PresolverBefore &) {return;};

void deepsort(PresolverDominates & d) {
  deepsort(d.ins);
  deepsort(d.temps);
  return;
};

void deepsort(PresolverActiveTable & t) {
  sort(t.tuples.begin(), t.tuples.end());
  return;
};

string pre() { return "[pre]\t "; }

bool timeout(Support::Timer & t, PresolverOptions & options, string pass,
             Support::Timer & t0, bool print_time) {
  if (options.verbose() && print_time) {
    cerr << pre() << "presolving time (" << pass << "): " << ceil(t0.stop())
         << " ms" << endl;
  }
  if (t.stop() > options.timeout()) {
    cerr << pre() << "timeout after " << pass << endl;
    return true;
  }
  return false;
}
