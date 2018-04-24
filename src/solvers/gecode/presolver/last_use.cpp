/*
 *  Main authors:
 *    Mats Carlsson <mats.carlsson@ri.se>
 *    Noric Couderc <noric@sics.se>
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
#include <functional>
#include "./last_use.hpp"

void last_use(PresolverAsserts& PA, Parameters& input) {
    map<temporary, vector<operand>> M;

    // For all o in JSON.O where o is mandatory
    for(operation o : input.O) {
      // For all p in OperUses(o) where p is mandatory
      if(is_mandatory(input, o))
	for (operand p : input.operands[o])
	  if (input.use[p]) {
	    // if TempDef(min(OpndTempsButNull(p))) is mandatory
	    temporary min_temp = first_temp_but_null(input, p);
	    if(first_temp(input, input.definer[min_temp]) != NULL_TEMPORARY) {
	      // AddToMap(min(OpndTempsButNull(p)) -> p, M)
	      M[min_temp].push_back(p);
	    }
	  }

      unsigned int op_type = oper_type(input, o);
      switch (op_type) {
      case TAILCALL:
      case OUT:
      case KILL:
	// For all p in OperUses(o)
	for (operand p : input.operands[o])
	  if (input.use[p])
	    // LastUse <- LastUse U {p}
	    input.last_use.push_back(p);
	break;
	// if OperType(o) = FUNCTION
      case FUN:
	// For all p in OperUses(o)
	for (operand p : input.operands[o])
	  if (input.use[p] && p_preassigned_caller_saved(input, p)) // caller-saved, but not reserved
	    input.last_use.push_back(p);
	break;
      case LINEAR:
	  // If there exist C in JSON.strictly_congr, def d in C, use u in C
	  // where {u, d} is a subset of OperOpnds(o)
	  // then LastUse <- LastUse U {u}
	for (pair<operand,operand> ud : input.redefined[o])
	  input.last_use.push_back(ud.first);
	break;
      }
    }

    // For all _ -> P in M
    for(const pair<temporary, vector<operand>>& kps : M) {
      vector<operand> P = kps.second;
      // b <- the block containing P
      block b = block_containing(input, P);

      if(b < 0) { continue; }
      Digraph data_dependencies = PA.dd_graph[b];

      // for all p in P where OperType(OpndOper(p)) != BRANCH while b >= 0
      for(operand p : P) { // For all p in P
	// Where OperType(OpndOper(p)) != BRANCH
	if(oper_type(input, opnd_oper(input, p)) == BRANCH) {
	  continue;
	}
	// if for all q in P\{p}:
	// OpndOper(p) in Reachable(OpndOper(q), DDGraph(b))
	// DDGraph(b)
	bool reachable = std::all_of(P.begin(), P.end(),
				     [&data_dependencies, &input, &p](operand q) {
				       if(p == q) { return true; }
				       vector<vertex> reachables =
				       data_dependencies.reachables(opnd_oper(input, q));
				       return ord_contains(reachables, opnd_oper(input, p));
				     });

	if(reachable) {
	  input.last_use.push_back(p);
	  b = -1;
	}
      }
    }

    std::sort(input.last_use.begin(), input.last_use.end());
    input.last_use.erase(unique(input.last_use.begin(), input.last_use.end()), input.last_use.end());
}

