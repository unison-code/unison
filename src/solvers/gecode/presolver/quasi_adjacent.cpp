/*
 *  Main authors:
 *    Mats Carlsson <matsc@sics.se>
 *
 *  This file is part of Unison, see http://unison-code.github.io
 *
 *  Copyright (c) 2016, SICS Swedish ICT AB
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
#include "./quasi_adjacent.hpp"

void quasi_adjacent(Parameters& input) {
  Digraph OI = Digraph(input.adjacent);

  // connected out with single successor implies connected in
  for(operand p : OI.vertices()) {
    vector<operand> succ = OI.neighbors(p);
    if(succ.size() > 0) {
      operand q = succ[0];
      if (succ.size()==1 && first_temp(input, q)==NULL_TEMPORARY) {
	vector<operand> qp = {q,p};
	vector_insert(input.quasi_adjacent, qp);
      }
    }
  }

  // find (in) just passing thru to (out): (in) implies (out)
  for(operand p : OI.vertices()) {
    vector<operand> succ = OI.neighbors(p);
    if(succ.size() > 0) {
      if(first_temp(input, p)==NULL_TEMPORARY) {
	temporary t1 = opnd_temps(input, p)[1];
	operand d = temp_def(input, t1);
	if(oper_type(input, opnd_oper(input, d))!=IN)
	  goto nextp;
	for(operand q : temp_uses(input, t1)) {
	  int ty = oper_type(input, opnd_oper(input, q));
	  if (ty!=COPY && ty!=OUT)
	    goto nextp;
	}
	vector<operand> pd = {p,d};
	if (!ord_contains(input.adjacent, pd))
	  vector_insert(input.quasi_adjacent, pd);
      }
    }
  nextp: ;
  }

  // find (out) implies (in) if it inherits incoming value
  for(operand p : OI.vertices()) {
    vector<operand> succ = OI.neighbors(p);
    if(succ.size() > 0) {
      for(temporary t1 : opnd_temps(input, p)) {
	if(t1!=NULL_TEMPORARY) {
	  operation o = temp_oper(input, t1);
	  while (oper_type(input, o)==COPY) {
	    t1 = first_temp_but_null(input, first_use(input, o));
	    o = temp_oper(input, t1);
	  }
	  if (oper_type(input, o)!=IN)
	    goto nextp2;
	}
      }
      temporary t1 = first_temp_but_null(input, p);
      operation o = temp_oper(input, t1);
      for(operand u : oper_defs(input, o)) {
	vector<temporary> ts = opnd_temps(input, u);
	if(ts[0]==NULL_TEMPORARY && ts[1]==t1) {
	  vector<operand> up = {u,p};
	  vector_insert(input.quasi_adjacent, up);
	}
      }
    }
  nextp2: ;
  }
}
