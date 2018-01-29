/*
 *  Main authors:
 *    Noric Couderc <noric@sics.se>
 *
 *  Contributing authors:
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
#include "./unsafe_temp.hpp"

using namespace std;

bool temp_is_unsafe(const Parameters& input, const temporary t) {
  // operand that can define temporary t
  operand d = temp_def(input, t);
  // operands that can use temporary t
  vector<operand> U = temp_uses(input, t);
  // operation that defines temporary t
  operation o = temp_oper(input, t);

  int otype = input.type[o];
  if (otype != IN  && otype != DEFINE && otype != FUN) {
    int ld = std::numeric_limits<int>::max();
    int lu = std::numeric_limits<int>::max();

    // For each instruction that can implement operation o
    for(unsigned int i = 0; i < oper_insns(input, o).size(); ++i) {
      // Latency of operand d, in operation o, if it' implemented by
      // instruction i
      if(oper_insns(input, o)[i] != NULL_INSTRUCTION) {
	unsigned int di = index_of(oper_opnds(input, o), d);
	ld = std::min(ld, input.lat[o][i][di]);
      }
    } // I get minimum latency of the definer of temporary
    // I assume that the operation does not have ONLY null
    // instructions

    // For operand that can use t
    for(operand u : U) {
      // For the operation that might use t
      operation o1 = opnd_oper(input, u);
      // For each instruction that can implement operation o1
      for(unsigned int i = 0; i < oper_insns(input, o1).size(); ++i) {
	if(oper_insns(input, o1)[i] != NULL_INSTRUCTION) {
	  unsigned int ui = index_of(oper_opnds(input, o1), u);
	  lu = std::min(lu, input.lat[o1][i][ui]);
	}
      }
    } // I get minimum latency of all the users of the temporary

    // If the minimum live range (which is min 1 cycle) of the
    // temporary is bigger than ld + lu
    if (input.minlive[t] > ld + lu)
      return true;
  }
  return false;
}

