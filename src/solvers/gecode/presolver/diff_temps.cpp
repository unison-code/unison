/*
 *  Main authors:
 *    Noric Couderc <noric@sics.se>
 *    Mats Carlsson <mats.carlsson@ri.se>
 *
 *  Contributing authors:
 *    Roberto Castaneda Lozano <rcas@acm.org>
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
#include "./diff_temps.hpp"

using namespace std;

void diff_temps(Parameters& input) {
    // M <- E <- empty
    multimap<vector<temporary>, pair<operation, operand>> M;
    vector<vector<operand>> E;

    vector<vector<temporary>> M_keys; // Keys stored for convenience

    // For all o in JSON.O where o is mandatory and OperType(o)
    // is not (in) or (kill)
    for(operation o : input.O) {
        if(is_mandatory(input, o)) {
            if(oper_type(input, o) != IN && oper_type(input, o) != KILL) {
                // For all p in OperUses(o)
              for(operand p : oper_uses(input, o)) {
                // AddToMap(OpndTemps(p) -> <o,p>, M)
                pair<operation, operand> edge =
                  make_pair(o, p);
                M.insert(make_pair(opnd_temps(input, p), edge));
                M_keys.push_back(opnd_temps(input, p));
              }
            }
        }
    }

    // P <- Closure(MakeDigraph(JSON.precs))
    Digraph G = Digraph(input.precs);
    Digraph P = G.closure();

    // Build all C (Drop the keys of M)
    vector<vector<pair<operation, operand>>> Cs;
    for(const vector<temporary>& k : M_keys) {
        auto range = M.equal_range(k);
        // Build C
        vector<pair<operation, operand>> C;
        transform(range.first, range.second, back_inserter(C),
                [](pair<
                    vector<temporary>,
                    pair<operation, operand>
                    > key_value){
                    // returns pair<operation, operand>
                    return key_value.second;
                });
        Cs.push_back(C);
    }

    // For all _ -> C in M
    for(const vector<pair<operation,operand>>& C : Cs) {
        // For all {<o,p>, <o', p'> } subsets of C where
        // (o,o') in P and p in JSON.last_use
        for (auto op = C.begin(); op != C.end(); ++op) {
            for (auto o1p1 = op; o1p1 != C.end(); ++o1p1) {
                operation o  = op->first;
                operation o1 = o1p1->first;
                operand p  = op->second;
                operand p1 = o1p1->second;

		// Different operands of one instruction can use same tmp
		if (input.type[o] != OUT && o == o1) continue;
                // build (o,o')
                pair<operation, operation> oo1 = make_pair(o, o1);

                // (o,o') in P and p in JSON.last_use
                if(ord_contains(P.edges(), oo1) && ord_contains(input.last_use, p)) {
                    // E <- E U {{p, p'}}
		  E.push_back({p,p1});
		  E.push_back({p1,p});
                }
            }
        }
    }

    input.difftemps = Digraph(E).max_cliques();
}
