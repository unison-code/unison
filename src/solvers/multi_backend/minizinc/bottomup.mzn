%
%  Main authors:
%    Mats Carlsson <mats.carlsson@ri.se>
%
%  This file is part of Unison, see http://unison-code.github.io
%
%  Copyright (c) 2016, RISE SICS AB
%  All rights reserved.
%
%  Redistribution and use in source and binary forms, with or without
%  modification, are permitted provided that the following conditions are met:
%  1. Redistributions of source code must retain the above copyright notice,
%     this list of conditions and the following disclaimer.
%  2. Redistributions in binary form must reproduce the above copyright notice,
%     this list of conditions and the following disclaimer in the documentation
%     and/or other materials provided with the distribution.
%  3. Neither the name of the copyright holder nor the names of its contributors
%     may be used to endorse or promote products derived from this software
%     without specific prior written permission.
%
%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%  POSSIBILITY OF SUCH DAMAGE.
%
%% go directly for optimal solution

solve :: seq_search(
     [int_search([obj], input_order, indomain_min, complete)] ++
     [bool_search([a[min(calleesaved_spill[i])] | i in index_set(calleesaved_spill)]++[true],
                  input_order, indomain_max, complete)] ++
     [seq_search([int_search([c[max(bb_ops[b])] , copysum[b]],
                             input_order, indomain_min, complete)] ++
                 [int_search([y[p] | p in bb_operands[b]
		 		   where operand_use[p] /\ op_mand[operand_definer[p]]],
		  first_fail, indomain_min, complete)] ++
		 [int_search([0]++[y[p] | p in bb_operands[b]
		                        where operand_use[p] /\ not op_mand[operand_definer[p]]],
		  first_fail, indomain_min, complete)])
     | b in bb_order] ++
     [int_search([ii[o] | o in bb_ops[b]], first_fail, indomain_min, complete)
     | b in bb_order] ++
     [int_search([c[o] | o in bb_ops[b]], smallest, indomain_min, complete)
     | b in bb_order] ++
     [int_search([r[t] | t in bb_temps[b] where temp_definer[t] != min(bb_ops[b])]++[0], first_fail, indomain_min, complete)
     | b in bb_order]
    ) minimize(obj);
