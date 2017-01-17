%
%  Main authors:
%    Mats Carlsson <matsc@sics.se>
%
%  This file is part of Unison, see http://unison-code.github.io
%
%  Copyright (c) 2016, SICS Swedish ICT AB
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
%% normal branch & bound search

% solve :: seq_search(
%      [bool_search([a[min(calleesaved_spill[i])] | i in index_set(calleesaved_spill)]++[true],
%                   input_order, indomain_max, complete)] ++
%      [bool_search([a[op_order[o]] | o in bb_ops[b]], input_order, indomain_min, complete)
%      | b in bb_order] ++
%      [int_search([i[o] | o in bb_ops[b]], input_order, indomain_min, complete)
%      | b in bb_order] ++
%      [int_search([y[p] | p in bb_operands[b]], input_order, indomain_min, complete)
%      | b in bb_order] ++
%      [int_search([c[o] | o in bb_ops[b]], smallest, indomain_min, complete)
%      | b in bb_order] ++
%      [int_search([r[t] | t in bb_temps[b]], first_fail, indomain_min, complete)
%      | b in bb_order]
%     ) minimize(obj);

% "baseline"
% solve :: seq_search(
%      [bool_search([a[min(calleesaved_spill[i])] | i in index_set(calleesaved_spill)]++[true],
%                   input_order, indomain_max, complete)] ++
%      [seq_search([int_search([c[max(bb_ops[b])] , copysum[b]],
%                              input_order, indomain_min, complete)] ++
%                  [int_search([y[p] | p in bb_operands[b]
% 		 		   where operand_use[p] /\ op_mand[operand_definer[p]]],
% 		  first_fail, indomain_min, complete)] ++
% 		 [int_search([0]++[y[p] | p in bb_operands[b]
% 		                        where operand_use[p] /\ not op_mand[operand_definer[p]]],
% 		  first_fail, indomain_min, complete)])
%      | b in bb_order] ++
%      [int_search([i[o] | o in bb_ops[b]], input_order, indomain_min, complete)
%      | b in bb_order] ++
%      [int_search([c[o] | o in bb_ops[b]], smallest, indomain_min, complete)
%      | b in bb_order] ++
%      [int_search([r[t] | t in bb_temps[b] where temp_definer[t] != min(bb_ops[b])]++[0], first_fail, indomain_min, complete)
%      | b in bb_order]
%     ) minimize(obj);

% "all basic blocks in parallel"
% solve :: seq_search(
%      [bool_search([a[min(calleesaved_spill[i])] | i in index_set(calleesaved_spill)]++[true],
%                   input_order, indomain_max, complete)] ++
%      [int_search([c[max(bb_ops[b])] , copysum[b]],
%                  input_order, indomain_min, complete)
%      | b in bb_order] ++
%      [int_search([y[p] | p in 0..MAXP where operand_use[p] /\ op_mand[operand_definer[p]]],
% 		 first_fail, indomain_min, complete)] ++
%      [int_search([0]++[y[p] | p in 0..MAXP where operand_use[p] /\ not op_mand[operand_definer[p]]],
% 		 first_fail, indomain_min, complete)] ++
%      [int_search([i[o] | o in 0..MAXO],
%                  input_order, indomain_min, complete)] ++
%      [int_search([c[o] | o in 0..MAXO],
%                  smallest, indomain_min, complete)] ++
%      [int_search([r[t] | t in 0..MAXT where op_type[temp_definer[t]] != TYPE_IN]++[0],
%                  first_fail, indomain_min, complete)]
%     ) minimize(obj);

% "one bb at a time"
% solve :: seq_search(
%      [bool_search([a[min(calleesaved_spill[i])] | i in index_set(calleesaved_spill)]++[true],
%                   input_order, indomain_max, complete)] ++
%      [seq_search([int_search([c[max(bb_ops[b])] , copysum[b]],
%                              input_order, indomain_min, complete)] ++
%                  [int_search([y[p] | p in bb_operands[b]
% 		 		   where operand_use[p] /\ op_mand[operand_definer[p]]],
% 		  first_fail, indomain_min, complete)] ++
% 		 [int_search([0]++[y[p] | p in bb_operands[b]
% 		                        where operand_use[p] /\ not op_mand[operand_definer[p]]],
% 		  first_fail, indomain_min, complete)] ++
% 		 [int_search([i[o] | o in bb_ops[b]], input_order, indomain_min, complete)] ++
% 		 [int_search([c[o] | o in bb_ops[b]], smallest, indomain_min, complete)] ++
% 		 [int_search([r[t] | t in bb_temps[b] where temp_definer[t] != min(bb_ops[b])]++[0], first_fail, indomain_min, complete)])
%      | b in bb_order]
%     ) minimize(obj);

% "free c[(out)], copysum"
% solve :: seq_search(
%      [bool_search([a[min(calleesaved_spill[i])] | i in index_set(calleesaved_spill)]++[true],
%                   input_order, indomain_max, complete)] ++
%      [seq_search([int_search([y[p] | p in bb_operands[b]
% 		 		   where operand_use[p] /\ op_mand[operand_definer[p]]],
% 		  first_fail, indomain_min, complete)] ++
% 		 [int_search([0]++[y[p] | p in bb_operands[b]
% 		                        where operand_use[p] /\ not op_mand[operand_definer[p]]],
% 		  first_fail, indomain_min, complete)])
%      | b in bb_order] ++
%      [int_search([i[o] | o in bb_ops[b]], input_order, indomain_min, complete)
%      | b in bb_order] ++
%      [int_search([c[o] | o in bb_ops[b]], smallest, indomain_min, complete)
%      | b in bb_order] ++
%      [int_search([r[t] | t in bb_temps[b] where op_type[temp_definer[t]] != TYPE_IN]++[0], first_fail, indomain_min, complete)
%      | b in bb_order]
%     ) minimize(obj);

% "c[(out)] later"
solve :: seq_search(
     % can help, but far from always
     % [bool_search([a[min(calleesaved_spill[i])] | i in index_set(calleesaved_spill)]++[true],
     %              input_order, indomain_max, complete)] ++
     % can help, but far from always
     % [bool_search([a[min(activator_ops[i])] | i in index_set(activator_ops)]++[true],
     %              input_order, indomain_max, complete)] ++
     [seq_search([int_search([y[p] | p in bb_operands[b]
    		 		   where operand_use[p] /\ op_mand[operand_definer[p]]],
    		  first_fail, indomain_min, complete)] ++
    		 [int_search([0]++[y[p] | p in bb_operands[b]
    		                        where operand_use[p] /\ not op_mand[operand_definer[p]]],
    		  first_fail, indomain_min, complete)])
     | b in bb_order] ++
     [int_search([i[o] | o in bb_ops[b]], input_order, indomain_min, complete)
     | b in bb_order] ++
     [seq_search([int_search([c[max(bb_ops[b])]], input_order, indomain_min, complete),
                  int_search([c[o] | o in bb_ops[b]], smallest, indomain_min, complete)])
     | b in bb_order] ++
     [int_search([r[t] | t in bb_temps[b] where op_type[temp_definer[t]] != TYPE_IN]++[0], first_fail, indomain_min, complete)
     | b in bb_order]
    )
    minimize(obj);
