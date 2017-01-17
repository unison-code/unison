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

include "globals.mzn";

%% solver parameters

bool: good_cumulative;
bool: optimize_cycles;

%% some constants
%% N.B. keep in sync with modeler!  see also isreal()!

int: TYPE_LINEAR  = 0;
int: TYPE_BRANCH  = 1;
int: TYPE_CALL    = 2;
int: TYPE_TAIL    = 3;
int: TYPE_IN      = 4;
int: TYPE_OUT     = 5;
int: TYPE_KILL    = 6;
int: TYPE_DEFINE  = 7;
int: TYPE_COMBINE = 8;
int: TYPE_PACK    = 9;
int: TYPE_LOW     = 10;
int: TYPE_HIGH    = 11;
int: TYPE_FUN     = 12;
int: TYPE_COPY    = 13;

%% problem parameters

int: MAXF;
int: MAXO;
int: MAXP;
int: MAXT;
int: MAXC;
int: MAXI;
int: MAXR;
int: MAXB = max(index_set(bb_ops));
int: MINL;
int: MAXL;

set of int: conjunction = index_set(sets);

%% see JSON freq, maxc, ins, tmp, operands, optional_min
array[int] of int: bb_order;
array[int] of set of int: bb_ops;
array[int] of set of int: bb_operands;
array[int] of set of int: bb_temps;
array[int] of int: bb_frequency;
array[int] of int: bb_maxcycle;
array[int] of int: bb_optional_min;

%% see JSON operands, instructions, type
array[int] of set of int: op_operands;
array[int] of set of int: op_instructions;
array[int] of int: op_type;
array[int] of bool: op_mand;
array[int] of int: op_order;

%% see JSON atoms
array[int] of set of int: atom_regs; % atom -> registers

%% see JSON calleesaved, callersaved, infinite, range
set of int: calleesaved;
set of int: callersaved;
set of int: infinite;

%% see JSON cap, con, dur, off
array[int] of int: res_cap;
array[int,int] of int: res_con;
array[int,int] of int: res_dur;
array[int,int] of int: res_off;

%% see JSON congr
array[int] of set of int: congr;

%% see JSON preassign
array[int] of int: preassign_operand;
array[int] of int: preassign_reg;

%% see JSON aligned/adist
array[int] of int: aligned_use;
array[int] of int: aligned_def;
array[int] of int: aligned_usei;
array[int] of int: aligned_defi;
array[int] of int: aligned_dist;

%% see JSON adjacent, only the subset that is not covered by congr
array[int] of int: adj_from;
array[int] of int: adj_to;

%% see JSON quasi_adjacent
array[int] of int: quasi_adj_from;
array[int] of int: quasi_adj_to;

%% see JSON operands
array[int] of int: operand_definer; % operand -> defining op

%% see JSON use
array[int] of bool: operand_use;

%% see JSON last_use
array[int] of bool: operand_lastuse;

%% see JSON temps
array[int] of set of int: operand_temps;

%% see JSON class, operands, instructions
array[int,int] of int: operand_atom; % operand x operation -> atom

%% see JSON temps
array[int] of set of int: related_temps;

%% see JSON definer, temps, width, minlive, temp_unsafe
array[int] of int: temp_definer; % temp -> defining op
array[int] of int: temp_def;     % temp -> defining operand
array[int] of int: temp_width;   % temp -> width
array[int] of int: temp_minlive; % temp -> minlive
array[int] of bool: temp_unsafe; % temp -> unsafe
array[int] of set of int: temp_uses; % temp -> set of use operands

%% see JSON packed, pinstrs
array[int,int] of int: packed_pq;
array[int] of set of int: packed_is;

%% see JSON before
array[int] of int: before_pred;
array[int] of int: before_succ;
array[int] of set of conjunction: before_cond;

%% see JSON nogoods
array[int] of conjunction: nogood;

%% see JSON across
array[int] of int: across_op;
array[int] of set of int: across_regs;
array[int] of set of int: across_items;
array[int] of int: across_item_temp;
array[int] of set of conjunction: across_item_cond;

%% see JSON set_across
array[int] of int: setacross_op;
array[int] of set of int: setacross_regs;
array[int] of set of int: setacross_tempsets;

%% see JSON difftemps
array[int] of set of int: difftemp;

%% see JSON diffregs
array[int] of set of int: diffreg;

%% see JSON domops
array[int] of set of int: domop_operands;
array[int] of set of int: domop_temps;

%% see JSON domuse
array[int] of int: domuse_p;
array[int] of int: domuse_q;
array[int] of int: domuse_r;

%% see JSON memassign
array[int] of int: memassign_temps;
array[int] of int: memassign_regs;

%% see JSON dominates
array[int] of int: dominate_ing;
array[int] of int: dominate_ed;
array[int] of set of int: dominate_instructions;
array[int] of set of int: dominate_temps;

%% see JSON precedences
array[int] of int: precedence_pred;
array[int] of int: precedence_succ;
array[int] of int: precedence_diff;
array[int] of set of conjunction: precedence_cond;

%% see JSON active_tables, tmp_tables
array[int] of set of int: table_exists_ops;
array[int] of set of int: table_iffall_ops;
array[int] of set of int: relation_ops;
array[int] of set of int: relation_temps;
array[int] of int: relation_ntuples;
array[int] of set of int: relation_range;

%% see JSON calleesaved_spill
array[int] of set of int: calleesaved_spill;
array[int,int] of int: cs_spill_transpose;

%% see JSON activators
array[int] of set of int: activator_insns;
array[int] of set of int: activator_ops;

%% see JSON predecessors
array[int] of set of int: predecessors_preds;
array[int] of int: predecessors_succ;
array[int] of int: predecessors_lat;

%% see JSON successors
array[int] of set of int: successors_succs;
array[int] of int: successors_pred;
array[int] of int: successors_lat;

%% see JSON instr_cond
array[int,int] of int: instr_cond;

%% see JSON value_precede_chain
array[int] of int: value_precede_min;
array[int] of int: value_precede_max;
array[int] of set of int: value_precede_regs;
array[int] of int: value_precede_temps;

%% see JSON lat
array[int,int] of int: lat_table;

%% extra storage for conjunctions and disjunctions
array[int] of int: literal_op;
array[int] of int: literal_arg1;
array[int] of int: literal_arg2;

%% extra store for relations in table constraints
array[int] of int: ints;

%% extra storage: each set is either a conjunction or a set of temporaries
array[int] of set of int: sets;

%% problem variables

var 0..MAXF: obj;
array[0..MAXO] of var bool: a;
array[0..MAXO] of var 0..MAXI: i;
array[0..MAXO] of var -1..MAXC: c;
array[0..MAXP] of var -1..MAXT: y;
array[0..MAXP] of var -1..MAXR: ry;
array[0..MAXP] of var MINL..MAXL: lt; % operand latency
array[0..MAXP] of var -MAXL..MAXL: s; % global operand slack
array[-1..MAXT] of var -1..MAXR: r;
array[-1..MAXT] of var -1..MAXC: ls;
array[-1..MAXT] of var 0..MAXC: ld;
array[-1..MAXT] of var -1..MAXC: le;
array[1..MAXB] of var 0..MAXC: copysum;

%% PREDICATES AND TESTS

test isreal(int: o) = (op_type[o] in
                       {TYPE_LINEAR, TYPE_BRANCH, TYPE_CALL, TYPE_TAIL, TYPE_FUN, TYPE_COPY});

test can_use(int: p, int: temp) = (
      operand_use[p] /\
      temp in operand_temps[p]
);

test fixedop(int: o) = (
    op_instructions[o] != {0} /\ card(op_instructions[o]) = 1
);

var 0..MAXI: insn(int: o) = if fixedop(o) then min(op_instructions[o]) else i[o] endif;

predicate sliced_diffn(array[int] of var int: x,
                       array[int] of var int: y,
                       array[int] of var int: dx,
                       array[int] of var int: dy) =
    diffn(x, y, [dx[t]*bool2int(dy[t]>0) | t in index_set(dx)], dy) :: domain
;

% stronger, non-scalable variant
% predicate sliced_diffn(array[int] of var int: x,
%                        array[int] of var int: y,
%                        array[int] of var int: dx,
%                        array[int] of var int: dy) =
%     let {set of int: I = index_set(x),
%          int: mint = min([lb(y[i]) | i in I]),
%          int: maxt = max([ub(y[i])+ub(dy[i])-1 | i in I])} in (
%         forall(t in mint..maxt)(
%            let {array[I] of var int: x0,
%                 array[I] of var int: x1,
%                 array[I] of var int: x2,
%                 array[I] of var int: x3,
%                 array[I] of var int: x4,
%                 array[I] of var int: x5,
%                 array[I] of var int: x6,
%                 array[I] of var int: x7} in (
% 	       forall(j in I)(
%                    let {var bool: precond = t>=y[j] /\ t<y[j]+dy[j]} in (
% 		       ite(precond /\ dx[j]>=1, x[j]  , -8*j  , x0[j]) /\
% 		       ite(precond /\ dx[j]>=2, x[j]+1, -8*j+1, x1[j]) /\
% 		       ite(precond /\ dx[j]>=3, x[j]+2, -8*j+2, x2[j]) /\
% 		       ite(precond /\ dx[j]>=4, x[j]+3, -8*j+3, x3[j]) /\
% 		       ite(precond /\ dx[j]>=5, x[j]+4, -8*j+4, x4[j]) /\
% 		       ite(precond /\ dx[j]>=6, x[j]+5, -8*j+5, x5[j]) /\
% 		       ite(precond /\ dx[j]>=7, x[j]+6, -8*j+6, x6[j]) /\
% 		       ite(precond /\ dx[j]>=8, x[j]+7, -8*j+7, x7[j])
% 	           )
% 	       ) /\
% 	       all_different(x0 ++ x1 ++ x2 ++ x3 ++ x4 ++ x5 ++ x6 ++ x7) :: domain
% 	   )
%         )
%     );


% standard variant
predicate disjoint1(array[int] of var int: x,
                      array[int] of var int: w) =
	    diffn(x, [1 | i in index_set(x)], w, [1 | i in index_set(x)]) :: domain
;

% stronger, non-scalable variant
% predicate disjoint1(array[int] of var int: x,
%                     array[int] of var int: w) = (
%     let {int: maxw = max([ub(y) | y in w])} in (
%         if maxw=1 then
% 	    all_different(x) :: domain
%         else if maxw=2 then
%            let {set of int: isetx = index_set(x),
%                 array[isetx] of var int: x1} in (
% 	       forall(j in isetx)(
% 		   ite(w[j]>=2, x[j]+1, -8*j+1, x1[j])
%                ) /\
% 	       all_different(x ++ x1) :: domain
% 	   )
%         else if maxw=4 then
%            let {set of int: isetx = index_set(x),
%                 array[isetx] of var int: x1,
%                 array[isetx] of var int: x2,
%                 array[isetx] of var int: x3} in (
% 	       forall(j in isetx)(
% 		   ite(w[j]>=2, x[j]+1, -8*j+1, x1[j]) /\
% 		   ite(w[j]>=4, x[j]+2, -8*j+2, x2[j]) /\
% 		   ite(w[j]>=4, x[j]+3, -8*j+3, x3[j])
% 	       ) /\
% 	       all_different(x ++ x1 ++ x2 ++ x3) :: domain
% 	   )
%         else if maxw=8 then
%            let {set of int: isetx = index_set(x),
%                 array[isetx] of var int: x1,
%                 array[isetx] of var int: x2,
%                 array[isetx] of var int: x3,
%                 array[isetx] of var int: x4,
%                 array[isetx] of var int: x5,
%                 array[isetx] of var int: x6,
%                 array[isetx] of var int: x7} in (
% 	       forall(j in isetx)(
% 		   ite(w[j]>=2, x[j]+1, -8*j+1, x1[j]) /\
% 		   ite(w[j]>=4, x[j]+2, -8*j+2, x2[j]) /\
% 		   ite(w[j]>=4, x[j]+3, -8*j+3, x3[j]) /\
% 		   ite(w[j]>=8, x[j]+4, -8*j+4, x4[j]) /\
% 		   ite(w[j]>=8, x[j]+5, -8*j+5, x5[j]) /\
% 		   ite(w[j]>=8, x[j]+6, -8*j+6, x6[j]) /\
% 		   ite(w[j]>=8, x[j]+7, -8*j+7, x7[j])
% 	       ) /\
% 	       all_different(x ++ x1 ++ x2 ++ x3 ++ x4 ++ x5 ++ x6 ++ x7) :: domain
% 	   )
%         else true endif endif endif endif
%     )
% );

predicate noverlap(array[int] of var int: x,
                   array[int] of var int: w) = (
	if min([lb(y) | y in w]) < max([ub(y) | y in w]) then
	    disjoint1(x, w)
	else if length(x) = 2 then
	    x[1] != x[2]
	else
	    all_different(x) :: domain
	endif endif
    );

predicate live_overlap(var int: i, var int: j) = (ls[i] < le[j] /\ ls[j] < le[i]);

predicate eval_literal(int: l) = (
    let {int: tag = literal_op[l],
         int: arg1 = literal_arg1[l],
         int: arg2 = literal_arg2[l]} in (
	if tag=0 then y[arg1] = y[arg2]
	else if tag=1 then y[arg1] = arg2
	else if tag=2 then a[arg1]
	else if tag=3 then i[arg1] = arg2
	else if tag=4 then live_overlap(y[arg1],y[arg2])
	else if tag=5 then live_overlap(arg1,arg2)
	else if tag=6 then r[arg1] in callersaved
	else if tag=7 then i[arg1] != arg2
	else if tag=8 then ry[arg1] in atom_regs[arg2]
        else false endif endif endif endif endif endif endif endif endif
    )
);

predicate eval_conjunction(int: conj) = (
    forall(lit in sets[conj])(eval_literal(lit))
);

predicate eval_disjunction(set of int: disj) = (
    exists(d in disj)(eval_conjunction(d))
);

predicate ite(var bool: cond,
              var int: thenval,
	      var int: elseval,
	      var int: value) = (
    let {array[0..1] of var int: ar = array1d(0..1, [elseval,thenval])} in (
        value = ar[bool2int(cond)]
    )
);

0..MAXT: single_temp(int: p) = max(operand_temps[p]);

set of 0..MAXT: real_temps(int: p) = operand_temps[p] intersect 0..MAXT;

%% (mostly) ESSENTIAL CONSTRAINT

%% basic invariants, null instructions

constraint
    r[-1] = -1 /\
    ls[-1] = -1 /\
    ld[-1] = 0 /\
    le[-1] = -1 /\
    forall(o in 0..MAXO where op_mand[o]) (a[o]) /\
    forall(o in 0..MAXO) (i[o] >= 1 <-> a[o]) /\
    forall(o in 0..MAXO) (c[o] >= 0 <-> a[o]) /\
    forall(p in 0..MAXP where card(operand_temps[p])>1) (ry[p] = r[y[p]]) /\
    forall(p in 0..MAXP where card(operand_temps[p])=1) (ry[p] = r[single_temp(p)]) /\
    forall(p in 0..MAXP where not (op_type[operand_definer[p]] in {TYPE_IN,TYPE_OUT})) % rematerialization
          (y[p] >= 0 <-> a[operand_definer[p]]) /\
    forall(v in 0..MAXT) (ls[v]+ld[v] = le[v]) /\
    forall(v in 0..MAXT where op_type[temp_definer[v]] != TYPE_IN) % rematerialization
          (ls[v] = c[temp_definer[v]] /\
	   (r[v] >= 0 <-> a[temp_definer[v]])) /\
    forall(v in 0..MAXT where op_type[temp_definer[v]] = TYPE_IN) % rematerialization
          (ls[v] in -1 .. 0 /\
	   (r[v] >= 0 <-> ls[v]=0) /\
	   (r[v] >= 0 <-> y[temp_def[v]] = v)) /\
    forall(v in 0..MAXT) (
	let {set of int: pset = {p | p in 0..MAXP where can_use(p,v)},
	     set of int: prange = 1..card(pset),
	     array[prange] of int: parr = [p | p in pset],
	     array[prange] of var -1..MAXC: carr} in (
	    forall(i in prange)
		  (ite((y[parr[i]]=v), c[operand_definer[parr[i]]], -1, carr[i])) /\
	    if card(pset)=1 /\ not temp_unsafe[v] then
	      le[v] = carr[1]
	    else
	      maximum(le[v], carr++[ls[v]+ % 20130930: live range can extend past last use
				    temp_minlive[v]*bool2int(a[def])
				   | def in [temp_definer[v]] where temp_unsafe[v]])
	    endif
	)
    );

%% objective, bounds on (out) cycle

constraint
    forall(b in 1..MAXB) (
	let {int: copysize = min(o in bb_ops[b], x in op_instructions[o]
				 where not op_mand[o] /\ x>0)(res_con[1,x])
	     % int: bundlesize = max(x in 0..MAXI)(res_con[1,x]),
	     % int: realsum = sum(o in bb_ops[b] where isreal(o))(1),
	     % int: realtotal = sum(o in bb_ops[b] where isreal(o))
	     % 		     (min(x in op_instructions[o])(res_con[1,x]))
				 } in (
	    forall(o in bb_ops[b])(c[o] in -1..bb_maxcycle[b]) /\
	    c[min(bb_ops[b])] = 0 /\
	    copysum[b] = sum(o in bb_ops[b] where not op_mand[o])(bool2int(a[o])) /\
	    copysum[b] >= bb_optional_min[b]
	    % wrong e.g. for ARM
	    % copysum[b] + realsum + 2 >= c[max(bb_ops[b])] /\
	    % copysize*copysum[b] + realtotal <= bundlesize*(c[max(bb_ops[b])]-1)
	)
    ) /\
    if optimize_cycles then
      obj = sum(b in 1..MAXB) (bb_frequency[b] * c[max(bb_ops[b])])
    else
      obj = sum(o in 0..MAXO)(res_con[1,insn(o)])
    endif;

%% domains
constraint
    forall(o in 0..MAXO) (i[o] in op_instructions[o]) /\
    forall(p in 0..MAXP) (y[p] in operand_temps[p]) /\
    forall(v in 0..MAXT) (
        let {int: p = temp_def[v],
	     set of int: rdom = array_union([atom_regs[operand_atom[p,q]] | q in 1..MAXI]),
    	     set of int: joker = if min(operand_temps[p])>=0 then {} else {-1} endif, % rematerialization
	     set of int: mdom =
	                 {memassign_regs[i] | i in index_set(memassign_temps) where memassign_temps[i]=v}} in (
    	    if card(mdom)=0 then
    	      r[v] in rdom union joker
    	    else
    	      r[v] in (rdom diff infinite) union mdom union joker
    	    endif
        )
    );

%% operation selection (register set depending on operation)
constraint
    forall(p in 0..MAXP)
          (let {int: o = operand_definer[p],
	  	set of int: joker = if min(operand_temps[p])>=0 then {} else {-1} endif} in (
	       if fixedop(o) then
	           ry[p] in atom_regs[operand_atom[p,min(op_instructions[o])]] union joker % rematerialization
	       else
		   forall(insn in op_instructions[o])
			 (if insn=0 then
			     i[o] = 0 -> ry[p] = -1
			  else
			     i[o] = insn -> ry[p] in atom_regs[operand_atom[p,insn]]
			  endif)
	       endif
           )
    );

%% preassignments
constraint
    forall(j in index_set(preassign_reg))
          (ry[preassign_operand[j]] = preassign_reg[j]);

%% congruences
constraint
    forall(cgr in congr) (
        let {int: junior = min(cgr)} in (
	    forall(j in cgr where j>junior) (ry[junior] = ry[j])
 	)
    );

%% adjacencies NEW FOR REMATERIALIZATION
constraint
    forall(j in index_set(adj_from)) (
        let {int: p = adj_from[j],
	     int: q = adj_to[j]} in
            (y[q] >= 0 -> y[p] >= 0 /\ ry[p] = ry[q]) /\
            (y[p] >= 0 -> exists(k in index_set(adj_from) where adj_from[k] = p)(y[adj_to[k]] >= 0))
    );

constraint
    forall(j in index_set(quasi_adj_from)) (
        let {int: p = quasi_adj_from[j],
	     int: q = quasi_adj_to[j]} in
            y[q] >= 0 -> y[p] >= 0
    );

% DISABLED, in favor of quasi_adjacent
% constraint % connected (out) inheriting from connected (in)
%     forall(pout in 0..MAXP where op_type[operand_definer[pout]] = TYPE_OUT)(
%         let {set of int: tsout = operand_temps[pout],
% 	     set of int: tsoutbutnull = tsout diff {-1},
% 	     int: t1 = min(tsoutbutnull),
% 	     int: oin = temp_definer[t1],
% 	     int: pin = temp_def[t1],
% 	     set of int: tsin = operand_temps[pin],
% 	     set of int: tsc = {v | v in tsoutbutnull where op_type[temp_definer[v]] in {TYPE_IN,TYPE_COPY}}} in
% 	    if min(tsout) = -1 /\ min(tsin) = -1 /\ op_type[oin] = TYPE_IN then
% 	       y[pout] in tsc -> y[pin] >= 0
% 	    else true endif
%     );

%?% constraint
%?%     forall(j in index_set(adj_from)) (
%?%         let {int: p = adj_from[j],
%?% 	     int: q = adj_to[j]} in (
%?%             ry[p] = -1 \/ ry[q] = -1 \/ ry[p] = ry[q] :: domain
%?%  	)
%?%     );

%?% adjacencies: (out) is connected iff some adjacent (in) is connected
%?% constraint
%?%     forall(j in index_set(adj_from)) (
%?%         let {int: p = adj_from[j]} in (
%?% 	    ry[p] != -1 <-> exists(k in index_set(adj_from) where adj_from[k] = p)(ry[adj_to[k]] != -1)
%?% 	)
%?%     );

%?% adjacencies: (in) is connected iff all adjacent (out) are connected
%?% constraint
%?%     forall(j in index_set(adj_to)) (
%?%         let {int: p = adj_to[j]} in (
%?% 	    ry[p] != -1 <-> forall(k in index_set(adj_to) where adj_to[k] = p)(ry[adj_from[k]] != -1)
%?% 	)
%?%     );

%% aligned/adist
constraint
    forall(j in index_set(aligned_use)) (
        let {int: p = aligned_use[j],
	     int: q = aligned_def[j],
	     int: pi = aligned_usei[j],
	     int: qi = aligned_defi[j],
	     int: offset = aligned_dist[j]} in (
	    i[operand_definer[p]] = pi /\ i[operand_definer[q]] = qi -> ry[p] + offset = ry[q] :: domain
	)
    );

%% disjoint live ranges
constraint
    forall(T in bb_temps) (
	sliced_diffn([r[t] | t in T], [ls[t] | t in T], [temp_width[t] | t in T], [ld[t] | t in T])
    );

%% precedences (with conditions added by presolver)
constraint
    forall(i in index_set(precedence_pred)) (
          let {int: pred = precedence_pred[i],
	       int: succ = precedence_succ[i],
	       int: dist = precedence_diff[i],
	       set of conjunction: cond = precedence_cond[i]} in (
	      eval_disjunction(cond) -> c[pred] + dist <= c[succ]
	  )
    );

%% ensure that all uses are within the live range
constraint
    forall(p in 0..MAXP where operand_use[p])
          (let {int: iuse = operand_definer[p],
	        set of int: temps = operand_temps[p]} in (
	           forall(v in temps where v >= 0)(
		       let {int: idef = temp_definer[v]} in (
		           if card(temps) = 1 then
		               c[idef] + ld[v] >= c[iuse]
		           else
		               y[p] = v -> c[idef] + ld[v] >= c[iuse]
		   	   endif
		       )
		   )
	   )
    );

%% slack and balancing

%% Model::global_operand
test global_operand(int: p) = op_type[operand_definer[p]] in {TYPE_IN,TYPE_OUT};

%% Model::slack
constraint
  forall(p in 0..MAXP)(if not global_operand(p) then s[p]=0 else true endif);

%% CompleteModel::post_slack_balancing_constraints
constraint
    forall(j in index_set(adj_from)) (
      s[adj_from[j]] + s[adj_to[j]] = 0
    );

%% Model::post_operand_latency_definition
constraint
  forall(o in 0..MAXO, p in op_operands[o])(
    let {array[int] of int: o_p_lat = [     lat_table[i,2*k]
    		      	   	      |     i in index_set_1of2(lat_table), k in 1..2
				      where lat_table[i,1] = o /\ lat_table[i,3] = p
				      ],
	 int: nrows = length(o_p_lat) div 2} in
      if forall(r in 1..nrows)(o_p_lat[2*r] = o_p_lat[2]) then
        lt[p] = o_p_lat[2]
      else
        table([i[o],lt[p]], array2d(1..nrows, 1..2, o_p_lat))
      endif
  );

MINL..MAXL: min_active_lat(int: p) =
    min({     lat_table[i,4]
        |     i in index_set_1of2(lat_table)
	where lat_table[i,2] > 0 /\ lat_table[i,3] = p
	});

%% Model::post_temporary_use_latency_definition
var -MAXL..3*MAXL: lat(int: q, int: t) =
  let {int: p = temp_def[t]} in lt[p] + s[p] + lt[q] + s[q];

%% data precedences with slack and everything

%% Model::post_data_precedences_constraints
constraint
  forall(u in 0..MAXO, q in op_operands[u] where operand_use[q])(
    let {int: nalts = card(operand_temps[q]),
         var 1..nalts: key,
         array[1..nalts] of var int: lbs = if min(operand_temps[q]) = -1 then [-1] else [] endif
	 		    	     	   ++
	 		    	     	   [ c[d] + max(min_active_lat(p), lt[p]) + s[p] + lt[q] + s[q]
					   | t in real_temps(q),
					     p in {temp_def[t]},
					     d in {operand_definer[p]}
					   ]} in
      y[q] = operand_temps[q][key] /\
      c[u] >= lbs[key]
  );

%% Model::post_reverse_data_precedences_constraints
set of int: global_definers(int: q) = {pd |     t1 in real_temps(q),
                                                pd in {temp_def[t1]}
					  where global_operand(pd)
				      };
				     
var -MAXL..MAXL: slack_of_global_definer(int: q) =
  let {set of int: gds = global_definers(q)} in
    if card(gds)>0 then s[min(gds)] else 0 endif;


constraint
  forall(o in 0..MAXO, p in op_operands[o], v in operand_temps[p] where v >= 0 /\ not operand_use[p])(
    let {array[int] of var int: acs = [ if y[q] = v then c[j] - lat(q,v) - slack_of_global_definer(q) else 0 endif
    		       	   	      | q in temp_uses[v],
    		       	   	        j in {operand_definer[q]}
				      ]} in
      if r[v] >= 0 then
        c[o] <= max(acs)
      else true endif
  );





%% resources

test unit_res_dur_cap(int: r, set of int: active) = (
    forall(o in active)(
        fixedop(o) /\
	res_con[r,min(op_instructions[o])]=1 /\
	res_dur[r,min(op_instructions[o])]=1 /\
	res_off[r,min(op_instructions[o])]=0
    ) /\
    res_cap[r]=1
);

test unit_res_dur(int: r, int: o) = (
    forall(oi in op_instructions[o])(res_dur[r,oi]=1)
);

test zero_res_off(int: r, int: o) = (
    forall(oi in op_instructions[o])(res_off[r,oi]=0)
);

int: max_res_dur(int: r, int: o) = (
    max(oi in op_instructions[o])(res_dur[r,oi])
);

int: max_res_off(int: r, int: o) = (
    max(oi in op_instructions[o])(res_off[r,oi])
);

constraint
    forall(r in index_set(res_cap), b in 1..MAXB) (
        let {set of int: active = {o | o in bb_ops[b] where
	                               exists(q in op_instructions[o])
				             (res_con[r,q]>0)}} in (
            if card(active)<=1 then true
            else if unit_res_dur_cap(r,active) then
	        all_different([c[o] | o in active])
            else if good_cumulative then
		cumulative([ if zero_res_off(r,o) then c[o] else c[o]+res_off[r,insn(o)] endif
			   | o in active
			   ],
			   [ if unit_res_dur(r,o) then 1 else res_dur[r,insn(o)] endif
			   | o in active
			   ],
			   [ res_con[r,insn(o)]
			   | o in active
			   ],
			   res_cap[r]) :: domain
	    else let {int: inf = min([lb(c[o]) | o in active]),
	              int: sup = max([ub(c[o]) | o in active])
		               + max([max_res_dur(r,o) | o in active])
		               + max([max_res_off(r,o) | o in active])
			       } in (
		     forall(j in inf..sup)(
		         sum(o in active,
			     io in op_instructions[o],
			     k in j-res_dur[r,io]-res_off[r,io]+1..j-res_off[r,io]
			     where res_con[r,io]>0 /\ k in lb(c[o])..ub(c[o]))
			    (res_con[r,io]*bool2int(c[o]=k /\ i[o]=io)) <= res_cap[r]
		     )
		 )
            endif endif endif
	)
    );


%% packing constraints
constraint
    forall(k in index_set_1of2(packed_pq)) (
      let {int: p = packed_pq[k,1],
           int: q = packed_pq[k,2],
           set of int: insns = packed_is[k],
	   int: w = temp_width[single_temp(p)],
	   set of int: odd = {y | y in 0..MAXR where y mod (2*w) = w},
	   var int: rtq} in (
	  ite(ry[p] in odd, ry[p]-w, ry[p]+w, rtq) /\
	  (i[operand_definer[p]] in insns -> ry[q] = rtq)
      )
    );

%% activators
constraint
  forall(x in index_set(activator_insns))(
    let {set of int: is = activator_insns[x],
         set of int: os = activator_ops[x]} in (
	(a[min(os)] <-> exists(o in 0..MAXO)(i[o] in is)) /\
	forall(i2 in os where i2>min(os))(a[i2] <-> a[min(os)])
    )
  );

%% IMPLIED CONSTRAINTS

%% effective copy, handling multiple defs and/or uses
constraint
    forall(o in 0..MAXO where op_type[o] = TYPE_COPY)(
        let {int: src = min([p | p in op_operands[o] where operand_use[p]]),
	     int: dst = min([p | p in op_operands[o] where not operand_use[p]])} in
          a[o] -> ry[src] != ry[dst]
    );

%% dominating copies
constraint
    forall(k in index_set(dominate_ing)) (
        let {int: ing = dominate_ing[k],
	     int: ed = dominate_ed[k],
	     int: edsrc = min(op_operands[ed]),
	     set of int: iset = dominate_instructions[k],
	     set of int: tset = dominate_temps[k]} in (
	    a[ing] \/ not a[ed] \/ i[ed] in iset \/ y[edsrc] in tset
 	)
    );

%% disjoint registers
constraint
    forall(R in diffreg) (
        noverlap([ry[j] | j in R],
	         [temp_width[min(operand_temps[j])] | j in R])
    );

%% different temporaries
constraint
    forall(T in difftemp) (
        all_different([y[j] | j in T])
    );

%% nogoods
constraint
    forall(N in nogood) (
        not eval_conjunction(N)
    );

%% live temporary
bool: hairy_live_temporary = false;

constraint
  if hairy_live_temporary then
    forall(T in related_temps) (
        let {set of int: tset = T, % set of copy-related temps
	     set of int: irange = 1..card(tset),
	     array[irange] of var 0..card(tset): count,
	     array[irange] of int: tarr = [x | x in tset]} in (
	    forall(j in irange)(count[j] >= bool2int(a[temp_definer[tarr[j]]])) /\
	    global_cardinality([y[p] | p in 0..MAXP where can_use(p,tarr[1])],
		               tarr, count) :: domain
 	)
    )
  else
    forall(p in 0..MAXP where not operand_use[p])(
        let {int: v = single_temp(p)} in (
            y[p]>=0 <-> member([y[q] | q in 0..MAXP where can_use(q,v)], v)
        )
    )
  endif;

%% last use
constraint
    forall(p in 0..MAXP where operand_use[p])
          (y[p] != -1 -> le[y[p]] >= c[operand_definer[p]]) % rematerialization
    ;

%% disjoint live ranges projected on callee-saved regs
constraint
    if good_cumulative then
	forall(T in bb_temps) (
	    cumulative([ls[t] | t in T],
		       [ld[t] | t in T],
		       [temp_width[t]*bool2int(r[t] in calleesaved) | t in T],
		       card(calleesaved)) :: domain
	)
    else true endif;

%% temp symmetry breaking among interchangeable temporaries in their potential use operands
constraint
    forall(j in index_set(domop_operands)) (
        value_precede_chain(domop_temps[j], [y[o] | o in domop_operands[j]])
    );

%% mandatory temporary reuse (hairy!)
constraint
    forall(j in index_set(domuse_p))(
          let {int: P = domuse_p[j], % use operand
	       int: Q = domuse_q[j], % use operand
	       int: R = domuse_r[j], % def operand
	       int: T = min(operand_temps[R]),
	       int: I = operand_definer[P]} in (
	      if not (op_type[I] in {TYPE_COMBINE,TYPE_LOW,TYPE_HIGH})
	         % and not operand_lastuse[P] % guaranteed by model2dzn
		 % and forall(j in index_set(preassign_reg))(preassign_operand[j]!=P) % guaranteed by model2dzn
	      then
		  forall(atm in index_set(atom_regs))(
		      let {set of int: insns = {ii | ii in op_instructions[I] where ii>0 /\ operand_atom[P,ii]=atm}} in (
		          if (card(insns)>=1) then
			      (y[Q]=T /\ i[I] in insns /\ r[T] in atom_regs[atm] -> y[P]=T)
			  else true endif
		      )
		  )
	      else true endif
	  )
    );

%% before
constraint
    forall(i in index_set(before_pred)) (
        let {int: p = before_pred[i],
	     int: q = before_succ[i],
	     set of int: cond = before_cond[i]} in (
            eval_disjunction(cond) -> le[y[p]] <= ls[y[q]]
	)
    );

%% active_table, tmp_table
constraint
    forall(E in table_exists_ops) (
        exists(j in E)(a[j])
    );

constraint
    forall(XOR in table_iffall_ops) (
        let {int: j = min(XOR),
	     int: k = max(XOR)} in (
	    a[j] <-> a[k]
	)
    );

constraint
    forall(i in index_set(relation_ops)) (
        let{set of int: ops = relation_ops[i],
	    set of int: temps = relation_temps[i],
	    int: nt = relation_ntuples[i],
	    set of int: range = relation_range[i],
	    set of int: irange = 1..(card(ops)+card(temps)),
	    array[irange] of var int: vars = [bool2int(a[j]) | j in ops]++
	                                     [y[j] | j in temps]} in (
	   table(vars, array2d(1..nt,irange,[ints[j] | j in range]))
	)
    );

%% pack operations cannot be the only users of their free temporaries
%% see Model::post_pack_sink_constraints()
constraint
    forall(v in 0..MAXT where op_type[temp_definer[v]]==TYPE_COPY)(
        let {bool: used_by_pack = exists(p in temp_uses[v])(op_type[operand_definer[p]]==TYPE_PACK),
	     set of int: uses_but_pack = {p | p in temp_uses[v] where op_type[operand_definer[p]]!=TYPE_PACK}} in
	    if used_by_pack /\ card(uses_but_pack)>0 then
	        y[temp_def[v]] >= 0 <-> exists(p in uses_but_pack)(y[p]=v)
	    else true endif
    );

%% combine
constraint
    forall(o in 0..MAXO where op_type[o] = TYPE_COMBINE)(
	let {array[int] of int: opnds = [x | x in op_operands[o]],
	     int: src1 = opnds[1],
	     int: src2 = opnds[2],
	     int: def  = opnds[3]} in (
	    minimum(ls[y[def]], [le[y[src1]], le[y[def]]]) /\
	    minimum(ls[y[def]], [le[y[src2]], le[y[def]]])
	)
    );

%% low/high
constraint
    forall(o in 0..MAXO where op_type[o] in {TYPE_HIGH, TYPE_LOW})(
	let {int: src  = min(op_operands[o]),
	     int: def  = max(op_operands[o])} in (
	    minimum(ls[y[def]], [le[y[src]], le[y[def]]])
	)
    );

%% useless memory stores
constraint
    forall(T in related_temps) (
	sum([bool2int(r[v] in infinite) | v in T]) <= 1
    );

%% across
% this version seems marginally better
constraint
    forall(i in index_set(across_op)) (
	let {int: opi = across_op[i],
	     int: maxwidth = max(temp_width),
             set of int: regs = across_regs[i],
             set of int: items = across_items[i],
	     array[items] of int: cand =
	         array1d(items, [across_item_temp[j] | j in items]),
	     array[items] of var bool: active,
	     array[items] of var -maxwidth*(MAXT+1)..MAXR: selected,
	     array[int] of var int: allreg =
	         [x | x in callersaved] ++ [x | x in regs] ++ selected,
	     array[int] of int: allwidth =
	         [1 | x in callersaved] ++
	         [1 | x in regs] ++
		 [temp_width[cand[j]] | j in items]} in (
	    forall(j in items)(
	        let {int: cj = cand[j]} in (
		    (   active[j] <->
			    eval_disjunction(across_item_cond[j]) \/
			    (ls[cj] <= c[opi] /\ c[opi] < le[cj])) /\
		    ite(active[j], r[cj], -maxwidth*(cj+1), selected[j])
		)
	    ) /\
	    noverlap(allreg, allwidth)
	)
    );

% this version seems marginally worse
% constraint
%     forall(i in index_set(across_op)) (
% 	let {int: opi = across_op[i],
% 	     int: maxwidth = max(temp_width),
%              set of int: regs = across_regs[i],
%              set of int: items = across_items[i],
% 	     array[items] of int: cand =
% 	         array1d(items, [across_item_temp[j] | j in items]),
% 	     array[items] of var bool: active,
% 	     array[items] of var -1..MAXR: selected,
% 	     array[int] of var int: allreg =
% 	         [x | x in callersaved] ++ [x | x in regs] ++ selected,
% 	     array[int] of var int: allwidth =
% 	         [1 | x in callersaved] ++
% 	         [1 | x in regs] ++
% 		 [temp_width[cand[j]]*bool2int(active[j]) | j in items]} in (
% 	    forall(j in items)(
% 	        let {int: cj = cand[j]} in (
% 		    (   active[j] <->
% 			    eval_disjunction(across_item_cond[j]) \/
% 			    (ls[cj] <= c[opi] /\ c[opi] < le[cj])) /\
% 		    selected[j] = r[cj]
% 		)
% 	    ) /\
% 	    noverlap(allreg, allwidth)
% 	)
%     );

%% set_across
constraint
    forall(i in index_set(setacross_op)) (
	let {int: opi = setacross_op[i],
             set of int: regs = setacross_regs[i],
             set of int: itemsset = setacross_tempsets[i],
	     set of int: items = 1..card(itemsset),
	     array[items] of set of int: cands = [sets[j] | j in itemsset],
	     array[items] of var 0..MAXR: selected,
	     array[items] of var 0..max(temp_width): width,
	     array[int] of var int: allreg =
	         [x | x in callersaved] ++ [x | x in regs] ++ selected,
	     array[int] of var int: allwidth =
	         [1 | x in callersaved] ++ [1 | x in regs] ++ width} in (
	    forall(j in items)(
	        let {set of int: cj = cands[j],
		     array[int] of int: ws  = [temp_width[k] | k in cj],
		     array[int] of var int: rs  = [r[k] | k in cj],
		     array[int] of var int: lss = [ls[k] | k in cj],
		     array[int] of var int: les = [le[k] | k in cj],
		     var 1..card(cj): k} in (
		  width[j] = ws[k] /\
		  selected[j] = rs[k] /\
		  lss[k] <= c[opi] /\ c[opi] < les[k]
		)
	    ) /\
	    noverlap(allreg, allwidth)
	)
    );

%% definer's reg must be user's
%% hasn't yet shown any effect
% constraint
%     forall(v in index_set(temp_uses)) (
%         let {set of int: tuv = temp_uses[v]} in (
%             if card(tuv) > 1 then
% 	        let {array[1..card(tuv)] of var int: rs,
% 		     array[int] of int: tuva = [x | x in tuv]} in (
% 		    forall(i in index_set(rs)) (
% 		        ite(y[tuva[i]]=v, ry[tuva[i]], -1, rs[i])
% 		    ) /\
% 	    	    member(rs, r[v])
% 		)
% 	    else true endif
% 	)
%     );

%% spill callee-saved if and only if some temp interfers with it
constraint
    if card(index_set(calleesaved_spill))>0 then
	let {int: nrows = card(index_set_1of2(cs_spill_transpose)),
	     int: ncols = card(index_set_2of2(cs_spill_transpose)),
             set of int: css_ops = {cs_spill_transpose[i,j] | i in 1..nrows, j in 1..ncols},
             set of int: preass_ops = {y | y in 0..MAXO where op_type[y] in {TYPE_IN,TYPE_CALL,TYPE_TAIL,TYPE_FUN}},
	     set of int: tcands = {y | y in 0..MAXT where not (temp_definer[y] in css_ops union preass_ops)}} in (
	    forall(j in 1..ncols) (
	        let {int: theop = cs_spill_transpose[1,j],
		     int: thepu = min(op_operands[theop]),
		     int: thetu = single_temp(thepu),
		     int: thepd = temp_def[thetu],
		     int: thea = min(k in index_set(preassign_reg) where preassign_operand[k]=thepd)(preassign_reg[k]),
		     int: thew = temp_width[thetu]} in (
		    % trace("a[\(theop)] <-> exists(ti in \(tcands))(r[ti]+temp_width[ti] > \(thea) && r[ti] in 0..\(thea+thew-1))\n",true)
		    a[theop] <-> exists(ti in tcands)(r[ti]+temp_width[ti] > thea /\ r[ti] in 0..thea+thew-1)
		)
	    )
	)
    else true endif;

%% break symmetries among which callee-saved to spill
constraint
    if card(index_set(calleesaved_spill))>0 then
	forall(i in index_set(calleesaved_spill)) (
	    let {set of int: cgr = calleesaved_spill[i],
		 int: cur = min(cgr)} in (
		forall(x in cgr where x>cur) (a[cur] <-> a[x])
	    )
	) /\
	let {int: nrows = card(index_set_1of2(cs_spill_transpose)),
	     int: ncols = card(index_set_2of2(cs_spill_transpose)),
	     set of int: spill = if nrows>=1 then {cs_spill_transpose[1,j] | j in 1..ncols}
				 else {} endif,
	     set of int: unspill = if nrows>=2 then {cs_spill_transpose[2,j] | j in 1..ncols}
				   else {} endif} in (
	    forall(i in 1..nrows)(
		decreasing([a[cs_spill_transpose[i,j]] | j in 1..ncols]) /\
		forall(j in 2..ncols)(
		  if i=1 then % spill
		    a[cs_spill_transpose[i,j]] -> c[cs_spill_transpose[i,j-1]] <= c[cs_spill_transpose[i,j]]
		  else % unspill
		    a[cs_spill_transpose[i,j]] -> c[cs_spill_transpose[i,j-1]] >= c[cs_spill_transpose[i,j]]
		  endif
		)
	    ) /\
	    if spill subset bb_ops[1] /\ unspill subset bb_ops[1] then
		forall(j in 1..ncols)(
		    let {int: u = cs_spill_transpose[2,j],
			 int: s = cs_spill_transpose[1,j]} in (
			a[u] -> c[s] < c[u]
		    )
		)
	    else true endif
	)
    else true endif;

%% symmetry breaking from JSON info
constraint
  forall(x in index_set(value_precede_min))(
    let {set of int: js = value_precede_min[x]..value_precede_max[x],
         int: nt = sum(j in js)(temp_width[value_precede_temps[j]]),
         array[1..nt] of var int: chain = [r[t]+d | j in js, t in {value_precede_temps[j]}, d in 0..temp_width[t]-1]} in
	value_precede_chain([v | v in value_precede_regs[x]], chain)
  );

%% predecessors -- DISABLED
constraint
  forall(x in index_set(predecessors_preds))(
    min([c[y] | y in predecessors_preds[x]]) + predecessors_lat[x] <= c[predecessors_succ[x]]
  );

%% successors -- DISABLED
constraint
  forall(x in index_set(successors_succs))(
     c[successors_pred[x]] + successors_lat[x] <= max([c[y] | y in successors_succs[x]])
  );

%% instr_cond
constraint
  forall(x in index_set_1of2(instr_cond))(
    let {int: ox = instr_cond[x,1],
         int: ix = instr_cond[x,2],
         int: px = instr_cond[x,3]} in (
      i[ox] = ix -> ls[y[px]] = c[ox]
    )
  );

%% IMPORTED FROM Model::

%% live range length
% constraint			%  rematerialization
%     forall(v in 0..MAXT where op_type[temp_definer[v]] != TYPE_IN)
%           (a[temp_definer[v]] -> ld[v] >= temp_minlive[v]);

%% Model::post_minimum_temporary_duration_constraints
constraint
  forall(o in 0..MAXO, dp in op_operands[o] where not operand_use[dp])(
    let {int: t = single_temp(dp)} in
	(y[dp]=t -> ld[t] >= temp_minlive[t]) /\
	forall(up in temp_uses[t])(y[up]=t -> ld[t] >= lat(up,t))
  );
        
%% Model::post_define_issue_cycle_constraints
constraint
  forall(o in 0..MAXO where op_type[o]=TYPE_DEFINE)(
    let {set of int: js = {operand_definer[q] | p in op_operands[o],
	                                        t in {single_temp(p)},
    	     	     	                        q in temp_uses[t]},
         array[int] of var int: lats = [lat(q,t) | p in op_operands[o],
	                                           t in {single_temp(p)},
    	     	     	                           q in temp_uses[t]]} in
      if card(js) = 1 then
        c[min(js)] = c[o] + max(lats)
      else true endif
  );
        
%% Model::post_kill_pack_issue_cycle_constraints
constraint
  forall(o2 in 0..MAXO where op_type[o2] in {TYPE_KILL,TYPE_PACK})(
    let {set of int: cards = {card(real_temps(q)) | q in op_operands[o2]},
	 array[int] of var int: lats = [lat(q,t) | q in op_operands[o2],
						   t in {single_temp(q)}],
	 set of int: o1s = {temp_definer[t] | q in op_operands[o2],
					      t in {single_temp(q)}}} in
      if cards = {1} then
        c[o2] = c[min(o1s)] + max(lats)
      else true endif
  );

%% Model::post_slack_lower_bound_constraints
constraint
  forall(C in congr)(
    let {array[int] of int: ins  = [p | p in C where op_type[operand_definer[p]] = TYPE_IN],
         array[int] of int: outs = [p | p in C where op_type[operand_definer[p]] = TYPE_OUT],
         array[int] of var int: inlats = [lt[q] | p in ins,
	 	       	    	   	          q in temp_uses[single_temp(p)]],
         array[int] of var int: outlats = [lt[q] | p in outs,
	 	       	    	   	           t in real_temps(p),
	 	       	    	   	           q in {temp_def[t]}]} in
      if (length(ins)>0) then
        let {var int: maxinlat = max(inlats)} in
          forall(p in ins)(s[p] >= - maxinlat)
      else true endif /\
      if (length(outs)>0) then
        let {var int: maxoutlat = max(outlats)} in
          forall(p in outs)(s[p] >= - maxoutlat)
      else true endif
  );


%% output

output
    ["\"objective\": "]   ++ [show(obj)] ++ ["\n"] ++
    ["\"cycles\": "]      ++ [show(c)]   ++ ["\n"] ++
    ["\"instructions\": "]  ++ [show(i)]   ++ ["\n"] ++
    ["\"temporaries\": "] ++ [show(y)]   ++ ["\n"] ++
    ["\"registers\": "]   ++ [show([r[t] | t in 0..MAXT])] ++ ["\n"] ++
    ["\"live start\": "]   ++ [show([ls[t] | t in 0..MAXT])] ++ ["\n"] ++
    ["\"live duration\": "]   ++ [show([ld[t] | t in 0..MAXT])] ++ ["\n"] ++
    ["\"live end\": "]   ++ [show([le[t] | t in 0..MAXT])] ++ ["\n"];

