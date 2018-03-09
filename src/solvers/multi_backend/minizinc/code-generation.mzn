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

include "globals.mzn";

%% solver parameters

bool: good_cumulative;
bool: good_diffn;
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
int: TYPE_LOW     = 9;
int: TYPE_HIGH    = 10;
int: TYPE_SPLIT2  = 11;
int: TYPE_SPLIT4  = 12;
int: TYPE_FUN     = 13;
int: TYPE_COPY    = 14;

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
int: MAXINSNS = max(o in 0..MAXO)(card(op_instructions[o]));
int: MAXTEMPS = max(p in 0..MAXP)(card(operand_temps[p]));

set of int: expr = index_set(expr_op);

%% see JSON freq, maxc, ins, tmp, operands, optional_min
array[int] of int: bb_order;
array[int] of set of int: bb_ops;
array[int] of set of int: bb_operands;
array[int] of set of int: bb_temps;
array[int] of set of int: bb_subsumed;
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

%% see JSON calleesaved, callersaved, infinite, bounded, range
set of int: calleesaved;
set of int: callersaved;
set of int: infinite;
array[int] of bool: bounded;
array[int] of set of int: range;

%% see JSON cap, con, dur, off
array[int] of int: res_cap;
array[int,int] of int: res_con;
array[int,int] of int: res_dur;
array[int,int] of int: res_off;

%% see JSON congr
array[int] of set of int: congr;

%% see JSON strictly_congr
array[int] of set of int: strictly_congr;

%% see JSON preassign
array[int] of int: preassign_operand;
array[int] of int: preassign_reg;

%% see JSON aligned/adist
array[int] of int: aligned_use;
array[int] of int: aligned_def;
array[int] of int: aligned_usei;
array[int] of int: aligned_defi;
array[int] of int: aligned_dist;

%% see JSON adjacent
array[int] of int: adj_from;
array[int] of int: adj_to;

%% see JSON quasi_adjacent
array[int] of int: quasi_adj_from;
array[int] of int: quasi_adj_to;

%% see JSON long_latency
array[int,int] of set of int: long_latency_index;
array[int] of int: long_latency_def;
array[int] of int: long_latency_use;

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

%% see JSON definer, temps, width, minlive
array[int] of int: temp_definer; % temp -> defining op
array[int] of int: temp_def;     % temp -> defining operand
array[int] of int: temp_width;   % temp -> width
array[int] of int: temp_minlive; % temp -> minlive
array[int] of set of int: temp_uses; % temp -> set of use operands

%% see JSON packed, pinstrs
array[int,int] of int: packed_pq;

%% see JSON before
array[int] of int: before_pred;
array[int] of int: before_succ;
array[int] of expr: before_cond;

%% see JSON E
array[int] of int: adhoc;

%% see JSON nogoods
array[int] of expr: nogood;

%% see JSON across
array[int] of int: across_op;
array[int] of set of int: across_regs;
array[int] of set of int: across_items;
array[int] of int: across_item_temp;
array[int] of expr: across_item_cond;

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

%% see JSON infassign
array[int,int] of int: infassign;

%% see JSON space
array[int] of int: space;

%% see JSON dominates
array[int] of int: dominate_ing;
array[int] of int: dominate_ed;
array[int] of set of int: dominate_instructions;
array[int] of set of int: dominate_temps;

%% see JSON precedences
array[int] of expr: precedence;

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

%% see JSON value_precede_chain
array[int] of int: value_precede_min;
array[int] of int: value_precede_max;
array[int] of set of int: value_precede_regs;
array[int] of int: value_precede_temps;

%% see JSON lat
array[int,int] of int: lat_table;

%% see JSON preschedule
array[int] of int: preschedule_op;
array[int] of int: preschedule_cycle;

%% see JSON bypass
array[int,int] of int: bypass_table;

%% see JSON exrelated, table
array[int] of int: exrelated_p;
array[int] of int: exrelated_q;
array[int] of set of int: exrelated_rows;
array[int,int] of int: exrelated_ext;

%% extra storage for constraint expressions
array[int] of int: expr_op;
array[int] of int: expr_arg1;
array[int] of int: expr_arg2;
array[int] of int: expr_arg3;
array[int] of set of int: expr_children;

%% extra store for relations in table constraints
array[int] of int: ints;

%% extra storage: each set is a set of temporaries
array[int] of set of int: sets;

%% problem variables

var 0..MAXF: obj;
array[0..MAXO] of var bool: a;
array[0..MAXO] of var 1..MAXINSNS: ii; % same as in gecode-solver
array[0..MAXO] of var -1..MAXC: c;
array[0..MAXP] of var -1..MAXR: rt;
array[0..MAXP] of var MINL..MAXL: lt; % operand latency
array[0..MAXP] of var -MAXL..MAXL: s; % global operand slack
array[0..MAXP] of var 1..MAXTEMPS: y; % same as in gecode-solver
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

test all_equal_ints(array[int] of int: x) =
	forall(i, j in index_set(x) where i < j) ( x[i] = x[j] );

var int: res_con_of_op(int: res, int: o) =
  let {array[int] of int: I = op_instructions[o]} in
  let {array[int] of int: ints = [res_con[res,I[i]] | i in index_set(I)]} in
  if all_equal_ints(ints) then ints[1] else ints[ii[o]] endif;

var int: res_dur_of_op(int: res, int: o) =
  let {array[int] of int: I = op_instructions[o]} in
  let {array[int] of int: ints = [res_dur[res,I[i]] | i in index_set(I)]} in
  if all_equal_ints(ints) then ints[1] else ints[ii[o]] endif;

var int: res_off_of_op(int: res, int: o) =
  let {array[int] of int: I = op_instructions[o]} in
  let {array[int] of int: ints = [res_off[res,I[i]] | i in index_set(I)]} in
  if all_equal_ints(ints) then ints[1] else ints[ii[o]] endif;

predicate insn_eq(int: o, int: x) =
  let {array[int] of int: I = op_instructions[o]} in
  exists(j in index_set(I) where I[j]=x)(ii[o]=j);

predicate insn_in(int: o, set of int: X) =
  if card(X)=0 then
    false
  else if card(X)=1 then
    insn_eq(o, min(X))
  else
    let {array[int] of int: I = op_instructions[o]} in
    ii[o] in {j | j in index_set(I) where I[j] in X}
  endif endif;

predicate smart_alldiff(array[int] of var int: X) =
  if length(X)<2 then
    true
  else if length(X)=2 then
    let {set of int: S = index_set(X)} in
    X[min(S)] != X[max(S)]
  else
    all_different(X)
  endif endif;

predicate ra_diffn_cumu(array[int] of var int: R,
			array[int] of int: W,
			array[int] of var int: LS,
			array[int] of var int: LD,
			int: v) =
      let {set of int: J = {i | i in index_set(R) where card(dom(R[i]) intersect max(0,v-W[i]+1)..v)>0}}
      in cumulative([LS[i] | i in J], [LD[i] | i in J],
                    [if W[i]=1 then R[i]=v else R[i] in max(0,v-W[i]+1)..v endif | i in J],
                    1);

predicate ra_diffn(array[int] of var int: R,
                   array[int] of int: W,
                   array[int] of var int: LS,
                   array[int] of var int: LD,
                   array[int] of var int: LE) =
  if good_cumulative then % [MC] infinite ranges are clash free by construction
    forall(v in 0..min(infinite)-1)(
      ra_diffn_cumu(R, W, LS, LD, v)
    ) /\
    forall(j in index_set(bounded), v in range[j] where bounded[j])(
      ra_diffn_cumu(R, W, LS, LD, v)
    )
  else if good_diffn then
    diffn_nonstrict(R, LS, W, LD)
  else
    forall(i,j in index_set(R) where i < j)(
        LD[i] = 0 \/ LD[j] = 0 \/
        R[i] + W[i] <= R[j] \/ LE[i] <= LS[j] \/
        R[j] + W[j] <= R[i] \/ LE[j] <= LS[i]
    )
  endif endif ;

predicate disjoint1(array[int] of var int: x,
                      array[int] of var int: w) =
  diffn(x, [1 | i in index_set(x)], w, [1 | i in index_set(x)]) ;

predicate noverlap(array[int] of var int: x,
                   array[int] of var int: w) = (
	if min([lb(z) | z in w]) < max([ub(z) | z in w]) then
	    disjoint1(x, w)
	else if length(x) = 2 then
	    x[1] != x[2]
	else
	    smart_alldiff(x)
	endif endif
    );

predicate live_overlap(int: k, int: j) = (ls[k] < le[j] /\ ls[j] < le[k]);

predicate operand_live_overlap(int: p, int: q) = (ls_t(p) < le_t(q) /\ ls_t(q) < le_t(p));

predicate operand_live_before(int: p, int: q) = (le_t(p) <= ls_t(q));

test operand_related(int: p, int: q) =
  operand_temps[p] subset operand_temps[q] \/ operand_temps[q] subset operand_temps[p];

predicate same_temp(int: p, int: q) =
  assert(operand_related(p,q), "same_temp[\(p),\(q)] with unrelated operands") /\
  y[p] = y[q];
  % let {array[int] of int: ts = operand_temps[p] intersect operand_temps[q]} in
  % exists(t in ts)(temp_eq(p,t) /\ temp_eq(q,t));

predicate temp_eq(int: p, int: t) =
  let {array[int] of int: ts = operand_temps[p]} in
  exists(j in index_set(ts) where ts[j]=t)(y[p]=j);

predicate temp_in(int: p, set of int: T) =
  if card(T)=0 then
    false
  else if card(T)=1 then
    temp_eq(p, min(T))
  else
    let {array[int] of int: ts = operand_temps[p]} in
    y[p] in {j | j in index_set(ts) where ts[j] in T}
  endif endif;

test temp_is_unsafe(int: t) =
  if op_type[temp_definer[t]] in {TYPE_IN,TYPE_DEFINE,TYPE_FUN} then
    false
  else
    let {array[int] of int: dlats = [lat_table[j,4] | j in index_set_1of2(lat_table)
						    where
						      lat_table[j,2] != 0 /\
						      lat_table[j,3] = temp_def[t]
						    ],
	 array[int] of int: ulats = [lat_table[j,4] | j in index_set_1of2(lat_table)
						    where
						      lat_table[j,2] != 0 /\
						      lat_table[j,3] in temp_uses[t]
						    ]} in
    temp_minlive[t] > min(dlats) + min(ulats)
  endif;

test temp_must_be_real(int: p) =
  min(operand_temps[p])>=0;

predicate temp_is_real(int: p) =
  if min(operand_temps[p])>=0 then
    true
  else
    y[p]!=1
  endif;

predicate temp_is_real_reif(int: p, var bool: re) =
  if min(operand_temps[p])>=0 then
    re
  else
    re <-> y[p]!=1
  endif;

predicate insn_is_real_reif(int: o, var bool: re) =
  if min(op_instructions[o])>=1 then
    re
  else
    re <-> ii[o]!=1
  endif;

predicate temp_member_reif(set of int: P, int: t, var bool: re) =
  (re <-> exists(p in P)(temp_eq(p,t)));

var int: t(int: p) =
  if card(operand_temps[p])=1 then min(operand_temps[p])
  else if card(operand_temps[p])=2 then
    let {int: a = min(operand_temps[p]),
         int: b = max(operand_temps[p])}
    in (b-a)*y[p] -b+a+a
  else
    operand_temps[p][y[p]]
  endif endif;

var int: ls_t(int: p) = [ls[t] | t in operand_temps[p]][y[p]];

var int: le_t(int: p) = [le[t] | t in operand_temps[p]][y[p]];

%% conditions
predicate eval_expr(int: l) = (
    let {int: tag = expr_op[l],
         int: arg1 = expr_arg1[l],
         int: arg2 = expr_arg2[l],
         int: arg3 = expr_arg3[l],
	 } in (
	     if tag=0 then exists(j in expr_children[l])(eval_expr(j))
	else if tag=1 then forall(j in expr_children[l])(eval_expr(j))
	else if tag=2 then eval_expr(arg1) xor eval_expr(arg2)
	else if tag=3 then eval_expr(arg1) -> eval_expr(arg2)
	else if tag=4 then not eval_expr(arg1)
	else if tag=5 then a[arg1]
	else if tag=6 then temp_eq(arg1,arg2)
	else if tag=7 then insn_eq(arg1,arg2)
	else if tag=8 then c[arg2] >= c[arg1] + arg3
	else if tag=9 then same_temp(arg1,arg2)
	else if tag=10 then operand_live_overlap(arg1,arg2)
	else if tag=11 then live_overlap(arg1,arg2)
	else if tag=12 then r[arg1] in callersaved
	else if tag=13 then rt[arg1] in atom_regs[arg2]
        else false endif endif endif endif endif endif endif endif endif endif endif endif endif endif
    )
);

var int: ite(var bool: cond, var int: thenval, var int: elseval) = (
         let {array[1..2] of var int: ar = [elseval,thenval]} in
         ar[cond+1]
);

0..MAXT: single_temp(int: p) = max(operand_temps[p]);

set of 0..MAXT: real_temps(int: p) = operand_temps[p] intersect 0..MAXT;

%% (mostly) ESSENTIAL CONSTRAINT

%% basic invariants, null instructions

test opt_in_temporary(int: v) = 
    let {int: p = temp_def[v]} in
    op_type[temp_definer[v]] = TYPE_IN /\ card(operand_temps[p])>1;

constraint
    r[-1] = -1 /\
    ls[-1] = -1 /\
    ld[-1] = 0 /\
    le[-1] = -1 /\
    forall(o in 0..MAXO where op_mand[o]) (a[o]) /\
    forall(o in 0..MAXO where not op_mand[o]) (insn_is_real_reif(o,a[o])) /\
    forall(o in 0..MAXO) (a[o] <-> c[o] >= 0) /\
    forall(p in 0..MAXP where card(operand_temps[p])>1) (rt[p] = [r[v] | v in operand_temps[p]][y[p]]) /\
    forall(p in 0..MAXP where card(operand_temps[p])=1) (rt[p] = r[single_temp(p)]) /\
    forall(p in 0..MAXP where not op_mand[operand_definer[p]]) % rematerialization
          (temp_is_real_reif(p,a[operand_definer[p]])) /\
    forall(v in 0..MAXT) (ls[v]+ld[v] = le[v]) /\
    forall(v in 0..MAXT where not opt_in_temporary(v)) % rematerialization
          (ls[v] = c[temp_definer[v]] /\
	   (a[temp_definer[v]] <-> r[v] >= 0)) /\
    forall(v in 0..MAXT where opt_in_temporary(v)) % rematerialization
          (ls[v] in -1 .. 0 /\
	   (r[v] >= 0 <-> ls[v]=0) /\
	   (r[v] >= 0 <-> temp_eq(temp_def[v],v))) /\
    forall(v in 0..MAXT) (
	let {bool: unsafe = temp_is_unsafe(v),
	     set of int: pset = {p | p in 0..MAXP where can_use(p,v)},
	     set of int: prange = 1..card(pset),
	     array[prange] of int: parr = [p | p in pset],
	     array[prange] of var -1..MAXC: carr} in (
	    forall(p in prange) (carr[p] = ite(temp_eq(parr[p],v), c[operand_definer[parr[p]]], -1)) /\
	    if card(pset)=1 /\ not unsafe then
	      le[v] = carr[1]
	    else if not unsafe then
	      maximum(le[v], carr)
	    else
	      maximum(le[v], carr++[ls[v]+ % 20130930: live range can extend past last use
				    temp_minlive[v]*bool2int(a[temp_definer[v]])])
	    endif endif
	)
    );

%% objective, bounds on (out) cycle

constraint
    forall(b in 1..MAXB) (
	forall(o in bb_ops[b])(c[o] in -1..bb_maxcycle[b]) /\
	c[min(bb_ops[b])] = 0 /\
	copysum[b] = sum(o in bb_ops[b] where not op_mand[o])(bool2int(a[o])) /\
	copysum[b] >= bb_optional_min[b]
    ) /\
    if optimize_cycles then
      obj = sum(b in 1..MAXB) (bb_frequency[b] * c[max(bb_ops[b])])
    else
      obj = sum(o in 0..MAXO)(res_con_of_op(1,o))
    endif;

%% domains
constraint
    forall(o in 0..MAXO) (ii[o] in 1..card(op_instructions[o]));

constraint
    forall(q in 0..MAXP) (y[q] in 1..card(operand_temps[q]));

set of int: temp_insn_regs(int: v, int: j) =
    let {int: p = temp_def[v],
         int: w = temp_width[v],
         int: oa = operand_atom[p,j],
	 set of int: ks = {k | k in index_set_1of2(infassign) where infassign[k,1]=v /\ infassign[k,2]=space[oa]},
	 set of int: unaligned =
	   if card(ks)>0 then
	     {r | r in atom_regs[oa]} intersect (infassign[min(ks),3]..infassign[min(ks),4])
	   else
	     {r | r in atom_regs[oa]}
	   endif}
    in {r | r in unaligned /*where r mod w = 0*/}; % currently, that would over-constrain

constraint
    forall(v in 0..MAXT) (
        let {int: p = temp_def[v],
	     int: w = temp_width[v],
	     set of int: rdom = array_union([temp_insn_regs(v,j) | j in 1..MAXI]),
    	     set of int: joker = if min(operand_temps[p])>=0 then {} else {-1} endif, % rematerialization
	} in r[v] in rdom union joker
    );

%% operation selection (register set depending on operation)
constraint
    forall(p in 0..MAXP)(
      let {int: o = operand_definer[p],
	   set of int: joker = if min(operand_temps[p])>=0 then {} else {-1} endif} in
	  if fixedop(o) then
	    rt[p] in atom_regs[operand_atom[p,min(op_instructions[o])]] union joker % rematerialization
	  else
            let {array[int] of set of int: doms = [ if insn=0 then {-1} else atom_regs[operand_atom[p,insn]] endif
	    		       	      	   	  | insn in op_instructions[o]
						  ],
	         array[int] of int: extension = [ if k=1 then j else r endif
		 	       	    	        | j in index_set(op_instructions[o]),
						  r in doms[j],
						  k in 1..2
						],
		 int: nt = length(extension) div 2} in
	    if forall(i in index_set(doms))(doms[i] = doms[1]) then
	      rt[p] in doms[1]
	    else
	      table([ii[o],rt[p]], array2d(1..nt, 1..2, extension))
	    endif
	  endif
    );

%% preassignments
constraint
    forall(j in index_set(preassign_reg))
          (rt[preassign_operand[j]] = preassign_reg[j]);

%% congruences
constraint
    forall(cgr in strictly_congr) (
        let {int: junior = min(cgr)} in (
	    forall(j in cgr where j>junior) (rt[junior] = rt[j])
 	)
    );

%% adjacencies NEW FOR REMATERIALIZATION
constraint
    forall(     j in index_set(adj_from)
          where not({adj_from[j],adj_to[j]} subset array_union(strictly_congr))
          ) (
        let {int: p = adj_from[j],
	     int: q = adj_to[j]} in
            (temp_is_real(q) -> temp_is_real(p)) /\
            (temp_is_real(q) -> rt[p] = rt[q]) /\
            (temp_is_real(p) -> exists(k in index_set(adj_from) where adj_from[k] = p)(temp_is_real(adj_to[k])))
    );

constraint
    forall(j in index_set(quasi_adj_from)) (
        let {int: p = quasi_adj_from[j],
	     int: q = quasi_adj_to[j]} in
            temp_is_real(q) -> temp_is_real(p)
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
% 	       t[pout] in tsc -> t[pin] >= 0
% 	    else true endif
%     );

%?% constraint
%?%     forall(j in index_set(adj_from)) (
%?%         let {int: p = adj_from[j],
%?% 	     int: q = adj_to[j]} in (
%?%             rt[p] = -1 \/ rt[q] = -1 \/ rt[p] = rt[q]
%?%  	)
%?%     );

%?% adjacencies: (out) is connected iff some adjacent (in) is connected
%?% constraint
%?%     forall(j in index_set(adj_from)) (
%?%         let {int: p = adj_from[j]} in (
%?% 	    rt[p] != -1 <-> exists(k in index_set(adj_from) where adj_from[k] = p)(rt[adj_to[k]] != -1)
%?% 	)
%?%     );

%?% adjacencies: (in) is connected iff all adjacent (out) are connected
%?% constraint
%?%     forall(j in index_set(adj_to)) (
%?%         let {int: p = adj_to[j]} in (
%?% 	    rt[p] != -1 <-> forall(k in index_set(adj_to) where adj_to[k] = p)(rt[adj_from[k]] != -1)
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
	    insn_eq(operand_definer[p],pi) /\ insn_eq(operand_definer[q],qi) -> rt[p] + offset = rt[q]
	)
    );

%% disjoint live ranges
constraint
    forall(T in bb_temps) (
	ra_diffn([r[v] | v in T], [temp_width[v] | v in T], [ls[v] | v in T], [ld[v] | v in T], [le[v] | v in T])
    );

%% precedences (with conditions added by presolver)
constraint
    forall(P in precedence) (
        eval_expr(P)
    );

%% ensure that all uses are within the live range -- pretty much subsumed by maximum(le[v], ...)
% constraint
%     forall(p in 0..MAXP where operand_use[p])
%           (let {int: iuse = operand_definer[p],
% 	        set of int: temps = operand_temps[p]} in (
% 	           forall(v in temps where v >= 0)(
% 		       let {int: idef = temp_definer[v]} in (
% 		           if card(temps) = 1 then
% 		               c[idef] + ld[v] >= c[iuse]
% 		           else
% 		               t[p] = v -> c[idef] + ld[v] >= c[iuse]
% 		   	   endif
% 		       )
% 		   )
% 	   )
%     );

%% slack and balancing

%% Model::global_operand
test global_operand(int: p) = 
  exists(j in index_set(adj_from))(adj_from[j]=p) \/
  exists(j in index_set(adj_to))(adj_to[j]=p);

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
    let {array[int] of int: lcol = [     lat_table[j,4]
    		       	           |     j in index_set_1of2(lat_table)
				   where lat_table[j,1] = o /\ lat_table[j,3] = p
				   ],
         int: n = length(lcol),
	 bool: simple = if not op_mand[o] then
	                  forall(k in 2..n)(lcol[k]=lcol[2])
			else
			  forall(k in 1..n)(lcol[k]=lcol[1])
			endif} in
      if simple then
        lt[p] = lcol[n]
      else
        lt[p] = lcol[ii[o]]
      endif
  );

%% Model::post_temporary_use_latency_definition
var -MAXL..3*MAXL: lat(int: q, int: v) =
  let {int: p = temp_def[v]} in
    lt[p] + s[p] + lt[q] + s[q];

%% data precedences with slack and everything

array[int] of var int: opnd_lbs(int: q) =
  [ if v = -1 then -1
    else c[temp_definer[v]] + lat(q,v)
    endif
  | v in operand_temps[q]
  ];

predicate post_data_prec(int: q) =
    let {int: u = operand_definer[q],
         array[int] of var int: lbs = opnd_lbs(q)} in
      c[u] >= lbs[1] /\
      c[u] >= lbs[y[q]];

%% Model::post_data_precedences_constraints
%%   redundant if op_type[operand_definer[q]]!=TYPE_KILL and implied constraints are being posted
constraint
  forall(q in 0..MAXP where operand_use[q])(post_data_prec(q));

%% Model::post_reverse_data_precedences_constraints
%% DISABLED, because it seems to clash with my slack fixing stuff
% set of int: global_definers(int: q) = {pd |     t1 in real_temps(q),
%                                                 pd in {temp_def[t1]}
% 					  where global_operand(pd)
% 				      };
				     
% var -MAXL..MAXL: slack_of_global_definer(int: q) =
%   let {set of int: gds = global_definers(q)} in
%     if card(gds)>0 then s[min(gds)] else 0 endif;


% constraint
%   forall(o in 0..MAXO, p in op_operands[o] where not operand_use[p])(
%     let {int: v = single_temp(p),
%          array[int] of var int: acs = [ if t[q] = v then c[operand_definer[q]] - lat(q,v) - slack_of_global_definer(q) else 0 endif
%     		       	   	      | q in temp_uses[v]
% 				      ]} in
%       if r[v] >= 0 then
%         c[o] <= max(acs)
%       else true endif
%   );

%% resources

% test unit_res_dur_cap(int: res, set of int: active) = (
%     forall(o in active)(
%         fixedop(o) /\
% 	res_con[res,min(op_instructions[o])]=res_cap[res] /\
% 	res_dur[res,min(op_instructions[o])]=1 /\
% 	res_off[res,min(op_instructions[o])]=0
%     )
% );

test unit_res_dur_cap(int: res, set of int: active) = (
    forall(o in active, oi in op_instructions[o])(
	res_con[res,oi]=res_cap[res] /\
	res_dur[res,oi]=1 /\
	res_off[res,oi]=0
    )
);

test unit_res_dur(int: res, int: o) = (
    forall(oi in op_instructions[o])(res_dur[res,oi]=1)
);

test zero_res_off(int: res, int: o) = (
    forall(oi in op_instructions[o])(res_off[res,oi]=0)
);

int: max_res_dur(int: res, int: o) = (
    max(oi in op_instructions[o])(res_dur[res,oi])
);

int: max_res_off(int: res, int: o) = (
    max(oi in op_instructions[o])(res_off[res,oi])
);

constraint
    forall(b in 1..MAXB, res in index_set(res_cap) diff bb_subsumed[b]) (
        let {set of int: active = {o | o in bb_ops[b] where
	                               exists(q in op_instructions[o])
				             (res_con[res,q]>0)}} in (
            if card(active)<=1 then true
            else if unit_res_dur_cap(res,active) then
	        smart_alldiff([c[o] | o in active])
            else if good_cumulative then
		cumulative([ if zero_res_off(res,o) then c[o] else c[o]+res_off_of_op(res,o) endif
			   | o in active
			   ],
			   [ if unit_res_dur(res,o) then 1 else res_dur_of_op(res,o) endif
			   | o in active
			   ],
			   [ res_con_of_op(res,o)
			   | o in active
			   ],
			   res_cap[res])
	    else let {int: inf = min([lb(c[o]) | o in active]),
	              int: sup = max([ub(c[o]) | o in active])
		               + max([max_res_dur(res,o) | o in active])
		               + max([max_res_off(res,o) | o in active])
			       } in (
		     forall(j in inf..sup)(
		         sum(o in active,
			     io in op_instructions[o],
			     k in j-res_dur[res,io]-res_off[res,io]+1..j-res_off[res,io]
			     where res_con[res,io]>0 /\ k in lb(c[o])..ub(c[o]))
			    (res_con[res,io]*bool2int(c[o]=k /\ insn_eq(o,io))) <= res_cap[res]
		     )
		 )
            endif endif endif
	)
    );


%% packing constraints
%% see Model::post_packing_constraints()
constraint
  forall(k in index_set_1of2(packed_pq)) (
    let {int: p = packed_pq[k,1],
	 int: q = packed_pq[k,2],
	 int: w = temp_width[single_temp(p)],
	 array[int] of int: modtab = [-1,-1]++[ if k=1 then v else v + w - 2*(v mod 2*w) endif
					      | v in 0..MAXR, k in 1..2
					      where v mod w = 0
					      ],
	 int: nt = length(modtab) div 2,
	 } in
       table([rt[p], rt[q]], array2d(1..nt, 1..2, modtab))
  );

%% activators
constraint
  forall(x in index_set(activator_insns))(
    let {set of int: is = activator_insns[x],
         set of int: os = activator_ops[x]} in (
	(a[min(os)] <-> exists(o in 0..MAXO)(insn_in(o,op_instructions[o] intersect is))) /\
	forall(i2 in os where i2>min(os))(a[i2] <-> a[min(os)])
    )
  );

%% prescheduling
constraint
  forall(j in index_set(preschedule_op))(
    let {int: oper = preschedule_op[j],
         int: cycle = preschedule_cycle[j],
         int: bb = min({k | k in index_set(bb_ops) where oper in bb_ops[k]}),
         int: outop = max(bb_ops[bb])}
    in (a[oper]  -> c[oper] = cycle) /\
       (a[oper] <-> c[outop] > cycle)
  );

%% bypass
constraint
  forall(j in index_set_1of2(bypass_table))(
    let {int: o = bypass_table[j,1],
         int: insn = bypass_table[j,2],
         int: p = bypass_table[j,3]}
    in insn_eq(o,insn) -> c[o] = ls_t(p)
  );

%% exrelated
constraint
  forall(j in index_set(exrelated_p))(
    let {int: p = exrelated_p[j],
         int: q = exrelated_q[j],
	 array[int] of int: ext = [exrelated_ext[r,k] | r in exrelated_rows[j], k in 1..2]}
    in table([rt[p],rt[q]], array2d(1..(length(ext) div 2), 1..2, ext))
  );

%% ad-hoc
constraint
    forall(L in adhoc) (
        eval_expr(L)
    );

%% IMPLIED CONSTRAINTS

%% effective copy, handling multiple defs and/or uses
constraint
    forall(o in 0..MAXO where op_type[o] = TYPE_COPY)(
        let {int: src = min([p | p in op_operands[o] where operand_use[p]]),
	     int: dst = min([p | p in op_operands[o] where not operand_use[p]])} in
          a[o] -> rt[src] != rt[dst]
    );

%% dominating copies
constraint
    forall(k in index_set(dominate_ing)) (
        let {int: ing = dominate_ing[k],
	     int: ed = dominate_ed[k],
	     int: edsrc = min(op_operands[ed]),
	     set of int: iset = dominate_instructions[k],
	     set of int: tset = dominate_temps[k]} in (
	    a[ing] \/ not a[ed] \/ insn_in(ed,iset) \/ temp_in(edsrc,tset)
 	)
    );

%% disjoint registers
constraint
    forall(R in diffreg) (
        noverlap([rt[j] | j in R],
	         [temp_width[min(operand_temps[j])] | j in R])
    );

%% different temporaries
constraint
    forall(T in difftemp) (
      let {array[int] of int: TA = T} in
      forall(j in index_set(TA), k in index_set(TA) where j<k)(
        assert(operand_related(TA[j],TA[k]), "difftemp(\(T)) with unrelated operands")
      ) /\
      smart_alldiff([y[p] | p in T])
    );

%% nogoods
constraint
    forall(N in nogood) (
        not eval_expr(N)
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
	    global_cardinality([t(p) | p in 0..MAXP where can_use(p,tarr[1])],
		               tarr, count)
 	)
    )
  else
    forall(p in 0..MAXP where not operand_use[p])(
        let {int: v = single_temp(p)} in (
	    if temp_must_be_real(p) then
	      temp_member_reif({q | q in 0..MAXP where can_use(q,v)}, v, true)
	    else if opt_in_temporary(v) then
	      let {var bool: rp}
              in  (temp_is_real_reif(p,rp)) /\
                  (temp_member_reif({q | q in 0..MAXP where can_use(q,v)}, v, rp))
	    else
                  temp_member_reif({q | q in 0..MAXP where can_use(q,v)}, v, a[operand_definer[p]])
	    endif endif
        )
    )
  endif;

%% last use -- pretty much subsumed by maximum(le[v], ...)
% constraint
%     forall(p in 0..MAXP where operand_use[p])
%           (t[p]>=0 -> le[t[p]] >= c[operand_definer[p]]) % rematerialization
%     ;

%% disjoint live ranges projected on callee-saved regs
constraint
    if good_cumulative then
	forall(T in bb_temps) (
	    cumulative([ls[v] | v in T],
		       [ld[v] | v in T],
		       [temp_width[v]*bool2int(r[v] in calleesaved) | v in T],
		       card(calleesaved))
	)
    else true endif;

%% temp symmetry breaking among interchangeable temporaries in their potential use operands
constraint
    forall(j in index_set(domop_operands)) (
        value_precede_chain(domop_temps[j], [t(o) | o in domop_operands[j]])
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
		      let {set of int: insns = {i | i in op_instructions[I] where i>0 /\ operand_atom[P,i]=atm}} in (
		          if (card(insns)>=1) then
			      (temp_eq(Q,T) /\ insn_in(I,insns) /\ r[T] in atom_regs[atm] -> temp_eq(P,T))
			  else true endif
		      )
		  )
	      else true endif
	  )
    );

%% before
constraint
    forall(j in index_set(before_pred)) (
        let {int: p = before_pred[j],
	     int: q = before_succ[j],
	     expr: bc = before_cond[j]} in (
            eval_expr(bc) -> operand_live_before(p,q)
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
    forall(j in index_set(relation_ops)) (
        let{set of int: ops = relation_ops[j],
	    set of int: temps = relation_temps[j],
	    int: nt = relation_ntuples[j],
	    set of int: range = relation_range[j],
	    set of int: irange = 1..(card(ops)+card(temps)),
	    array[irange] of var int: vars = [bool2int(a[j]) | j in ops]++
	                                     [t(j) | j in temps]} in (
	   table(vars, array2d(1..nt,irange,[ints[j] | j in range]))
	)
    );

%% combine
constraint
    forall(o in 0..MAXO where op_type[o] = TYPE_COMBINE)(
	let {array[int] of int: opnds = [x | x in op_operands[o]],
	     int: src1 = opnds[1],
	     int: src2 = opnds[2],
	     int: def  = opnds[3]} in (
	    minimum(ls_t(def), [le_t(src1), le_t(def)]) /\
	    minimum(ls_t(def), [le_t(src2), le_t(def)])
	)
    );

%% low/high
constraint
    forall(o in 0..MAXO where op_type[o] in {TYPE_HIGH, TYPE_LOW})(
	let {int: src  = min(op_operands[o]),
	     int: def  = max(op_operands[o])} in (
	    minimum(ls_t(def), [le_t(src), le_t(def)])
	)
    );

%% useless memory stores
constraint
    forall(T in related_temps) (
	sum([bool2int(r[v] in infinite) | v in T]) <= 1
    );

%% across
constraint
    forall(j in index_set(across_op)) (
	let {int: opi = across_op[j],
	     int: maxwidth = max(temp_width),
             set of int: regs = across_regs[j],
             set of int: items = across_items[j],
	     array[items] of int: cand =
	         array1d(items, [across_item_temp[k] | k in items]),
	     array[items] of var bool: active,
	     array[items] of var -maxwidth*(MAXT+1)..MAXR: selected,
	     array[int] of var int: allreg =
	         [x | x in callersaved] ++ [x | x in regs] ++ selected,
	     array[int] of int: allwidth =
	         [1 | x in callersaved] ++
	         [1 | x in regs] ++
		 [temp_width[cand[k]] | k in items]} in (
	    forall(k in items)(
	        let {int: ck = cand[k]} in (
		    (   active[k] <->
			    eval_expr(across_item_cond[k]) \/
			    (ls[ck] <= c[opi] /\ c[opi] < le[ck])) /\
		    selected[k] = ite(active[k], r[ck], -maxwidth*(ck+1))
		)
	    ) /\
	    noverlap(allreg, allwidth)
	)
    );

%% set_across
constraint
    forall(j in index_set(setacross_op)) (
	let {int: opi = setacross_op[j],
             set of int: regs = setacross_regs[j],
             set of int: itemsset = setacross_tempsets[j],
	     set of int: items = 1..card(itemsset),
	     array[items] of set of int: cands = [sets[it] | it in itemsset],
	     array[items] of var 0..MAXR: selected,
	     array[items] of var 0..max(temp_width): width,
	     array[int] of var int: allreg =
	         [x | x in callersaved] ++ [x | x in regs] ++ selected,
	     array[int] of var int: allwidth =
	         [1 | x in callersaved] ++ [1 | x in regs] ++ width} in (
	    forall(it in items)(
	        let {set of int: cit = cands[it],
		     array[int] of int: ws  = [temp_width[k] | k in cit],
		     array[int] of var int: rs  = [r[k] | k in cit],
		     array[int] of var int: lss = [ls[k] | k in cit],
		     array[int] of var int: les = [le[k] | k in cit],
		     var 1..card(cit): k} in (
		  width[it] = if min(ws) < max(ws) then ws[k] else min(ws) endif /\
		  selected[it] = rs[k] /\
		  lss[k] <= c[opi] /\ c[opi] < les[k]
		)
	    ) /\
	    noverlap(allreg, allwidth)
	)
    );

%% spill callee-saved if and only if some temp interfers with it
constraint
    if card(index_set(calleesaved_spill))>0 then
	let {int: nrows = card(index_set_1of2(cs_spill_transpose)),
	     int: ncols = card(index_set_2of2(cs_spill_transpose)),
             set of int: css_ops = {cs_spill_transpose[j,k] | j in 1..nrows, k in 1..ncols},
             set of int: preass_ops = {o | o in 0..MAXO where op_type[o] in {TYPE_IN,TYPE_CALL,TYPE_TAIL,TYPE_FUN}},
	     set of int: tcands = {o | o in 0..MAXT where not (temp_definer[o] in css_ops union preass_ops)}} in (
	    forall(j in 1..ncols) (
	        let {int: theop = cs_spill_transpose[1,j],
		     int: thepu = min(op_operands[theop]),
		     int: thetu = single_temp(thepu),
		     int: thepd = temp_def[thetu],
		     int: thea = min(k in index_set(preassign_reg) where preassign_operand[k]=thepd)(preassign_reg[k]),
		     int: thew = temp_width[thetu]} in (
		    a[theop] <-> exists(ti in tcands)(r[ti]+temp_width[ti] > thea /\ r[ti] in 0..thea+thew-1)
		)
	    )
	)
    else true endif;

%% break symmetries among which callee-saved to spill
constraint
    if card(index_set(calleesaved_spill))>0 then
	forall(j in index_set(calleesaved_spill)) (
	    let {set of int: cgr = calleesaved_spill[j],
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
	    forall(j in 1..nrows)(
		decreasing([a[cs_spill_transpose[j,k]] | k in 1..ncols]) /\
		forall(k in 2..ncols)(
		  if j=1 then % spill
		    a[cs_spill_transpose[j,k]] -> c[cs_spill_transpose[j,k-1]] <= c[cs_spill_transpose[j,k]]
		  else % unspill
		    a[cs_spill_transpose[j,k]] -> c[cs_spill_transpose[j,k-1]] >= c[cs_spill_transpose[j,k]]
		  endif
		)
	    ) /\
	    if spill subset bb_ops[1] /\ unspill subset bb_ops[1] then
		forall(k in 1..ncols)(
		    let {int: u = cs_spill_transpose[2,k],
			 int: v = cs_spill_transpose[1,k]} in (
			a[u] -> c[v] < c[u]
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
         array[1..nt] of var int: chain = [r[v]+d | j in js, v in {value_precede_temps[j]}, d in 0..temp_width[v]-1]} in
	value_precede_chain([v | v in value_precede_regs[x]], chain)
  );

%% predecessors -- DISABLED
constraint
  forall(x in index_set(predecessors_preds))(
    min([c[o] | o in predecessors_preds[x]]) + predecessors_lat[x] <= c[predecessors_succ[x]]
  );

%% successors -- DISABLED
constraint
  forall(x in index_set(successors_succs))(
     c[successors_pred[x]] + successors_lat[x] <= max([c[o] | o in successors_succs[x]])
  );

%% IMPORTED FROM Model::

%% live range length
% constraint			%  rematerialization
%     forall(v in 0..MAXT where op_type[temp_definer[v]] != TYPE_IN)
%           (a[temp_definer[v]] -> ld[v] >= temp_minlive[v]);

%% Model::post_minimum_temporary_duration_constraints
constraint
  forall(o in 0..MAXO, dp in op_operands[o] where not operand_use[dp])(
    let {int: v = single_temp(dp)} in
	(temp_eq(dp,v) -> ld[v] >= temp_minlive[v]) /\
	forall(up in temp_uses[v])(temp_eq(up,v) -> ld[v] >= lat(up,v))
  );
        
%% Model::post_define_issue_cycle_constraints
constraint
  forall(o in 0..MAXO where op_type[o]=TYPE_DEFINE)(
    let {set of int: js = {operand_definer[q] | p in op_operands[o],
	                                        v in {single_temp(p)},
    	     	     	                        q in temp_uses[v]},
         array[int] of var int: lats = [lat(q,v) | p in op_operands[o],
	                                           v in {single_temp(p)},
    	     	     	                           q in temp_uses[v]]} in
      if card(js) = 1 then
        c[min(js)] = c[o] + max(lats)
      else true endif
  );
        
%% DOMINATION: live range of temp occurring in (kill)
predicate kill_live_range(int: v) =
  (ld[v] = lt[temp_def[v]]);

%% Model::post_kill_issue_cycle_constraints
constraint
  forall(o2 in 0..MAXO where op_type[o2] = TYPE_KILL)(
    if op_mand[o2] then % e.g. after function call, multiple operands
      let {array[int] of var int: lats = [lat(q,v) | q in op_operands[o2],
						     v in {single_temp(q)}],
	   array[int] of int: ts = [v | q in op_operands[o2],
					v in {single_temp(q)}],
	   set of int: o1s = {temp_definer[v] | q in op_operands[o2],
						v in {single_temp(q)}},
      } in
        forall(v in ts)(kill_live_range(v)) /\
	c[o2] = c[min(o1s)] + max(lats)
    else % e.g. after move subject to pack, one operand, multiple temps
      let {int: q1 = min(op_operands[o2]),
           int: p1 = temp_def[single_temp(q1)],
	   set of int: o1s = {temp_definer[v] | v in real_temps(q1)},
	   array[int] of var int: issue = [-1] ++ [c[temp_definer[v]] + lat(q1,v) | v in real_temps(q1)],
      } in
        card(op_operands[o2]) = 1 /\ % sanity check
        forall(v in real_temps(q1))(kill_live_range(v)) /\
	c[o2] = issue[y[q1]] /\
	if card(real_temps(q1))=1 then
	  same_temp(p1,q1) /\ rt[p1] = rt[q1]
	else
	  sum(o in o1s)(a[o]) <= 1
	endif
    endif
  );

%% Model::post_slack_lower_bound_constraints
% constraint
%   forall(C in congr)(
%     let {set of int: CG  = {p | p in C where global_operand(p)},
% 	 array[int] of int: ins  = [p | p in CG where op_type[operand_definer[p]] = TYPE_IN],
% 	 array[int] of int: outs = [p | p in CG where op_type[operand_definer[p]] = TYPE_OUT],
% 	 array[int] of var int: inlats = [lt[q] | p in ins,
% 						  q in temp_uses[single_temp(p)]],
% 	 array[int] of var int: outlats = [lt[q] | p in outs,
% 						   v in real_temps(p),
% 						   q in {temp_def[v]}]} in
%       if (length(ins)>0) then
% 	let {var int: maxinlat = max(inlats)} in
% 	  forall(p in ins)(trace("s[\(p)] >= -max(\(inlats))\n", s[p] >= - maxinlat))
%       else true endif /\
%       if (length(outs)>0) then
% 	let {var int: maxoutlat = max(outlats)} in
% 	  forall(p in outs)(trace("s[\(p)] >= -max(\(outlats))\n", s[p] >= - maxoutlat))
%       else true endif
%   );

%% [matsc] domination constraint for slack, in fact make it a functional dependency
%% N.B. Subsumes post_slack_lower_bound_constraints
%% N.B. MAY clash with post_reverse_data_precedences_constraints
constraint
  forall(j in index_set_1of2(long_latency_index))(
    let {set of int: inps = long_latency_index[j,1],
	 set of int: inix = long_latency_index[j,2],
         set of int: outps = long_latency_index[j,3],
	 set of int: outix = long_latency_index[j,4],
         array[int] of var int:  inubs = [ if temp_eq(q,single_temp(p)) then
					     c[operand_definer[q]] - lt[q] - s[q] - lt[p] 
					   else MAXL endif
					 | i in inix,
					   p in [long_latency_def[i]],
					   q in [long_latency_use[i]]
					 ],
         array[int] of var int: outubs = [ if temp_eq(q,single_temp(p)) then
	 	       	   	       	     ld[single_temp(p)] - lt[q] - lt[p] - s[p]
					   else MAXL endif
					 | i in outix,
					   p in [long_latency_def[i]],
					   q in [long_latency_use[i]]
					 ]} in
      assert((card(inps)>0 /\ card(outps)>0), "invalid long_latency item") /\
      let {var int: outlb = if length( inubs)>0 then -min( inubs) else -MAXL endif,
	   var int: outub = if length(outubs)>0 then  min(outubs) else  MAXL endif} in
	outlb <= outub /\
	forall(p in outps)(s[p] = min(outub,max(outlb,0)))
  );

%% output

output
    ["\"cost\": "]   ++ [show(obj)] ++ ["\n"] ++
    ["\"cycles\": "]      ++ [show(c)]   ++ ["\n"] ++
    ["\"instructions\": "]  ++ [show([op_instructions[o][fix(ii[o])] | o in 0..MAXO])]   ++ ["\n"] ++
    ["\"temporaries\": "] ++ [show([operand_temps[p][fix(y[p])] | p in 0..MAXP])]   ++ ["\n"] ++
    ["\"registers\": "]   ++ [show([r[v] | v in 0..MAXT])] ++ ["\n"] ++
    ["\"live start\": "]   ++ [show([ls[v] | v in 0..MAXT])] ++ ["\n"] ++
    ["\"live duration\": "]   ++ [show([ld[v] | v in 0..MAXT])] ++ ["\n"] ++
    ["\"live end\": "]   ++ [show([le[v] | v in 0..MAXT])] ++ ["\n"] ++
    % handy for debugging
    % ["\"active\": "]      ++ [show(a)]   ++ ["\n"] ++
    % ["\"regs of temporaries\": "] ++ [show(rt)]   ++ ["\n"] ++
    % ["\"lat\": "] ++ [show(lt)]   ++ ["\n"] ++
    % ["\"slack\": "] ++ [show(s)]   ++ ["\n"] ++
    % ["\"temp nb\": "] ++ [show(y)]   ++ ["\n"] ++
    % ["c[\(o)] = \(c[o])\n" | o in 0..MAXO where fix(c[o])>=0] ++
    % ["\"regs of globals\": "] ++ ["\n,[\(p),\(rt[p])]" | p in 0..MAXP where global_operand(p)]   ++ ["\n"] ++
    [];

