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
%
%   TODO:

include "globals.mzn";

%% solver parameters

bool: good_cumulative;

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
int: TYPE_FUN     = 11;
int: TYPE_COPY    = 12;

%% problem parameters

int: MAXI;
int: MAXP;
int: MAXT;
int: MAXC;
int: MAXO;
int: MAXR;
int: MAXB = max(index_set(bb_insns));

set of int: conjunction = index_set(sets);

%% see JSON freq, maxc, ins, tmp, operands, copymin
array[int] of int: bb_order;
array[int] of set of int: bb_insns;
array[int] of set of int: bb_operands;
array[int] of set of int: bb_temps;
array[int] of int: bb_frequency;
array[int] of int: bb_maxcycle;
array[int] of int: bb_copymin;

%% see JSON operands, operations, type
array[int] of set of int: insn_operands;
array[int] of set of int: insn_operations;
array[int] of int: insn_type;
array[int] of bool: insn_mand;
array[int] of int: insn_order;

%% see JSON atoms
array[int] of set of int: atom_regs; % atom -> registers

%% see JSON calleesaved, callersaved, infinite, range
set of int: calleesaved;
set of int: callersaved;
set of int: infinite;

%% see JSON cap, dur, con
array[int] of int: res_cap;
array[int,int] of int: res_dur;
array[int,int] of int: res_con;

%% see JSON congr
array[int] of set of int: congr;

%% see JSON preassign
array[int] of int: preassign_operand;
array[int] of int: preassign_reg;

%% see JSON high
array[int] of int: high_use;
array[int] of int: high_def;

%% see JSON low
array[int] of int: low_use;
array[int] of int: low_def;

%% see JSON operands
array[int] of int: operand_definer; % operand -> defining insn

%% see JSON use
array[int] of bool: operand_use;

%% see JSON last_use
array[int] of bool: operand_lastuse;

%% see JSON temps
array[int] of set of int: operand_temps;

%% see JSON class, operands, operations
array[int,int] of int: operand_atom; % operand x operation -> atom

%% see JSON temps
array[int] of set of int: related_temps;

%% see JSON definer, temps, width, minlive, temp_unsafe
array[int] of int: temp_definer; % temp -> defining insn
array[int] of int: temp_def;     % temp -> defining operand
array[int] of int: temp_width;   % temp -> width
array[int] of int: temp_minlive; % temp -> minlive
array[int] of bool: temp_unsafe; % temp -> unsafe
array[int] of set of int: temp_uses; % temp -> set of use operands

%% see JSON before
array[int] of int: before_pred;
array[int] of int: before_succ;
array[int] of set of conjunction: before_cond;

%% see JSON nogoods
array[int] of conjunction: nogood;

%% see JSON across
array[int] of int: across_insn;
array[int] of set of int: across_regs;
array[int] of set of int: across_items;
array[int] of int: across_item_temp;
array[int] of set of conjunction: across_item_cond;

%% see JSON set_across
array[int] of int: setacross_insn;
array[int] of set of int: setacross_regs;
array[int] of set of int: setacross_tempsets;

%% see JSON diffops
array[int] of set of int: diffop;

%% see JSON diffregs
array[int] of set of int: diffreg;

%% see JSON domops
array[int] of set of int: domop_operands;
array[int] of set of int: domop_temps;

%% see JSON domuse
array[int] of int: domuse_p;
array[int] of int: domuse_q;
array[int] of int: domuse_r;

%% see JSON dominates
array[int] of int: dominate_ing;
array[int] of int: dominate_ed;
array[int] of set of int: dominate_operations;
array[int] of set of int: dominate_temps;

%% see JSON precedences
array[int] of int: precedence_pred;
array[int] of int: precedence_succ;
array[int] of int: precedence_diff;
array[int] of set of conjunction: precedence_cond;

%% see JSON copy_tables, tmp_tables
array[int] of set of int: table_exists_insns;
array[int] of set of int: table_iffall_insns;
array[int] of set of int: relation_insns;
array[int] of set of int: relation_temps;
array[int] of int: relation_ntuples;
array[int] of set of int: relation_range;

%% see JSON calleesaved_spill
array[int] of set of int: calleesaved_spill;
array[int,int] of int: cs_spill_transpose;

%% extra storage for conjunctions and disjunctions
array[int] of int: literal_op;
array[int] of int: literal_arg1;
array[int] of int: literal_arg2;

%% extra store for relations in table constraints
array[int] of int: ints;

%% extra storage: each set is either a conjunction or a set of temporaries
array[int] of set of int: sets;

%% problem variables

var int: obj;
array[0..MAXI] of var bool: a;
array[0..MAXI] of var 0..MAXO: o;
array[0..MAXI] of var -1..MAXC: c;
array[0..MAXP] of var -1..MAXT: t;
array[0..MAXP] of var -1..MAXR: rt;
array[-1..MAXT] of var -1..MAXR: r;
array[-1..MAXT] of var -1..MAXC: ls;
array[-1..MAXT] of var 0..MAXC: ld;
array[-1..MAXT] of var -1..MAXC: le;
array[1..MAXB] of var 0..MAXC: copysum;

%% PREDICATES AND TESTS

test isreal(int: i) = (insn_type[i] in
                       {TYPE_LINEAR, TYPE_BRANCH, TYPE_CALL, TYPE_TAIL, TYPE_FUN, TYPE_COPY});

test can_use(int: p, int: temp) = (
      operand_use[p] /\
      temp in operand_temps[p]
);

test fixedop(int: i) = (
    insn_operations[i] != {0} /\ card(insn_operations[i]) = 1
);

predicate noverlap(array[int] of var int: x,
                   array[int] of var int: w) = (
	if min([lb(y) | y in w]) < max([ub(y) | y in w]) then
	    diffn(x, [1 | i in index_set(x)], w, [1 | i in index_set(x)]) :: domain
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
	if tag=0 then t[arg1] = t[arg2]
	else if tag=1 then t[arg1] = arg2
	else if tag=2 then a[arg1]
	else if tag=3 then o[arg1] = arg2
	else if tag=4 then live_overlap(t[arg1],t[arg2])
	else if tag=5 then live_overlap(arg1,arg2)
	else if tag=6 then r[arg1] in callersaved
        else false endif endif endif endif endif endif endif
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
	      int: elseval,
	      var int: value) = (
    let {array[0..1] of var int: ar = array1d(0..1, [elseval,thenval])} in (
        value = ar[bool2int(cond)]
    )
);

%% (mostly) ESSENTIAL CONSTRAINT

%% basic invariants, null operations

constraint
    r[-1] = -1 /\
    ls[-1] = -1 /\
    ld[-1] = 0 /\
    le[-1] = -1 /\
    forall(i in 0..MAXI where insn_mand[i]) (a[i]) /\
    forall(i in 0..MAXI) (o[i] >= 1 <-> a[i]) /\
    forall(i in 0..MAXI) (c[i] >= 0 <-> a[i]) /\
    forall(p in 0..MAXP) (rt[p] = r[t[p]]) /\
    forall(p in 0..MAXP where operand_use[p])
          (t[p] >= 0 <-> a[operand_definer[p]]) /\
    forall(v in 0..MAXT) (ls[v] = c[temp_definer[v]]) /\
    forall(v in 0..MAXT) (ls[v]+ld[v] = le[v]) /\
    forall(v in 0..MAXT) (r[v] >= 0 <-> a[temp_definer[v]]) /\
    forall(v in 0..MAXT) (
        let {set of int: pset = {p | p in 0..MAXP where can_use(p,v)},
	     set of int: prange = 1..card(pset),
	     array[prange] of int: parr = [p | p in pset],
	     array[prange] of var -1..MAXC: carr} in (
            forall(i in prange)
	          (ite((t[parr[i]]=v), c[operand_definer[parr[i]]], -1, carr[i])) /\
            maximum(le[v], carr++[ls[v]+ % 20130930: live range can extend past last use
	                          temp_minlive[v]*bool2int(a[def])
				 | def in [temp_definer[v]] where temp_unsafe[v]])
        )
    );

%% objective, bounds on (out) cycle

constraint
    forall(b in 1..MAXB) (
        if exists(i in bb_insns[b] where not insn_mand[i])(true) then
	    let {int: realsum = sum(i in bb_insns[b] where isreal(i))(1),
		 int: bundlesize = max(x in 0..MAXO)(res_con[1,x]),
		 int: copysize = min(i in bb_insns[b], x in insn_operations[i]
			             where not insn_mand[i] /\ x>0)(res_con[1,x]),
		 int: realtotal = sum(i in bb_insns[b] where isreal(i))
				     (min(x in insn_operations[i])(res_con[1,x]))} in (
		forall(i in bb_insns[b])(c[i] in -1..bb_maxcycle[b]) /\
		c[min(bb_insns[b])] = 0 /\
		copysum[b] = sum(i in bb_insns[b] where not insn_mand[i])(bool2int(a[i])) /\
		copysum[b] >= bb_copymin[b] /\
		copysum[b] + realsum + 2 >= c[max(bb_insns[b])] /\
		copysize*copysum[b] + realtotal <= bundlesize*(c[max(bb_insns[b])]-1)
	    )
	else
	    let {int: realsum = sum(i in bb_insns[b] where isreal(i))(1),
		 int: bundlesize = max(x in 0..MAXO)(res_con[1,x]),
		 int: realtotal = sum(i in bb_insns[b] where isreal(i))
				     (min(x in insn_operations[i])(res_con[1,x]))} in (
		forall(i in bb_insns[b])(c[i] in -1..bb_maxcycle[b]) /\
		c[min(bb_insns[b])] = 0 /\
		copysum[b] = 0 /\
		realsum + 2 >= c[max(bb_insns[b])] /\
		realtotal <= bundlesize*(c[max(bb_insns[b])]-1)
	    )
	endif
    ) /\
    obj = sum(b in 1..MAXB) (bb_frequency[b] * c[max(bb_insns[b])]);

%% domains

constraint
    forall(i in 0..MAXI) (o[i] in insn_operations[i]) /\
    forall(p in 0..MAXP) (t[p] in operand_temps[p]) /\
    forall(v in 0..MAXT) (
        let {int: p = temp_def[v]} in (
    	    r[v] in array_union([atom_regs[operand_atom[p,q]] | q in 0..MAXO]) union {-1}
        )
    );

%% operation selection (register set depending on operation)

constraint
    forall(p in 0..MAXP)
          (let {int: i = operand_definer[p]} in (
	       if fixedop(i) then
	           rt[p] in atom_regs[operand_atom[p,min(insn_operations[i])]]
	       else
		   forall(opcode in insn_operations[i])
			 (if opcode=0 then
			     o[i] = 0 -> rt[p] = -1
			  else
			     o[i] = opcode -> rt[p] in atom_regs[operand_atom[p,opcode]]
			  endif)
	       endif
           )
    );

%% preassignments

constraint
    forall(j in index_set(preassign_reg))
          (rt[preassign_operand[j]] = preassign_reg[j]);

%% congruences

constraint
    forall(j in index_set(congr)) (
        let {set of int: cgr = congr[j],
	     int: junior = min(cgr)} in (
	    forall(j in cgr where j>junior) (rt[junior] = rt[j])
 	)
    );

%% low/high

constraint
    forall(i in index_set(low_use)) (
        let {int: p = low_use[i],
	     int: q = low_def[i]} in (
            rt[p] = rt[q]
	)
    ) /\
    forall(i in index_set(high_use)) (
        let {int: p = high_use[i],
	     int: q = high_def[i],
	     int: offset = temp_width[min(operand_temps[p])] div 2} in (
            rt[p] + offset = rt[q]
	)
    );

%% disjoint live ranges

constraint
    forall(b in index_set(bb_temps)) (
	diffn([r[i] | i in bb_temps[b]],
	      [ls[i] | i in bb_temps[b]],
	      [temp_width[i]*bool2int(ld[i]>0) | i in bb_temps[b]],
	      [ld[i] | i in bb_temps[b]]) :: domain
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
    ) /\
    forall(p in 0..MAXP where operand_use[p])
          (let {int: iuse = operand_definer[p],
	        set of int: temps = operand_temps[p]} in (
	       % 20130930: live range can extend past last use
	       % if operand_lastuse[p] then
	       %     forall(v in temps where v >= 0)(
	       % 	       let {int: idef = temp_definer[v]} in (
	       %             if insn_type[iuse] = TYPE_KILL then
	       % 	               ld[v] = 1
	       % 	           else if card(temps) = 1 then
	       % 	               c[idef] + ld[v] = c[iuse]
	       % 	           else
	       % 	               t[p] = v -> c[idef] + ld[v] = c[iuse]
	       % 	   	   endif endif
	       % 	       )
	       % 	   )
	       % else
	           forall(v in temps where v >= 0)(
		       let {int: idef = temp_definer[v]} in (
		           if card(temps) = 1 then
		               c[idef] + ld[v] >= c[iuse]
		           else
		               t[p] = v -> c[idef] + ld[v] >= c[iuse]
		   	   endif
		       )
		   )
	       % endif
	   )
    );

%% resources

constraint
    forall(r in index_set(res_cap), b in 1..MAXB) (
        let {set of int: active = {i | i in bb_insns[b] where
	                               exists(q in insn_operations[i])
				             (res_dur[r,q]>0)}} in (
            if card(active)>1 then
		cumulative([c[i] | i in active],
			   [res_dur[r,if fixedop(i) then
					  min(insn_operations[i])
				      else
					  o[i]
				      endif] | i in active],
			   [res_con[r,if fixedop(i) then
					  min(insn_operations[i])
				      else
					  o[i]
				      endif] | i in active],
			   res_cap[r]) :: domain
            else true endif
	)
    );

%% IMPLIED CONSTRAINTS

%% live range length

constraint
    forall(v in 0..MAXT) (a[temp_definer[v]] -> ld[v] >= temp_minlive[v]);

%% effective copy

constraint
    forall(p in 0..MAXP where operand_use[p] /\
                              insn_type[operand_definer[p]] = TYPE_COPY)
          (a[operand_definer[p]] -> rt[p] != rt[p+1]);	  % p+1 is corresponding def

%% dominating copies

constraint
    forall(k in index_set(dominate_ing)) (
        let {int: i = dominate_ing[k],
	     int: j = dominate_ed[k],
	     int: jsrc = min(insn_operands[j]),
	     set of int: oset = dominate_operations[k],
	     set of int: tset = dominate_temps[k]} in (
	    a[i] \/ not a[j] \/ o[j] in oset \/ t[jsrc] in tset
 	)
    );

%% disjoint registers

constraint
    forall(i in index_set(diffreg)) (
        noverlap([rt[j] | j in diffreg[i]],
	         [temp_width[min(operand_temps[j])] | j in diffreg[i]])
    );

%% different temporaries

constraint
    forall(i in index_set(diffop)) (
        all_different([t[j] | j in diffop[i]])
    );

%% nogoods

constraint
    forall(i in index_set(nogood)) (
        not eval_conjunction(nogood[i])
    );

%% live temporary

constraint
    forall(i in index_set(related_temps)) (
        let {set of int: tset = related_temps[i], % set of copy-related temps
	     set of int: irange = 1..card(tset),
	     array[irange] of var 0..card(tset): count,
	     array[irange] of int: tarr = [x | x in tset]} in (
	    forall(j in irange)(count[j] >= bool2int(a[temp_definer[tarr[j]]])) /\
	    global_cardinality([t[p] | p in 0..MAXP where can_use(p,tarr[1])],
		               tarr, count) :: domain
 	)
    );

%% last use

constraint
    forall(p in 0..MAXP where operand_use[p])
          (le[t[p]] >= c[operand_definer[p]])
    % /\
    % 20130930: live range can extend past last use
    % forall(p in 0..MAXP where operand_lastuse[p])
    %       (le[t[p]] = c[operand_definer[p]])
    ;

%% disjoint live ranges projected on callee-saved regs

constraint
    if good_cumulative then
	forall(b in index_set(bb_temps)) (
	    cumulative([ls[i] | i in bb_temps[b]],
		       [ld[i] | i in bb_temps[b]],
		       [temp_width[i]*bool2int(r[i] in calleesaved) | i in bb_temps[b]],
		       card(calleesaved)) :: domain
	)
    else true endif;

%% temporary reuse (hairy!)

constraint
    forall(i in index_set(domuse_p)) (
          let {int: P = domuse_p[i], % use operand
	       int: Q = domuse_q[i], % use operand
	       int: R = domuse_r[i], % def operand
	       int: T = min(operand_temps[R]),
	       int: I = operand_definer[P],
	       int: J = operand_definer[Q],
	       int: K = operand_definer[R]} in (
	      c[K] < c[I] /\ c[I] <= c[J] /\
	      t[Q] = T /\
	      exists(opcode in insn_operations[I] where opcode>0) (
	          o[I] = opcode /\
	          r[T] in atom_regs[operand_atom[P,opcode]]
	      ) ->
	      t[P] = T
	  )
    );

%% before

constraint
    forall(i in index_set(before_pred)) (
        let {int: p = before_pred[i],
	     int: q = before_succ[i],
	     set of int: cond = before_cond[i]} in (
            eval_disjunction(cond) -> le[t[p]] <= ls[t[q]]
	)
    );

%% copy_table, tmp_table

constraint
    forall(i in index_set(table_exists_insns)) (
        exists(j in table_exists_insns[i])(a[j])
    );

constraint
    forall(i in index_set(table_iffall_insns)) (
        let {int: j = min(table_iffall_insns[i]),
	     int: k = max(table_iffall_insns[i])} in (
	    a[j] <-> a[k]
	)
    );

constraint
    forall(i in index_set(relation_insns)) (
        let{set of int: insns = relation_insns[i],
	    set of int: temps = relation_temps[i],
	    int: nt = relation_ntuples[i],
	    set of int: range = relation_range[i],
	    set of int: irange = 1..(card(insns)+card(temps)),
	    array[irange] of var int: vars = [bool2int(a[j]) | j in insns]++
	                                     [t[j] | j in temps]} in (
	   table(vars, array2d(1..nt,irange,[ints[j] | j in range]))
	)
    );

%% low/high/combine

constraint
    forall(i in 0..MAXI)(
        if insn_type[i] = TYPE_COMBINE then
	    let {array[int] of int: opnds = [x | x in insn_operands[i]],
	         int: src1 = opnds[1],
	         int: src2 = opnds[2],
	         int: def  = opnds[3]} in (
	        minimum(ls[t[def]], [le[t[src1]], le[t[def]]]) /\
	        minimum(ls[t[def]], [le[t[src2]], le[t[def]]])
	    )
	else if insn_type[i] in {TYPE_HIGH, TYPE_LOW} then
	    let {int: src  = min(insn_operands[i]),
	         int: def  = max(insn_operands[i])} in (
	        minimum(ls[t[def]], [le[t[src]], le[t[def]]])
	    )
	else
	    true
	endif endif
    );

%% useless memory stores

constraint
    forall(i in index_set(related_temps)) (
	sum([bool2int(r[v] in infinite) | v in related_temps[i]]) <= 1
    );

%% across

% this version seems marginally better
constraint
    forall(i in index_set(across_insn)) (
	let {int: insn = across_insn[i],
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
			    (ls[cj] <= c[insn] /\ c[insn] < le[cj])) /\
		    ite(active[j], r[cj], -maxwidth*(cj+1), selected[j])
		)
	    ) /\
	    noverlap(allreg, allwidth)
	)
    );

% this version seems marginally worse
% constraint
%     forall(i in index_set(across_insn)) (
% 	let {int: insn = across_insn[i],
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
% 			    (ls[cj] <= c[insn] /\ c[insn] < le[cj])) /\
% 		    selected[j] = r[cj]
% 		)
% 	    ) /\
% 	    noverlap(allreg, allwidth)
% 	)
%     );

%% set_across

constraint
    forall(i in index_set(setacross_insn)) (
	let {int: insn = setacross_insn[i],
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
		  lss[k] <= c[insn] /\ c[insn] < les[k]
		)
	    ) /\
	    noverlap(allreg, allwidth)
	)
    );

%% definer's reg must be user's
%% hasn't yet shown any effect
constraint
    forall(v in index_set(temp_uses)) (
        let {set of int: tuv = temp_uses[v]} in (
            if card(tuv) > 1 then
	        let {array[1..card(tuv)] of var int: rs,
		     array[int] of int: tuva = [x | x in tuv]} in (
		    forall(i in index_set(rs)) (
		        ite(t[tuva[i]]=v, rt[tuva[i]], -1, rs[i])
		    ) /\
	    	    member(rs, r[v])
		)
	    else true endif
	)
    );

%% spilling callee-saved -- DISABLED, and replaced by symmetry-breaking their cycles
% constraint
%     forall(j in index_set(calleesaved_spill)) (
%         let {set of int: cgr = calleesaved_spill[j],
% 	     int: cur = min(cgr),
% 	     var bool: aprev =
% 	               if j>1 then a[min(calleesaved_spill[j-1])] else true endif} in (
% 	    (a[cur] -> aprev) /\
% 	    forall(j in cgr where j>cur) (a[cur] <-> a[j])
%  	)
%     );

constraint
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
	    decreasing([c[cs_spill_transpose[i,(ncols+1)-j]] | j in 1..ncols])
	) /\
	if spill subset bb_insns[1] /\ unspill subset bb_insns[1] then
	    forall(j in 1..ncols)(
	        let {int: u = cs_spill_transpose[2,j],
		     int: s = cs_spill_transpose[1,1]} in (
		    a[u] -> c[s] < c[u]
		)
	    )
	else true endif
    );

%%%%%%%% SYMMETRY-FINDER CONSTRAINTS %%%%%%%%

array[int] of int: facit_a;
array[int] of int: facit_c;
array[int] of int: facit_o;
array[int] of int: facit_t;
array[int] of int: facit_r;
array[int] of int: facit_rt;

constraint
    let {set of int: mandatory = {i | i in 0..MAXI where insn_mand[i]}} in (
        forall(j in mandatory)(
	    c[j] = facit_c[j] /\
	    o[j] = facit_o[j] /\
            forall(p in insn_operands[j])(rt[p] = facit_rt[p])
	)
    );

constraint
    forall(b in 1..MAXB)(
 	let {set of int: optional = {i | i in bb_insns[b] where not insn_mand[i]},
	     set of int: temps = bb_temps[b],
	     int: ni = card(optional),
	     int: nr = card(temps),
	     array[1..ni] of var -1..MAXC: cbag,
	     array[1..ni] of var  0..MAXO: obag,
	     array[1..nr] of var -1..MAXR: rbag} in (
	    sum(j in optional)(bool2int(a[j])) = sum(j in optional)(facit_a[j]) /\
	    sort([facit_c[j] | j in optional], cbag) /\
	    sort([c[j] | j in optional], cbag) /\
	    sort([facit_o[j] | j in optional], obag) /\
	    sort([o[j] | j in optional], obag) /\
	    sort([facit_r[j] | j in temps], rbag) /\
	    sort([r[j] | j in temps], rbag)
	)
    );

%% essentially force same solution
% constraint
%     forall(i in 0..MAXI)(
%         c[i] = facit_c[i] /\
%         o[i] = facit_o[i]
%     ) /\
%     forall(p in 0..MAXP)(
%         t[p] = facit_t[p]
%     ) /\
%     forall(t in 0..MAXT)(
%         r[t] = facit_r[t]
%     );

output
    ["{\"objective\": "]   ++ [show(obj)] ++ [",\n"] ++
    [" \"cycles\": "]      ++ [show(c)]   ++ [",\n"] ++
    [" \"instructions\": "]  ++ [show(o)]   ++ [",\n"] ++
    [" \"temporaries\": "] ++ [show(t)]   ++ [",\n"] ++
    [" \"registers\": "]   ++ [show([r[i] | i in 0..MAXT])] ++ [",\n"] ++
    ["}\n"];

solve satisfy;

