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
:- use_module(library(avl)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(codesio)).
:- use_module(library(clpfd)).

:- dynamic
	cur_operation/5,	 % (?Insn,?Type,?Instructions,?Uses,?Defs)
	cur_operand/4,	 % (?Operand,?Insn,?Use,?Temps)
	cur_temp/4.		 % (?Temp,?Width,?Operands,?Definer)

model(AVL, Vars, scalar_product(Freq,OutCs), BBs, CSSpill, Constraints) :-
	model(AVL, Vars, scalar_product(Freq,OutCs), BBs, Frame, CSSpill, Constraints0, []),
	(   foreach(C0,Constraints0),
	    fromto(Constraints,Constraints1,Constraints2,[]),
	    param(Frame)
	do  rewrite(C0, C, Frame),
	    (C==true -> Constraints1 = Constraints2 ; Constraints1 = [C|Constraints2])
	).

model(AVL, Vars, scalar_product(Freq,OutCs), BBs, frame(Mandatory,Instructions), CSSpill) -->
	{Vars = [var(r(-1..MAXT) in (-1..MAXR)), % -1 stands for reg of dead temp
		 var(i(0..MAXO) in (0..MAXI)), % 0 stands for null operation
		 var(c(0..MAXO) in (-1..MAXC)), % -1 stands for cycle of inactive insn
		 var(t(0..MAXOPND) in (-1..MAXT)), % -1 stands for dead temp
		 var(a(0..MAXO) in (0..1)),
		 var(ls(-1..MAXT) in (-1..MAXC)),
		 var(ld(-1..MAXT) in (0..MAXC)),
		 var(le(-1..MAXT) in (-1..MAXC)),
		 array(dur(0..MAXRES,0..MAXI) = DurT),
		 array(con(0..MAXRES,0..MAXI) = ConT)]},
	{avl_fetch('I', AVL, InsnSet)}, % set of insns aka. opcodes
	{avl_fetch('O', AVL, OpSet)}, % set of operations
	{avl_fetch('P', AVL, OpndSet)}, % set of operands
	{avl_fetch('R', AVL, ResSet)}, % set of resources
	{avl_fetch('T', AVL, TempSet)}, % set of temporaries
	{avl_fetch(ops, AVL, Ops)},    % set of insn of block b
	{avl_fetch(tmp, AVL, Tmp)},    % set of temporaries of block b
	{avl_fetch(operands, AVL, Operands)}, % set of operands of op o
	{avl_fetch(use, AVL, Use)}, % whether operand p is a use
	{avl_fetch(temps, AVL, Temps)}, % set of temporary candidates for op p
	{avl_fetch(def_opr, AVL, Definer)}, % insn defining temporary t
	{avl_fetch(strictly_congr, AVL, Congr)},	% whether operands p,q are strictly congruent
	{avl_fetch(adjacent, AVL, Adjacent)},	% adjacency pairs (rematerialization)
	{avl_fetch(aligned, AVL, Aligned)},	% whether operands p,q represent are at a fixed distance
	{avl_fetch(adist, AVL, ADist)},	% shat distance
	{avl_fetch(width, AVL, Width)},	      % width of temporary t
	{avl_fetch(preassign, AVL, Preassign)}, % whether operand p is preassigned to reg r
	{avl_fetch(dep, AVL, Dep)}, % dependency graph of block b
	{avl_fetch(dist, AVL, Dist)}, % min issue distance of ops o1,o2 for insn
	{avl_fetch(freq, AVL, Freq)}, % execution frequency of block b
	{avl_fetch(atoms, AVL, Atoms)}, % atoms within register class s
	{avl_fetch(instructions, AVL, Instructions)}, % set of instructions for op o
	{avl_fetch(class, AVL, Class)},	% reg class for op,i,p
	{avl_fetch(domuses, AVL, Domuses)}, % mandatory temporary reuse
	{avl_fetch(cap, AVL, Cap)},	% capacity of resource r
	{avl_fetch(con, AVL, Con)},	% consumption of resource r by op
	{avl_fetch(dur, AVL, Dur)},	% duration of use of resource r by op
	{avl_fetch(maxc, AVL, MaxC)}, % maximum issue cycle of basic block
	{avl_fetch(type, AVL, Type)}, % see type/2
	{avl_fetch(callersaved, AVL, CallerSaved)},
	{avl_fetch(calleesaved, AVL, CalleeSaved)},
	{avl_fetch(calleesaved_spill, AVL, CalleeSavedSpill)},
	{avl_fetch(activators, AVL, Activators)},
	{list_to_fdset(CallerSaved, CSSet)},
	{list_to_fdset(CalleeSaved, CEESSet)},
	{fdset_to_range(CSSet, CSRange)},
	{fdset_to_range(CEESSet, CEESRange)},
	%% presolver additions
	% live range precedences
	{avl_fetch(before, AVL, Before1)},
	{avl_fetch(before2, AVL, Before2)},
	{append(Before1, Before2, Before)},
	% nogoods
	{avl_fetch(nogoods, AVL, Nogoods)},
% 	{avl_fetch(nogoods2, AVL, Nogoods2)},
% 	{append(Nogoods1, Nogoods2, Nogoods)},
	% dominated move insns
	{avl_fetch(dominates, AVL, Dominates)},
	% maximum temporary usage
	{avl_fetch(maxtemp, AVL, MaxTemp)},
	% sets of potential no-caller-save temporaries
	{avl_fetch(across, AVL, Across)},
	{avl_fetch(set_across, AVL, SetAcross)},
	% sets of operands with different temps
	{avl_fetch(difftemps, AVL, DiffTemps)},
	% sets of operands with different regs
	{avl_fetch(diffregs, AVL, DiffRegs)},
	% sets of [Opnds,Temps] domination sets
	{avl_fetch(domops, AVL, DomOps)},
	% min number of copies per basic block
	{avl_fetch(optional_min, AVL, OptionalMin)},
	% preassigned memory locations
	{avl_fetch(memassign, AVL, MemAssign)},
	% min live range length if in use
	{avl_fetch(minlive, AVL, MinLive)},
	% whether may be live past last use
	{avl_fetch(unsafe_temp, AVL, UnsafeTemp)},
	% tables of valid combinations for combinations of a(_) variables
	{avl_fetch(active_tables, AVL, ActiveTables)},
	% tables of valid combinations for combinations of a(_) and t(_) variables
	{avl_fetch(tmp_tables, AVL, TmpTables)},
	% last use operands
	{avl_fetch(last_use, AVL, LastUse)},
	% cond. precedences
	{avl_fetch(precedences, AVL, Precedences1)},
	{avl_fetch(precedences2, AVL, Precedences2)},
	{append(Precedences1, Precedences2, Precedences)},
	% predecessors and successors
	{avl_fetch(predecessors, AVL, Predecessors)},
	{avl_fetch(successors, AVL, Successors)},
	% quasi-adjacent
	{avl_fetch(quasi_adjacent, AVL, QuasiAdjacent)},
	%% end presolver additions
	{transpose(Dur, DurT)},
	{transpose(Con, ConT)},
	{max_member(MAXI, InsnSet)},
	{max_member(MAXOPND, OpndSet)},
	{max_member(MAXC, MaxC)},
	{max_member(MAXO, OpSet)},
	{max_member(MAXT, TempSet)},
	{max_member(MAXRES, ResSet)},
	{max_member(MAXWIDTH, Width)},
	{   foreach(Atomsi,Atoms),
	    fromto(0,MP1,MP3,MAXR)
	do  last(Atomsi, MP2),
	    MP3 is max(MP1,MP2)
	},
	{assert_json(Operands, Instructions, Type, Use, Temps, Width, Definer)},
	% OBJECTIVE
	{   foreach(Ops0,Ops),
	    foreach(c(Ops0n),OutCs)
	do  last(Ops0, Ops0n)
	},
	% DOMAINS
	[r(-1) #= -1],		% [MC] technicality
	[ls(-1) #= -1],		% [MC] technicality
	[ld(-1) #= 0],		% [MC] technicality
	[le(-1) #= -1],		% [MC] technicality
	(   foreach(Insns0,Instructions),
	    count(I0,0,_),
	    fromto(Mandatory,Mand1,Mand2,[])
	do  {   Insns0 = [0|_] -> Mand1 = Mand2
	    ;   Mand1 = [I0|Mand2]
	    }
	),
	(   foreach(M3,Mandatory)
	do  [a(M3) #= 1]
	),
	% CONSTRAINTS
	core_constraints(CallerSaved, Atoms, Congr, Adjacent, Dominates, Aligned, ADist, Nogoods,
			 Preassign, DiffTemps, DiffRegs, DomOps, QuasiAdjacent,
			 MAXO, MAXOPND),
	constrain_definer_reg(MAXOPND),
	% hypothesis: the following is useful for the decomposition solver only
	% corollaries(Dominates),
	(   for(T1,0,MAXT),
	    param(Use,MAXO,MinLive,UnsafeTemp,MemAssign)
	do  {cur_temp(T1, _, [_|T1Uses], D1)},
	    {cur_operand(D1, Ty0, _, _, _)},
	    {   foreach(U1,Use),
		fromto(0,Rank1,Rank2,_Rank3),
		count(P3,0,_),
		param(T1)
	    do  cur_operand(P3, _, _, Temps1),
	        (   U1=:=1, member(T1, Temps1) % potentially using T1
		->  Rank2 is Rank1-1
		;   Rank2 is Rank1
		)
	    },
				% constraint: live start
	    /***
	    % before rematerialization
	    [ls(T1) #= c(D1)],
	    ***/
	    % rematerialization
	    (   {Ty0=in}
	    ->  [ls(T1) in -1..0, r(T1) #>= 0 #<=> ls(T1) #= 0]
	    ;   [ls(T1) #= c(D1)]
	    ),
	    [ls(T1) + ld(T1) #= le(T1)],
	    {   foreach(T1Use,T1Uses),
		foreach(if(t(T1Use) #= T1,c(I1),-1),UserIssueCycles),
		param(T1)
	    do  cur_operand(T1Use, I1, _, _)
	    },
	      			% constraint: live end
				% crucial for bsPutUInt32!!!
	    (   {member(T1, UnsafeTemp)}
	    ->  {nth0(T1, MinLive, LDLB)},
		[let([lb],[lb #= c(D1)+LDLB*bool2int(a(D1)),
			   maximum(le(T1),[lb|UserIssueCycles])])]
	    ;   {UserIssueCycles = [IC]}
	    ->  [le(T1) #= IC]
	    ;   [maximum(le(T1),UserIssueCycles)]
	    )
	),
	(   foreach(Tmp1,Tmp)
	do  (   foreach(T2,Tmp1),
		foreach(rect(r(T2),W2,ls(T2),ld(T2),le(T2)),Rects)
	    do  {cur_temp(T2, W2, _, _)}
	    ),
	    (   {Rects=[]} -> []
	    ;   {Rects=[_]} -> []
	    ;   [disjoint2(Rects)] % constraint: disjoint live ranges
	    )
	),
	instruction_constraints(Atoms, Class, Domuses, LastUse, MAXR,
				Preassign, MinLive, MemAssign),
	% [MC] order live ranges for intra-block congruences
	(   foreach([P,Q,Json],Before),
	    param(CSRange,Atoms)
	do  [Cond #=> le(t(P)) #=< ls(t(Q))],
	    {translate_condition(Json, Cond, CSRange, Atoms)}
	),
	% [RCAS] max temporary usage
	(   foreach([Ps4,T4,UB],MaxTemp)
	do  {   foreach(P1,Ps4),
		foreach(t(P1) #= T4,Eqs),
		param(T4)
	    do  true
	    },
	    [sum(Eqs) #=< UB]
	),
	% single table constraint for durations & resource consumptions
	dur_con(Dur, Con, MAXO),
	% BASIC BLOCKS - bb(Irange,OPNDrange,Trange,Reals,Calls,MaxC,OptionalMin,Optional,COrder) per bb
	(   foreach(Ops1,Ops),
	    foreach(Tmp2,Tmp),
	    foreach(M1,MaxC),
	    foreach(M2,OptionalMin),
	    foreach(bb(Isucc..Imax,OPNDsucc..OPNDmax,Tsucc..Tmax,Reals,Calls,M1,M2,Optional,COrder),BBs),
	    fromto(bb(-1,-1,-1),bb(Iprev,OPNDprev,Tprev),bb(Imax,OPNDmax,Tmax),_),
	    param(Operands,CSRange,Atoms,CEESRange)
	do  {Isucc is Iprev+1},
	    {OPNDsucc is OPNDprev+1},
	    {Tsucc is Tprev+1},
	    {last(Ops1, Imax)},
	    {   Tmp2 = []	% no temps, no opnds?
	    ->  Tmax is Tsucc-1,
		OPNDmax is OPNDsucc-1
	    ;   last(Tmp2, Tmax),
		prefix_length(Operands, OpPartA, Isucc),
		append(OpPartA, OpPartBC, Operands),
		NI is Imax-Iprev,
		prefix_length(OpPartBC, OpPartB, NI),
		append(OpPartB, OpFlatB),
		last(OpFlatB, OPNDmax)
	    },
	    {   for(K,Isucc,Imax),
		foreach(OutFlow3-K,CO1),
		foreach(_-_K2,CO2),
		foreach(K/**K2**/,COrder), % no heuristic COrder for now
		foreach(OpsK,OpPartB),
		fromto(Calls,Calls1,Calls2,[]),
		fromto(Reals,Reals1,Reals2,[])
	    do  cur_operation(K, TypeName, _, _, _),
		(TypeName=call -> Calls1 = [K|Calls2] ; Calls1 = Calls2),
		(real_type(TypeName) -> Reals1 = [K|Reals2] ; Reals1 = Reals2),
		(   foreach(OpK,OpsK),
		    fromto(0,OutFlow1,OutFlow2,OutFlow3)
		do  cur_operand(OpK, _, UseK, TempKs),
		    delete_null(TempKs, [TempK|_]),
		    cur_temp(TempK, WK, _, _),
		    OutFlow2 is OutFlow1+(-2*UseK+1)*WK
		)
	    },
	    {keysort(CO1, CO2)},
	    [c(Isucc) #= 0],	% [MC] fix start of bb
	    {   for(A0,Isucc,Imax),
		fromto(Optional,Optional1,Optional2,[])
	    do  (   cur_operation(A0, _, [0|_], _, _) % optional
		->  Optional1 = [A0|Optional2]
		;   Optional1 = Optional2
		)
	    },
				% redundant cumulative for caller-saved
	    live_range_extra(Tsucc, Tmax, CEESRange)
	),
	% [MC] valid combinations of a(_) variables
	active_table_constraints(ActiveTables, TmpTables),
	% [MC] valid combinations of a(_) and t(_) variables
	tmp_table_constraints(TmpTables),
	% [MC] alldiffs at call sites
	across_call_constraints(Across, CallerSaved, CSRange, Atoms, MAXWIDTH),
	set_across_call_constraints(SetAcross, CallerSaved, CSRange),
	memory_domination_constraints(Atoms),
	precedences_and_resources(MAXO, Precedences,
				  Dep, Dist, Instructions, Ops, ResSet, DurT, Cap, CSRange, Atoms),
	callee_saved_spill(CalleeSavedSpill, CSSpill, Ops),
	activators(Activators, Instructions),
				% predecessors
	(   foreach([Preds,Succ,Lat1],Predecessors)
	do  [let([pmin], [minimum(pmin,CPreds), pmin + Lat1 #=< c(Succ)])],
	    {   foreach(P2,Preds),
		foreach(c(P2),CPreds)
	    do  true
	    }
	),
	(   foreach([Pred,Succs,Lat2],Successors)
	do  [let([smax], [maximum(smax,CSuccs), c(Pred) + Lat2 #=< smax])],
	    {   foreach(S1,Succs),
		foreach(c(S1),CSuccs)
	    do  true
	    }
	).

% for the callee-saved:
% 1. per set of spill/unspill insns: decreasing cycle order
% 2. if basic block 1 is returning (spills and unspills):
%    all spills must precede all unspills, otherwise spilling is pointless
callee_saved_spill([], _, _) --> !.
callee_saved_spill(CalleeSavedSpill, Spill, [Ops0|_]) -->
	(   foreach([I1|Is],CalleeSavedSpill),
	    foreach(I1,Spill)
	do  (   foreach(I2,Is),
		param(I1)
	    do  [a(I1) #= a(I2)]
	    )
	),
	{transpose(CalleeSavedSpill, Transpose)},
	(   foreach(Row,Transpose)
	do  (   foreach(Y,Row),
		fromto(100000,X,c(Y),_)
	    do  [X #>= c(Y)]
	    )
	),
	(   {Transpose = [Spill,Unspill|_]},
	    {ord_subset(Spill, Ops0)},
	    {ord_subset(Unspill, Ops0)}
	->  {Spill = [S|_]},
	    (   foreach(U,Unspill),
		param(S)
	    do  [a(U) #=> c(S) #< c(U)]
	    )
	;   []
	).

activators(Activators, Instructions) -->
	(   foreach(Insns1,Activators),
	    foreach(Insns2-I3,KL1),
	    count(I3,0,_)
	do  {sort(Insns1,Insns2)}
	),
	{keysort(KL1, KL2)},
	{keyclumped(KL2, KL3)},
	(   foreach(Insns3-Ops3,KL3),
	    param(Instructions)
	do  (   {Insns3 = []} -> []
	    ;   {Ops3 = [H|T]},
		(   foreach(Y,T),
		    param(H)
		do  [a(H) #<=> a(Y)]
		),
		{list_to_fdset(Insns3, InsnsFD)},
		{fdset_to_range(InsnsFD, InsnsR)},
		(   foreach(Dom,Instructions),
		    count(O,0,_),
		    fromto(Ctrs1,Ctrs2,Ctrs3,[]),
		    param(Insns3,InsnsR)
		do  (   {ord_intersect(Dom,Insns3)}
		    ->  {Ctrs2 = [i(O) in InsnsR|Ctrs3]}
		    ;   {Ctrs2 = Ctrs3}
		    )
		),
		[sum(Ctrs1) #>= 1 #<=> a(H)]
	    )
	).

constrain_definer_reg(MAXOPND) -->
	{   for(P0,0,MAXOPND),
	    fromto(KL1,KL2,KL4,[]),
	    count(P,0,_)
	do  cur_operand(P0, _, Use1, Temps1),
	    (   Use1=:=0 -> KL2 = KL4
	    ;   (   foreach(T,Temps1),
		    fromto(KL2,[T-P|KL3],KL3,KL4),
		    param(P)
		do  true
		)
	    )
	},
	{keysort(KL1, KL5)},
	{keyclumped(KL5, KL6)},
	{KL6 = [-1-_|KL7]},
	(   foreach(T1-Ps,KL7)
	do  (   {Ps = [_,_|_]}
	    ->  [all_eq_or_joker(RTs, r(T1))],
		{   foreach(Q,Ps),
		    foreach(if(t(Q) #= T1, r(t(Q)), -1), RTs),
		    param(T1)
		do  true
		}
	    ;   []
	    )
	).


live_range_extra(Tfirst, Tlast, Range) -->
	(   for(O,Tfirst,Tlast),
	    foreach(task(ls(O),ld(O),le(O),if(r(O) in Range,W,0)), Tasks),
	    param(Range)
	do  {cur_temp(O, W, _, _)}
	),
	{range_to_fdset(Range, Set)},
	{fdset_size(Set, Size)},
	[live_cumulative(Tasks,Size)].

active_table_constraints(ActiveTables, TmpTables) -->
	{   foreach([ASet1,_,_],TmpTables),
	    foreach(ASet2,ASets)
	do  (   foreach(A,ASet1),
		foreach(a(A),ASet2)
	    do  true
	    )
	},
	{ord_union(ASets, TmpTableVars)},
	{   foreach([Set,Table],ActiveTables),
	    foreach(Table-ASet,KL1)
	do  (   foreach(X,Set),
		foreach(a(X),ASet)
	    do  true
	    )
	},
	{keysort(KL1, KL2)},
	{keyclumped(KL2, KL3)},
	(   foreach(Table1-Sets,KL3),
	    param(TmpTableVars)
	do  (   {Table1 = [[1]]}
	    ->  (   foreach([a(I1)],Sets)
		do  [a(I1) #= 1]
		)
	    ;   {Table1 = [[0,0],[1,1]]}
	    ->  (   foreach([a(I2),a(I3)],Sets)
		do  [a(I2) #= a(I3)]
		)
	    ;   {at_least_one_table(Table1)}
	    ->  (   foreach(Set1,Sets)
		do  [sum(Set1) #>= 1]
		)
	    ;   {   foreach(ASet3,Sets),
		    param(TmpTableVars)
		do  ord_subset(ASet3, TmpTableVars)
		}
	    ->  []		% completely subsumed by tmp_tables
	    ;   [table(Sets,Table1)]
	    )
	).

tmp_table_constraints(TmpTables) -->
	(   foreach([As,Ts,Table],TmpTables)
	do  [table([Vars1],Table)],
	    {   foreach(A,As),
		fromto(Vars1,[a(A)|Vars2],Vars2,Vars3)
	    do  true
	    },
	    {   foreach(T,Ts),
		foreach(t(T),Vars3)
	    do  true
	    }
	).

% true if the relation is equivalent to a big OR
at_least_one_table(Table) :-
	Table = [Row|_],
	last(Row, 1),
	length(Row, N),
	length(Table, M),
	(2**N)-1 =:= M.

    % forall(p in 0..MAXP where not operand_use[p])(
    %     let {int: v = max(operand_temps[p])} in (
    %         t[p]>=0 <-> member([t[q] | q in 0..MAXP where can_use(q,v)], v)
    %     )
    % )
temp_distribution_constraints(MAXOPND) -->
	(   for(P,0,MAXOPND)
	do  (   {cur_operand(P, _, 0, Ts)}
	    ->  {last(Ts, V)},
		{findall(t(Q)#=V, (cur_operand(Q, _, 1, Us), memberchk(V, Us)), Eqs)},
		{disjify(Eqs, Disj)},
		(   {Ts = [-1|_]} -> [t(P)#>=0 #<=> Disj]
		;   {Eqs = [_]} -> []
		;   [Disj]
		)
	    ;   []
	    )
	).

% % Hairy, expensive and incomplete
% temp_distribution_constraints(TmpTables, MAXOPND) -->
% 	{   foreach([_,TSet1,_],TmpTables),
% 	    foreach(TSet2,TSets)
% 	do  (   foreach(T,TSet1),
% 		foreach(t(T),TSet2)
% 	    do  true
% 	    )
% 	},
% 	{ord_union(TSets, TmpTableVars)},
% 	{findall(V, cur_operand(_,_,0,[-1,V]), TOpt1)}, % find all optional temps
% 	{sort(TOpt1, TOpt)},
% 	{   for(P0,0,MAXOPND),
% 	    fromto(UsedBy1,UsedBy2,UsedBy5,[])
% 	do  cur_operand(P0, _, U0, Temps),
% 	    delete_null(Temps, [Torig|Tcopies]),
% 	    (   U0=:=0
% 	    ->  UsedBy2 = UsedBy5
% 	    ;   UsedBy2 = [Torig-p(P0)|UsedBy3],
% 		(   foreach(T0,Tcopies),
% 		    fromto(UsedBy3,[Torig-t(T0)|UsedBy4],UsedBy4,UsedBy5),
% 		    param(Torig)
% 		do  true
% 		)
% 	    )
% 	},
% 	{keysort(UsedBy1, UsedBy6)},
% 	{keyclumped(UsedBy6, UsedBy7)},
% 	(   foreach(Orig-PsTs,UsedBy7),
% 	    param(TmpTableVars,TOpt)
% 	do  {   foreach(PT,PsTs),
% 		fromto(TempVars1,TempVars2,TempVars3,[]),
% 		fromto(VAs1,VAs2,VAs3,[])
% 	    do  (   PT = p(O)
% 		->  TempVars2 = [t(O)|TempVars3], VAs2 = VAs3
% 		;   PT = t(O)
% 		->  TempVars2 = TempVars3, VAs2 = [O-a(J)|VAs3],
% 		    cur_temp(O, _, _, J)
% 		)
% 	    },
% 	    {sort(VAs1, VAs4)},
% 	    {   ord_member(Orig, TOpt)
% 	    ->  cur_temp(Orig, _, [D|_], I),
% 		cur_operation(I, Type, _, _, _)
% 	    ;   Type = mand
% 	    },
% 	    (   {VAs4=[]} -> []
% 	    ;   {ord_intersect(TempVars1, TmpTableVars)} -> []
% 	    ;   {Type = mand} -> [temp_cardinality(TempVars1, [Orig-1|VAs4])],
% 		{print_message(warning,temp_cardinality(TempVars1, [Orig-1|VAs4]))}
% 	    ;   {Type = in} -> [temp_cardinality(TempVars1, [Orig-(t(D)#>=0)|VAs4])],
% 		{print_message(warning,temp_cardinality(TempVars1, [Orig-(t(D)#>=0)|VAs4]))}
% 	    ;   [temp_cardinality(TempVars1, [Orig-a(I)|VAs4])],
% 		{print_message(warning,temp_cardinality(TempVars1, [Orig-a(I)|VAs4]))}
% 	    )
% 	).


register_set(Op, Atoms, RegSet) :-
	nth0(Op, Atoms, AtomsOp),
	list_to_fdset(AtomsOp, RegSet).

across_call_constraints(Across, CallerSaved, CSRange, Atoms, MaxW) -->
	(   foreach([O,Extra,Clump],Across),
	    param(CallerSaved,CSRange,Atoms,MaxW)
	do  (   foreach([T1,Disj1],Clump),
		foreach(item(if(Disj4,r(T1),NT1),W),Terms),
		param(O,CSRange,Atoms,MaxW)
	    do  {translate_condition(Disj1, Disj3, CSRange, Atoms)},
		{Default = (ls(T1) #=< c(O) #/\ le(T1) #> c(O)
			   /** #/\ #\ (r(T1) in CSRange) -- why was it here? **/
			   )},
		{   Disj3=true
		->  Disj4=true
		;   Disj3=false
		->  Disj4=Default
		;   Disj4=(Disj3#\/Default)
		},
		{NT1 is (-T1-1)*MaxW},
		{cur_temp(T1, W, _, _)}
	    ),
	    (   foreach(R,Extra),
		foreach(item(R,1),Extras)
	    do  []
	    ),
	    {caller_saved_items(CallerSaved, CSRange, CSItems, [])},
	    {append([CSItems,Extras,Terms], Items)},
	    [disjoint1(Items)]
	).

set_across_call_constraints(Across, CallerSaved, CSRange) -->
	(   foreach([O,Extra,Clump],Across),
	    param(CallerSaved,CSRange)
	do  (   foreach(Set1,Clump),
		foreach(Item,Terms),
		fromto(Qs1,Qs2,Qs3,[]),
		fromto(Ctrs1,Ctrs2,Ctrs3,[disjoint1(Items)]),
		param(O)
	    do  {Set1 = [T1|_]},
		{set_across_expression(Set1, O, T1, Item, Qs2, Qs3, Ctrs2, Ctrs3)}
	    ),
	    (   foreach(R,Extra),
		foreach(item(R,1),Extras)
	    do  []
	    ),
	    {caller_saved_items(CallerSaved, CSRange, CSItems, [])},
	    {append([CSItems,Extras,Terms], Items)},
	    [let(Qs1,Ctrs1)]
	).

caller_saved_items(_, Min..Max) --> !, [item(Min,Len)],
	{Len is Max-Min+1}.
caller_saved_items(CallerSaved, _) -->
	(   foreach(R,CallerSaved)
	do  [item(R,1)]
	).

set_across_expression([T], _, _, item(r(T),W), Qs, Qs) --> !,
	{cur_temp(T, W, _, _)}.
set_across_expression(Set, O, P, item(rs(P),Width), [is(P),rs(P),lss(P),les(P),ws(P)|Qs], Qs) -->
	{   foreach(X,Set),
	    foreach(r(X),RSet),
	    foreach(ls(X),LSSet),
	    foreach(le(X),LESet),
	    foreach(W, WSet)
	do  cur_temp(X, W, _, _)
	},
	[dc_element(is(P), RSet, rs(P))],
	(   {sort(WSet, [W1])} -> {Width = W1}
	;   {Width = ws(P)},
	    [element(is(P),  WSet,  ws(P))]
	),
	[element(is(P), LSSet, lss(P))],
	[element(is(P), LESet, les(P))],
	[lss(P) #=< c(O), les(P) #> c(O)].

translate_condition(Disj1, Disj3, CSRange, Atoms) :-
	(   foreach(Conj1,Disj1),
	    foreach(Conj3,Disj2),
	    param(CSRange,Atoms)
	do  (   foreach([Tag|Args],Conj1),
		foreach(Cond,Conj2),
		param(CSRange,Atoms)
	    do  translate_lit(Tag, Args, Cond, CSRange, Atoms)
	    ),
	    conjify(Conj2, Conj3)
	),
	disjify(Disj2, Disj3).

translate_lit(0, [P,Q], t(P)#=t(Q), _, _).
translate_lit(1, [P,T], t(P)#=T, _, _).
translate_lit(2, [O], a(O), _, _).
translate_lit(3, [O,I], i(O)#=I, _, _).
translate_lit(4, [P,Q], (ls(t(P)) #< le(t(Q)) #/\ ls(t(Q)) #< le(t(P))), _, _).
translate_lit(5, [T,U], (ls(T) #< le(U) #/\ ls(U) #< le(T)), _, _).
translate_lit(6, [T], r(T) in CSRange, CSRange, _).
translate_lit(7, [O,I], i(O)#\=I, _, _).
translate_lit(8, [P,C], r(t(P)) in Range, _, Atomes) :-
	nth0(C, Atomes, Atom),
	list_to_fdset(Atom, Set),
	fdset_to_range(Set, Range).

core_constraints(CallerSaved, Atoms, Congr, Adjacent, Dominates, Aligned, ADist, Nogoods,
		 Preassign, DiffTemps, DiffRegs, DomOps, QuasiAdjacent, MAXO, MAXOPND) -->
	/***
	% before rematerialization
	(   for(P0,0,MAXOPND)
	do  {cur_operand(P0, D1, U, TempsP0)},
	    {delete_null(TempsP0, TempsP)},
	    (   {U=:=1} ->
		[t(P0) in TempsPR],
		{list_to_fdset(TempsP0,TempsPS)},
		{fdset_to_range(TempsPS,TempsPR)}
	    ;   {TempsP = [T0]} ->
		[t(P0) #= T0],
		[r(T0) #>= 0 #<=> a(D1)] % live reg constraint
	    )
	),
	***/
	% rematerialization
	(   for(P0,0,MAXOPND)
	do  {cur_operand(P0, D1, U, TempsP)},
	    {cur_operation(D1, Ty1, [Ins|_], _, _)},
	    [t(P0) in TempsPR],
	    {list_to_fdset(TempsP,TempsPS)},
	    {fdset_to_range(TempsPS,TempsPR)},
	    (   {Ins=:=0} -> [t(P0) #>= 0 #<=> a(D1)] % opt. operation
	    ;   {Ty1=in} -> []
	    ;   {U=:=1} -> []	% use
	    ;   [r(t(P0)) #>= 0 #<=> a(D1)] % mand. def except (in)
	    )
	),
	(   for(O,0,MAXO)
	do  {cur_operation(O, Ty2, _, Us, Ds)},
	    /***
	    % before rematerialization
	    (   {Ty2 = out} -> []
	    ;   (   foreach(P,Us),
		    param(O)
		do  [t(P) #>= 0 #<=> a(O)]
		)
	    ),
	    ***/
	    (   {Ty2 = copy} -> % special for (copy)
		{Us = [Pu|_], Ds = [Pd|_]},
		[a(O) #=> r(t(Pu)) #\= r(t(Pd))] % constraint: effective copy
	    ;   []
	    )
	),
	% constraint: congruence, already OK for rematerialization
	(   foreach(C4,Congr)
	do  {C4 = [P4|Q4s]},
	    (   foreach(Q4,Q4s),
		param(P4)
	    do
	        [r(t(P4)) #= r(t(Q4))]
	    )
	),
	% handle adjacent, optional operands
	(   foreach([P5,Q5],Adjacent)
	    % foreach(P5-(t(Q5)#>=0),KL5)
	do  {cur_operand(P5, _, _, PTs5)},
	    (   {PTs5 = [-1|_]}
	    ->  [t(Q5) #>= 0 #=> t(P5) #>= 0 #/\ r(t(P5)) #= r(t(Q5))]
	    ;   []
	    )
	),
	% TEMP DISABLED, replacing by "quasi_adjacent" -- lost nothing, it seems
	% {keysort(KL5, KL6)},
	% {keyclumped(KL6, KL7)},
	% (   foreach(Pout-Qsa,KL7)
	% do  {disjify([t(Pout) #= -1 | Qsa], Disj)}, [Disj],
	%     {cur_operand(Pout, _, _, Tsout)},
	%     (   {Tsout = [-1,T1|T2s]}, % optional (out) operand Pout
	% 	{cur_temp(T1, _, [Pin|_], _)},
	% 	{cur_operand(Pin, Oin, _, [-1|_])}, % inhering value from optional (in) operand Pin
	% 	{cur_operation(Oin, in, _, _, _)}
	%     ->  {   foreach(T2,T2s),
	% 	    fromto(Tos1,Tos2,Tos3,[])
	% 	do  cur_temp(T2, _, _, Oc),
	% 	    cur_operation(Oc, copy, _, _, _)
	% 	->  Tos2 = [T2|Tos3]
	% 	;   Tos2 = Tos3
	% 	},
	% 	{list_to_fdset([T1|Tos1], TosS)},
	% 	{fdset_to_range(TosS, TosR)},
	% 	[t(Pout) in TosR #=> t(Pin) #>= 0]
	%     ;   []
	%     )
	% ),
	% constraint: aligned operand component, MOD for rematerialization
	(   foreach([P6,Pop,Q6,Qop],Aligned),
	    foreach(Offset,ADist)
	do  {cur_operand(P6, I6, _, _)},
	    {cur_operand(Q6, J6, _, _)},
	    [i(I6) #= Pop #/\ i(J6) #= Qop #=> r(t(P6)) #= r(t(Q6)) + Offset]
	),
	% constraint: pre-assignment
	(   foreach([P7,R7],Preassign)
	do  [r(t(P7)) #= R7]
	),
	(   foreach([I8,J8,ODiff8,TDiff8],Dominates)
	do  {list_to_fdset([0|ODiff8], OS8)},
	    {fdset_to_range(OS8, OR8)},
	    {list_to_fdset([-1|TDiff8], TS8)},
	    {fdset_to_range(TS8, TR8)},
	    {cur_operation(J8, _, _, [P8|_], _)},
	    {   ODiff8=[], TDiff8=[]
	    ->  C8 = (a(I8) #\/ #\ a(J8))
	    ;   ODiff8=[]
	    ->  C8 = (a(I8) #\/ t(P8) in TR8)
	    ;   TDiff8=[]
	    ->  C8 = (a(I8) #\/ i(J8) in OR8)
	    ;   true
	    ->  C8 = (a(I8) #\/ i(J8) in OR8 #\/ t(P8) in TR8)
	    },
	    [C8]
	),
	% [MC] forbidden tuples of temp choices
	{list_to_fdset(CallerSaved, CSSet)},
	{fdset_to_range(CSSet, CSRange)},
	(   foreach(Nogood,Nogoods),
	    param(CSRange,Atoms)
	do  {translate_condition([Nogood], Conj, CSRange, Atoms)},
	    [#\ Conj]
	),
	% [MC] sets of different temporaries
	(   foreach(DTs,DiffTemps)
	do  {   foreach(O1,DTs),
		foreach(t(O1),Ts)
	    do  true
	    },
	    [all_different(Ts)]
	),
	% [MC] sets of nonoverlapping registers
	(   foreach(DRs,DiffRegs)
	do  {   foreach(O2,DRs),
		foreach(r(t(O2)),RTs)
	    do  true
	    },
	    noverlap(RTs)
	),
	findall(NG, temp_domination_nogood(DomOps, NG)),
	% [MC] ensure matching def/use
	temp_distribution_constraints(MAXOPND),
	(   foreach([Then,If],QuasiAdjacent)
	do  [t(If) #>=0 #=> t(Then) #>= 0]
	).

corollaries(Dominates) -->
	(   foreach([I,J,Oset,Tset],Dominates)
	do  (   {Oset = [],
		 Tset = [],
		 cur_operation(I, _, _, _, [PI|_]),
		 cur_operation(J, _, _, _, [PJ|_]),
		 cur_operand(PI, _, _, OpsI),
		 delete_null(OpsI, [TI]),
		 cur_operand(PJ, _, _, OpsJ),
		 delete_null(OpsJ, [TJ]),
		 cur_operand(PU, _, _, [TD,TI,TJ])}
	    ->  [r(t(PU)) #\= r(TD) #=> a(I)]
	    ;   []
	    )
	).

temp_domination_nogood(DomOps, t(P1)#\=T1 #\/ t(P2)#\=T2) :-
	member([Ops,Temps], DomOps),
	suffix(Temps, [T2|Temps1]),
	member(T1, Temps1),
	suffix(Ops, [P1|Ops1]),
	member(P2, Ops1).

instruction_constraints(Atoms, Class, Domuses, LastUse, MAXR, Preassign,
			MinLive, MemAssign) -->
	(   foreach(ClassI,Class),
	    count(O,0,_),
	    param(Atoms,MAXR,LastUse,MinLive,MemAssign)
	do  [i(O) in OpsRange], % i(_) domains
	    {cur_operation(O, TypeName, Ops, Us, Ds)},
	    {append(Us, Ds, Opnds)},
	    {list_to_fdset(Ops, OpsSet)},
	    {fdset_to_range(OpsSet, OpsRange)},
				% constraint: null operation
	    [i(O) #>= 1 #<=> a(O)],
	     			% constraint: null issue cycle [MC]
	    [c(O) #>= 0 #<=> a(O)],
	    {   Opnds = [] -> true
	    ;   transpose(ClassI, ClassIT)
	    },
	    (   foreach(P,Opnds),
		foreach(ClassITOpnd,ClassIT),
		param(O,Ops,Atoms,MAXR,LastUse,MinLive,MemAssign)
	    do  {cur_operand(P, _, U, TPs)},
		(   {U=:=1} ->	% REALLY has effect!!!
				% constraint: use precedes live end [MC presolved]
		    % 20130930 unsafe -- live range can extend past last use
		    % (   {ord_member(P, LastUse)}
		    % ->  [le(t(P)) #=  c(O)]
		    % ;   [le(t(P)) #>= c(O)]
		    % )
		    [le(t(P)) #>= c(O)]
		;   % [MC] 20130531: live range >= latency
		    {delete_null(TPs, [TP])},
		    {nth0(TP, MinLive, LDLB)},
		    [a(O) #=> ld(TP) #>= LDLB]
		),		% constraint: operation selection for operands
		operand_registers(P, O, MAXR, ClassITOpnd, Ops, Atoms, MemAssign)
	    ),
	    (   {TypeName = low}
	    ->  {Opnds = [Src,Def]},
				% 20130424: redundant live ranges for low/high
		[minimum(c(O), [le(t(Src)),le(t(Def))])]
	    ;   {TypeName = high}
	    ->  {Opnds = [Src,Def]},
				% 20130424: redundant live ranges for low/high
		[minimum(c(O), [le(t(Src)),le(t(Def))])]
	    ;   {TypeName = combine}
	    ->  {Opnds = [Srca,Srcb,Def]},
				% 20130424: redundant live ranges for combine
		[minimum(c(O), [le(t(Srca)),le(t(Def))])],
		[minimum(c(O), [le(t(Srcb)),le(t(Def))])]
	    ;   []
	    )
	),
	domuses(Domuses, Atoms, Class, LastUse, Preassign).

% avoid if-conditions on single opcode, if possible
operand_registers(P, O, MAXR, ClassITOpnd, Ops, Atoms, MemAssign) -->
	{   cur_operand(P, _, 0, [-1,T]),
	    member([T,R], MemAssign)
	->  Cookie = R
	;   Cookie = -1
	},
	{   foreach(Op,Ops),
	    foreach(ClassITOpndOper,ClassITOpnd),
	    foreach(RegSet,RegSets),
	    fromto(KL1,KL2,KL3,[]),
	    param(Atoms,Cookie,MAXR)
	do  (   Op=:=0
	    ->  RegSet = [[-1|-1]]
	    ;   register_set(ClassITOpndOper, Atoms, RegSet0),
		(   fdset_member(Cookie, RegSet0)
		->  fdset_singleton(RegSet, Cookie)
		;   RegSet = RegSet0
		)
	    ),
	    (   Op>0,
		fdset_to_range(RegSet, RegRange),
		RegRange \== (0..MAXR)
	    ->  KL2 = [RegRange-Op|KL3]
	    ;   KL2 = KL3
	    )
	},
	(   {Ops = [_]} ->  []
	;   {keysort(KL1, KL4)},
	    {keyclumped(KL4, KL5)},
	    (   foreach(RRange-Clump,KL5),
		param(P,O)
	    do  {list_to_fdset(Clump, OpSet)},
		{fdset_to_range(OpSet, OpRange)},
		[i(O) in OpRange #=> r(t(P)) in RRange]
	    )
	),
	{fdset_union(RegSets, RegUnion)},
	{fdset_to_range(RegUnion, RegUnionRange)},
	(   {RegUnionRange \== (-1..MAXR)},
	    {RegUnionRange \== (0..MAXR)}
	->  [r(t(P)) in RegUnionRange]
	;   []
	).

domuses(Domuses, Atoms, Class, LastUse, Preassign) -->
	(   foreach([P,Q,R],Domuses),
	    param(Atoms,Class,LastUse,Preassign)
	do  {cur_operand(R, _, _, Ts)},
	    {delete_null(Ts, [T])},
	    {cur_operand(P, O, _, _)},
	    {cur_operation(O, TypeName, Ops, Us, Ds)},
	    {append(Us, Ds, Opnds)},
	    (   {pack_type(TypeName)} -> [] % (combine),(low),(high)
	    ;   {ord_member(P, LastUse)} -> []
	    ;   {member([P,_], Preassign)} -> []
	    ;   {nth0(O, Class, ClassI)},
		{transpose(ClassI, ClassIT)},
		{   nth0(J, Opnds, P)
		->  nth0(J, ClassIT, ClassITOpnd)
		},
		{   foreach(Op,Ops),
		    foreach(ClassITOpndOper,ClassITOpnd),
		    fromto(KL1,KL2,KL3,[]),
		    param(Atoms)
		do  register_set(ClassITOpndOper, Atoms, RegSet),
		    (   Op=0 -> KL2 = KL3
		    ;   RegSet = []
		    ;   KL2 = [RegSet-Op|KL3]
		    )
		},
		{keysort(KL1, KL4)},
		{keyclumped(KL4, KL5)},
		(   foreach(RegSet1-OpList,KL5),
		    param(P,Q,T,O,Atoms)
		do  {fdset_to_range(RegSet1, RegRange)},
		    {list_to_fdset(OpList, OpSet)},
		    {fdset_to_range(OpSet, OpRange)},
		    [t(Q)#=T #/\ i(O) in OpRange #/\ r(T) in RegRange #=> t(P)#=T]
		)
	    )
	).

% CO-USE-ALLDIFF
noverlap(Rs) -->		% list of r(t(_))
	{   foreach(r(t(P)),Rs),
	    foreach(item(r(t(P)),W),Items)
	do  (   cur_operand(P, _, _, TP),
		member(T, TP),
		T >= 0
	    ->  cur_temp(T, W, _, _)
	    )
	},
	(   {Items = []} -> []
	;   {Items = [_]} -> []
	;   {Items = [item(X,Z),item(Y,Z)]} -> [X #\= Y]
	;   [disjoint1(Items)]
	).

memory_domination_constraints(Atoms) -->
	{   foreach([A|_],Atoms),
	    fromto(0,A1,A2,MemStart)
	do  A2 is max(A1,A)
	},
	{copy_related_keylist(KL)},
	(   foreach(_-Set,KL),
	    param(MemStart)
	do  (   {Set = [_]} -> []
	    ;   {   foreach(T,Set),
		    foreach(m(T),MTs),
		    fromto(Conj1,[r(T) #>= MemStart #<=> m(T)|Conj2],Conj2,[sum(MTs) #=< 1]),
		    param(MemStart)
		do  true
		},
		[let(MTs,Conj1)]
	    )
	).

copy_related_keylist(KL6) :-
	findall(TSet, cur_operand(_,_,_,TSet), Temps0),
	sort(Temps0, Temps),
	(   foreach(Set,Temps),
	    fromto(KL1,KL2,KL3,[])
	do  (   Set = [Tc|_],
		Tc \== -1
	    ->  KL2 = [Tc-Set|KL3]
	    ;   KL2 = KL3
	    )
	),
	keysort(KL1, KL4),
	keyclumped(KL4, KL5),
	(   foreach(Key-Sets,KL5),
	    foreach(Key-Union,KL6)
	do  ord_union(Sets, Union)
	).

dur_con(Dur, Con, MAXO) -->
	{Con = [Con0|_]},
	{length(Con0, NRes)},
	{   for(O,0,MAXO),
	    foreach([i(O)|DursCons1],VarTuples),
	    param(NRes)
	do  (   for(R,0,NRes-1),
		fromto(DursCons1,[dur(R,i(O)),con(R,i(O))|DursCons2],DursCons2,[]),
		param(O)
	    do  true
	    )
	},
	{   foreach(Dur1,Dur),
	    foreach(Con1,Con),
	    foreach([I|DursCons3],RelTuples),
	    count(I,0,_)
	do  (   foreach(Durr,Dur1),
		foreach(Conr,Con1),
		fromto(DursCons3,[Durr,Conr|DursCons4],DursCons4,[])
	    do  true
	    )
	},
	[table(VarTuples,RelTuples)].

% capture all precedences and pass to cumulative
% precedences come from presolver
precedences_and_resources(MAXO, Precedences,
			  _, _, _, Ops, ResSet, DurT, Cap, CSRange, Atoms) -->
	{extra_data_precedences(MAXO, QVars1, Goals1, Goals2)},
	{cond_precedences(Precedences, CSRange, Atoms, QVars2, Goals2, Goals3)},
	{append(QVars1, QVars2, QVars3)},
	{sort(QVars3, QVars)},
	{resources(QVars, Ops, ResSet, DurT, Cap, Goals3, [])},
	[let(QVars,Goals1)].

cond_precedences(Precedences, CSRange, Atoms, QVars) -->
	(   foreach([I,J,K,Json],Precedences),
	    foreach(Dis,QVars),
	    param(CSRange,Atoms)
	do  {translate_condition(Json, Cond, CSRange, Atoms)},
	    (   {I<J}		% avoid having both dis(A,B) and dis(B,A)
	    ->  {Dis = dis(I,J)},
		[Cond #=> dis(I,J) #>= K]
	    ;   {Dis = dis(J,I)},
		{NK is -K},
		[Cond #=> dis(J,I) #=< NK]
	    )
	).

% merge in the influence of ld(T)
extra_data_precedences(MAXO, QVars1) -->
	(   for(O,0,MAXO),
	    fromto(QVars1,QVars2,QVars7,[])
	do  {cur_operation(O, TypeName, _, Opnds, _)},
	    (   foreach(P,Opnds),
		fromto(QVars2,QVars3,QVars6,QVars7),
		param(O,TypeName)
	    do  {cur_operand(P, _, 1, TempsP)},
		(   foreach(T,TempsP),
		    fromto(QVars3,QVars4,QVars5,QVars6),
		    param(P,O,TempsP,TypeName)
		do  {cur_temp(T, _, _, D)},
		    (   {T<0}
		    ->  {QVars4 = QVars5}
		    ;   (   {TempsP=[T]} -> % single temp
			    [dis(D,O) #=< ld(T)]
			;	% non-primary temp
			    [t(P) #= T #=> dis(D,O) #=< ld(T)]
			),
			{QVars4 = [dis(D,O)|QVars5]}
		    )
		)
	    )
	).

resources(QVars, Ops, ResSet, DurT, Cap) -->
	(   foreach(OpsBB,Ops),
	    param(QVars,ResSet,DurT,Cap)
	do  {   foreach(DVar,QVars),
		fromto(Prec1,Prec2,Prec3,[]),
		param(OpsBB)
	    do  (   DVar = dis(J1,J2),
		    ord_member(J1, OpsBB),
		    ord_member(J2, OpsBB)
		->  Prec2 = [dis(J1,J2)|Prec3]
		;   Prec2 = Prec3
		)
	    },
	    (   foreach(Res1,ResSet),
		foreach(Cap1,Cap),
		foreach(DurCol,DurT),
		param(OpsBB,Prec1)
	    do  (   foreach(O,OpsBB),
		    foreach(task(c(O),dur(Res1,i(O)),con(Res1,i(O))),Tasks),
		    param(Res1)
		do  []
		),
				% constraint: processor resources
		{max_member(DurMax, DurCol)},
		(   {DurMax=:=0} -> []
		;   [cumulative(Tasks,Cap1,Prec1)]
		)
	    )
	).

% constraint simplification

rewrite_expr(X, X, _) :-
	simple(X), !.
rewrite_expr(bool2int(X), Y, _) :- !,
	(   X==true -> Y = 1
	;   X==false -> Y = 0
	;   Y = X
	).
rewrite_expr(if(If1,Then1,Else1), Expr, Frame) :- !,
	rewrite(If1, If2, Frame),
	rewrite_expr(Then1, Then2, Frame),
	rewrite_expr(Else1, Else2, Frame),
	(   If2==true ->  Expr=Then2
	;   If2==false ->  Expr=Else2
	;   Then2==1, Else2==0 -> Expr=If2
	;   Expr=if(If2,Then2,Else2)
	).
rewrite_expr(sum(Xs), sum(Ys), Frame) :- !,
	(   foreach(X,Xs),
	    foreach(Y,Ys),
	    param(Frame)
	do  rewrite(X, Y, Frame)
	).
rewrite_expr(1*Y, Expr, Frame) :- !,
	rewrite_expr(Y, Expr, Frame).
rewrite_expr(X*Y, X1*Y1, Frame) :- !,
	rewrite_expr(X, X1, Frame),
	rewrite_expr(Y, Y1, Frame).
rewrite_expr(X+Y, X1-NY, Frame) :-
	rewrite_expr(X, X1, Frame),
	rewrite_expr(Y, Y1, Frame),
	integer(Y1), Y < 0, !,
	NY is -Y1.
rewrite_expr(X+Y, Expr, Frame) :- !,
	rewrite_expr(X, X1, Frame),
	rewrite_expr(Y, Y1, Frame),
	(   X1==0 -> Expr = Y1
	;   Y1==0 -> Expr = X1
	;   true -> Expr = X1+Y1
	).
rewrite_expr(X-Y, X1-Y1, Frame) :- !,
	rewrite_expr(X, X1, Frame),
	rewrite_expr(Y, Y1, Frame).
rewrite_expr(X#=Y, _, Frame) :- !,
	rewrite_expr(X, X1, Frame),
	rewrite_expr(Y, Y1, Frame),
	(   integer(X1),
	    integer(Y1),
	    X=:=Y
	->  Expr = true
	;   integer(X1),
	    integer(Y1)
	->  Expr = false
	;   Expr = (X1 #= Y1)
	).
rewrite_expr(a(O), Expr, Frame) :- !,
	Frame = frame(Mandatory,_),
	(memberchk(O, Mandatory) -> Expr = true ; Expr = a(O)).
rewrite_expr(c(0), 0, _) :- !.
rewrite_expr(i(O), Expr, Frame) :- !,
	Frame = frame(_,Instructions),
	nth0(O, Instructions, Dom),
	(Dom = [Expr] -> true ; Expr = i(O)).
rewrite_expr(t(O), Expr, _) :- !, % can optimize % this???
	cur_operand(O, _, U, Dom0),
	(U=:=1 -> Dom0 = Dom ; delete_null(Dom0, Dom)),
	(Dom = [Expr] -> true ; Expr = t(O)).
rewrite_expr(r(I), r(J), Frame) :- !,
	rewrite_expr(I, J, Frame).
rewrite_expr(ls(I), ls(J), Frame) :- !,
	rewrite_expr(I, J, Frame).
rewrite_expr(ld(I), ld(J), Frame) :- !,
	rewrite_expr(I, J, Frame).
rewrite_expr(le(I), le(J), Frame) :- !,
	rewrite_expr(I, J, Frame).
rewrite_expr(Expr, Expr, _).

% rewrite(X, _, _) :-
% 	print_message(informational, rewrite(X)),
% 	fail.
rewrite(true, true, _).
rewrite(false, false, _).
rewrite(m(O), m(O), _).
rewrite(a(O), Expr, Frame) :-
	Frame = frame(Mandatory,_),
	(memberchk(O, Mandatory) -> Expr = true ; Expr = a(O)).
rewrite(P #<=> R, Expr, Frame) :-
	rewrite(P, Q, Frame),
	rewrite(R, S, Frame),
	(   Q==true -> Expr = S
	;   S==true -> Expr = Q
	;   Q==false -> Expr = (#\ S)
	;   S==false -> Expr = (#\ Q)
	;   Expr = (Q #<=> S)
	).
rewrite(P #=> R, Expr, Frame) :-
	rewrite(P, Q, Frame),
	rewrite(R, S, Frame),
	(   Q==true -> Expr = S
	;   Expr = (Q #=> S)
	).
rewrite(#\ (S#=T), Expr, Frame) :- !,
	rewrite(S#\=T, Expr, Frame).
rewrite(#\ (P#/\Q), Expr, Frame) :- !,
	rewrite(#\ P #\/ #\ Q, Expr, Frame).
rewrite(#\ R, #\ S, Frame) :-
	rewrite(R, S, Frame).
rewrite(a(O) #= 1, a(O) #= 1, _) :- !.
rewrite(c(O) #= 0, c(O) #= 0, _) :- !.
rewrite(t(O) #= Int, t(O) #= Int, _) :- integer(Int), !.
rewrite(P #= R, Expr, Frame) :-
	rewrite_expr(P, Q, Frame),
	rewrite_expr(R, S, Frame),
	(   integer(Q),
	    integer(S),
	    Q=:=S
	->  Expr = true
	;   Expr = (Q #= S)
	).
rewrite(P #< R, Expr, Frame) :-
	rewrite_expr(P, Q, Frame),
	rewrite_expr(R, S, Frame),
	(   integer(Q),
	    integer(S),
	    Q<S
	->  Expr = true
	;   Expr = (Q #< S)
	).
rewrite(P #=< R, Expr, Frame) :-
	rewrite_expr(P, Q, Frame),
	rewrite_expr(R, S, Frame),
	(   integer(Q),
	    integer(S),
	    Q=<S
	->  Expr = true
	;   Expr = (Q #=< S)
	).
rewrite(P #>= R, Expr, Frame) :-
	rewrite_expr(P, Q, Frame),
	rewrite_expr(R, S, Frame),
	(   integer(Q),
	    integer(S),
	    Q>=S
	->  Expr = true
	;   Expr = (Q #>= S)
	).
rewrite(P #> R, Expr, Frame) :-
	rewrite_expr(P, Q, Frame),
	rewrite_expr(R, S, Frame),
	(   integer(Q),
	    integer(S),
	    Q>S
	->  Expr = true
	;   Expr = (Q #> S)
	).
rewrite(P #\= R, Expr, Frame) :-
	rewrite_expr(P, Q, Frame),
	rewrite_expr(R, S, Frame),
	(   integer(Q),
	    integer(S),
	    Q=\=S
	->  Expr = true
	;   Expr = (Q #\= S)
	).
rewrite(X in Dom, X in Dom, _).
	% rewrite_expr(X, Y, Frame).
rewrite(P #/\ R, Ctr, Frame) :-
	rewrite(P, Q, Frame),
	rewrite(R, S, Frame),
	(   Q==true -> Ctr = S
	;   S==true -> Ctr = Q
	;   true -> Ctr = (Q #/\ S)
	).
rewrite(P #\/ R, Q #\/ S, Frame) :-
	rewrite(P, Q, Frame),
	rewrite(R, S, Frame).
rewrite(lr_overlap(LS1,LE1,LS2,LE2), lr_overlap(LS3,LE3,LS4,LE4), Frame) :-
	rewrite_expr(LS1, LS3, Frame),
	rewrite_expr(LS2, LS4, Frame),
	rewrite_expr(LE1, LE3, Frame),
	rewrite_expr(LE2, LE4, Frame).
rewrite(maximum(M1,Xs), Expr, Frame) :-
	(   foreach(X,Xs),
	    foreach(Y,Ys),
	    param(Frame)
	do  rewrite_expr(X, Y, Frame)
	),
	rewrite_expr(M1, M2, Frame),
	(   Ys = [] -> Expr = (M2 #= -1)
	;   Ys = [Y1] -> Expr = (M2 #= Y1)
	;   Expr = maximum(M2,Ys)
	).
rewrite(minimum(M1,Xs), Expr, Frame) :-
	(   foreach(X,Xs),
	    foreach(Y,Ys),
	    param(Frame)
	do  rewrite_expr(X, Y, Frame)
	),
	rewrite_expr(M1, M2, Frame),
	(   Ys = [] -> Expr = (M2 #= -1)
	;   Ys = [Y1] -> Expr = (M2 #= Y1)
	;   Expr = minimum(M2,Ys)
	).
rewrite(atleast_min(M1,Xs), Expr, Frame) :-
	(   foreach(X,Xs),
	    foreach(Y,Ys),
	    param(Frame)
	do  rewrite_expr(X, Y, Frame)
	),
	rewrite_expr(M1, M2, Frame),
	(   Ys = [] -> Expr = true
	;   Ys = [Y1] -> Expr = (M2 #>= Y1)
	;   Expr = atleast_min(M2,Ys)
	).
rewrite(cumulative(Tasks1,Lim,Precedences), cumulative(Tasks2,Lim,Precedences), Frame) :-
	(   foreach(task(C1,Dur1,Use1),Tasks1),
	    foreach(task(C1,Dur2,Use2),Tasks2),
	    param(Frame)
	do  rewrite_expr(Dur1, Dur2, Frame),
	    rewrite_expr(Use1, Use2, Frame)
	).
rewrite(all_different(Items1), Goal, Frame) :-
	(   foreach(X1,Items1),
	    foreach(X2,Items2),
	    param(Frame)
	do  rewrite_expr(X1, X2, Frame)
	),
	(   Items2 = [] -> Goal = true
	;   Items2 = [_] -> Goal = true
	;   Items2 = [Y1,Y2] -> Goal = (Y1 #\= Y2)
	;   true -> Goal = all_different(Items2)
	).
rewrite(disjoint1(Items1), disjoint1(Items2), Frame) :-
	(   foreach(item(X1,W1),Items1),
	    foreach(item(X2,W2),Items2),
	    param(Frame)
	do  rewrite_expr(X1, X2, Frame),
	    rewrite_expr(W1, W2, Frame)
	).
rewrite(disjoint2(Rects1), disjoint2(Rects2), Frame) :-
	(   foreach(rect(X1,W1,LS1,LD1,LE1),Rects1),
	    foreach(rect(X2,W2,LS2,LD2,LE2),Rects2),
	    param(Frame)
	do  rewrite_expr(X1, X2, Frame),
	    rewrite_expr(W1, W2, Frame),
	    rewrite_expr(LS1, LS2, Frame),
	    rewrite_expr(LD1, LD2, Frame),
	    rewrite_expr(LE1, LE2, Frame)
	).
rewrite(all_eq_or_joker(Array,Val), all_eq_or_joker(Array,Val), _).
rewrite(element(Ix,Array,Val), element(Ix,Array,Val), _).
rewrite(dc_element(Ix,Array,Val), dc_element(Ix,Array,Val), _).
rewrite(let(Q,Cs1), let(Q,Cs2), Frame) :-
	(   foreach(C1,Cs1),
	    foreach(C2,Cs2),
	    param(Frame)
	do  rewrite(C1, C2, Frame)
	).
rewrite(temp_cardinality(Temps,ValuesCounts), temp_cardinality(Temps,ValuesCounts), _).
rewrite(table(Tuples,Relation), table(Tuples,Relation), _).
rewrite(live_cumulative(Tasks,Limit), live_cumulative(Tasks,Limit), _).
rewrite(value_precede_chain(Symm,All,Vars), value_precede_chain(Symm,All,Vars), _).

% utilities -- used by presolver too

json2avl(Filename, AVL) :-
	see(Filename),
	read_line(Line1),
	(   fromto(Line1,Line2,Line3,end_of_file),
	    fromto(Lines1,[[10],Line2|Lines2],Lines2,[".\n"])
	do  read_line(Line3)
	),
	seen,
	append(Lines1, LongLine),
	read_from_codes(LongLine, {Term}),
	term_to_keylist(Term, KL1, []),
	keysort(KL1, KL2),
	ord_list_to_avl(KL2, AVL).

avl2json(Filename, AVL) :-
	avl_to_list(AVL, KL1),
	length(KL1, Len),
	tell(Filename),
	(   foreach(Key-Val,KL1),
	    count(I,1,_),
	    param(Len)
	do  (I>1 -> true ; write('{\n')),
	    (I<Len -> Sep = ',' ; Sep = '}'),
	    format(' "~w": ', [Key]),
	    (   string_key(Key) ->
		write_string_list(Val)
	    ;   write(Val)
	    ),
	    write(Sep), nl
	),
	told.

write_string_list(Val) :-
	(   foreach(V,Val),
	    fromto('[',Sep,', ',_)
	do  format('~w"~s"', [Sep,V])
	),
	write(']').

string_key(atomname).
string_key(classname).
string_key(insname).
string_key(spacename).

term_to_keylist((T1,T2)) --> !,
	term_to_keylist(T1),
	term_to_keylist(T2).
term_to_keylist(Qname:T) --> [Name-T],
	{atom_codes(Name, Qname)}.

replace_file_extension(InputName, BeforeExt, AfterExt, OutputName) :-
	atom_concat(BaseName, BeforeExt, InputName),
	atom_concat(BaseName, AfterExt, OutputName).

andify([], true).
andify([X|L], Conj) :- andify(L, X, Conj).

andify([], X, X).
andify([Y|L], X, (X,Conj)) :- andify(L, Y, Conj).

conjify([], true).
conjify([X|L], Conj) :- conjify(L, X, Conj).

conjify([], X, X).
conjify([Y|L], X, (X#/\Conj)) :- conjify(L, Y, Conj).

disjify([], false) :- !.
disjify(L, true) :-		% 4.3.0 patch
	member(X, L),
	X == true, !.
disjify([X|L], Disj) :- disjify(L, X, Disj).

disjify([], X, X).
disjify([Y|L], X, (X#\/Disj)) :- disjify(L, Y, Disj).

assert_json(Operands, Instructions, Type, Use, Temps, Width, Definer) :-
	retractall(cur_operation(_,_,_,_,_)),
	retractall(cur_operand(_,_,_,_)),
	retractall(cur_temp(_,_,_,_)),
	assertz(cur_temp(-1,0,[],-1)), % will be called with key -1
	(   count(J,0,_),
	    foreach(OperandsI,Operands),
	    foreach(InstructionsI,Instructions),
	    foreach(TypeI,Type),
	    fromto(Use,Use1,Use3,[]),
	    fromto(Temps,Temps1,Temps3,[]),
	    fromto(KL0,KL1,KL5,[])
	do  type(TypeI, TypeName),
	    (   foreach(P,OperandsI),
		fromto(Uses1,Uses2,Uses3,[]),
		fromto(Defs1,Defs2,Defs3,[]),
		fromto(Use1,[U|Use2],Use2,Use3),
		fromto(Temps1,[Ts|Temps2],Temps2,Temps3),
		fromto(KL1,KL2,KL4,KL5),
		param(J)
	    do  assertz(cur_operand(P,J,U,Ts)),
		(   U=:=1 -> Uses2 = [P|Uses3], Defs2 = Defs3
		;    true -> Uses2 = Uses3, Defs2 = [P|Defs3]
		),
		delete_null(Ts, Ts1),
		(   foreach(T,Ts1),
		    fromto(KL2,[U-(T-P)|KL3],KL3,KL4),
		    param(P,U)
		do  true
		)
	    ),
	    sort(InstructionsI, InstructionsIs),
	    assertz(cur_operation(J,TypeName,InstructionsIs,Uses1,Defs1))
	),
	keysort(KL0, KL6),	% now all def before all use
	keyclumped(KL6, [0-DefKL,1-UseKL]),
	append(DefKL, UseKL, KL7),
	keysort(KL7, KL8),
	keyclumped(KL8, KL9),
	(   count(T1,0,_),
	    foreach(WidthT,Width),
	    foreach(DefinerT,Definer),
	    foreach(T1-Ps,KL9)
	do  assertz(cur_temp(T1,WidthT,Ps,DefinerT))
	).

/*** see modeler/InstructionScheduling.hs
     -- | type of each instruction
      -- 0:  linear
      -- 1:  branch
      -- 2:  call
      -- 3:  tail call
      -- 4:  in-delimiter
      -- 5:  out-delimiter
      -- 6:  kill
      -- 7:  define
      -- 8:  combine
      -- 9:  pack
      -- 10: low
      -- 11: high
      -- 12: function
      -- 13: copy
***/
type(0,  linear).
type(1,  branch).
type(2,  call).
type(3,  tail).
type(4,  in).
type(5,  out).
type(6,  kill).
type(7,  define).
type(8,  combine).
type(9,  pack).
type(10, low).
type(11, high).
type(12, function).
type(13, copy).

real_type(linear).
real_type(branch).
real_type(call).
real_type(tail).
real_type(function).
real_type(copy).

pack_type(low).
pack_type(high).
pack_type(combine).

delete_null([-1|Ts], Ts) :- !.
delete_null(Ts, Ts).
