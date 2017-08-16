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
:- use_module(library(codesio)).
:- use_module(library(clpfd)).

:- ensure_loaded(common).

model(AVL, Vars, Objective, Def, Use, Rus, Clusters) -->
	{Vars = vars([c(0..NoInsn-1) in (0..MAXC),
		     p(0..NoTemp-1) in (0..MAXP),
		     s(0..NoInsn-1) in (0..1),
		     ls(0..NoTemp-1) in (0..MAXC),
		     le(0..NoTemp-1) in (0..MAXC)],BBs)},
	{avl_fetch(source, AVL, Source)},	% source of basic block
	{avl_fetch(maxc, AVL, MaxC)}, % maximum issue cycle of basic block
	{avl_fetch(f, AVL, F)},	% execution frequency of basic block
	{avl_fetch(e, AVL, E)},	% latency of instruction * instruction
	{avl_fetch(man, AVL, Man)}, % mandatoriness of instruction
	{avl_fetch(use, AVL, Use)}, % use of instruction
	{avl_fetch(def, AVL, Def)}, % def of instruction
	{avl_fetch(w, AVL, W)}, % width of temporary
	{avl_fetch(cap, AVL, Cap)}, % capacity of resource
	{avl_fetch(rus, AVL, Rus)}, % resource use & occupancy of instruction
	{avl_fetch(atoms, AVL, Atoms)}, % atoms function
	{avl_fetch(must, AVL, Must)}, % must of temporary * register atom
	{avl_fetch(same, AVL, Same)}, % same of 2^temporary
	{avl_fetch(cl, AVL, Clusters)}, % same of 2^cycle
	{avl_fetch(drs, AVL, DRs)},   % default register space of temporary
	{avl_fetch(crs, AVL, CRs)},   % copy register space of temporary
	{avl_fetch(ac, AVL, AC)},   % alternative copies of set of instruction
	{avl_fetch(mac, AVL, Mac)},   % mandatoriness of alternative copy groups
	{avl_fetch(cdisj, AVL, CDisj)},   % cond. live range disjunction
	{avl_fetch(linked, AVL, Linked)}, % groups of s[i] that must be equal
	{avl_fetch(diff, AVL, Diff)},	% groups of p[i] that must be different
	{avl_fetch(cbounds, AVL, CBounds)}, % bounds of c[i]
	{avl_fetch(cec, AVL, Cec)}, % cond earliest cycle i.e. s[i] = 1 -> c[j] >= LB
	{avl_fetch(pdom, AVL, PDom)}, % domain of p[i]
	{length(Use, NoInsn)},
	{length(DRs, NoTemp)},
	{   foreach([MP0,MP2],Atoms),
	    foreach(I0-[MP0,MP2],I2Atom1),
	    fromto(0,MP1,MP3,MAXP),
	    count(I0,0,_)
	do  MP3 is max(MP1,MP2)
	},
	{   foreach(M,MaxC),
	    fromto(0,M1,M2,MAXC)
	do  M2 is max(M1,M)
	},
	(   foreach(DT,Def),
	    foreach(UT,Use),
	    foreach(Ru,Rus),
	    foreach([LB,UB],CBounds),
	    foreach(I1-(DT-UT),I2DU1),
	    foreach(I1-Ru,I2Rus1),
	    count(I1,0,_),
	    fromto(DU1,DU2,DU6,[])
	do  [c(I1) in LB..UB],
	    {   foreach(T1,DT),
		fromto(DU2,[T1-d(I1)|DU3],DU3,DU4),
		param(I1)
	    do  true
	    },
	    {   foreach(T2,UT),
		fromto(DU4,[T2-u(I1)|DU5],DU5,DU6),
		param(I1)
	    do  true
	    }
	),
	{keysort(DU1, DU7)},
	{keyclumped(DU7, DU8)},
	{ord_list_to_avl(DU8, T2DU)},
	{ord_list_to_avl(I2DU1, I2DU)},
	{ord_list_to_avl(I2Rus1, I2Rus)},
	{ord_list_to_avl(I2Atom1, I2Atom)},
	{objective(Source, F, I2DU, MaxC, NoInsn, Objective, BBs)},
	{issue_cycle(Source, MaxC, MAXC, NoInsn)},
	(   foreach([P4,R1],Must)
	do  [p(P4) #= R1]
	),
	(   foreach([P5|P6s],Same)
	do  (   foreach(P6,P6s),
		param(P5)
	    do  [p(P5) #= p(P6)]
	    )
	),
	(   foreach([C5|C6s],Clusters)
	do  (   foreach(C6,C6s),
		param(C5)
	    do  [c(C5) #= c(C6)]
	    )
	),
	(   foreach(I5,Man)
	do  [s(I5) #= 1]
	),
	(   foreach(I4s,Mac)
	do  [sum(SI4) #>= 1],
	    {   foreach(I4,I4s),
		foreach(s(I4),SI4)
	    do  true
	    }
	),
	%% PP = 1 <=> P can't be empty, le(P) =< ls(P) must be false
	%% PQ = 1 <=> Q can't precede P, le(Q) =< ls(P) must be false
	%% QP = 1 <=> P can't precede Q, le(P) =< ls(Q) must be false
	%% QQ = 1 <=> Q can't be empty, le(Q) =< ls(Q) must be false
	(   foreach([I7s,P8,Q8,PP,PQ,QP,QQ],CDisj)
	do  [Cstr5],
	    {   foreach(I7,I7s),
		foreach(s(I7),SI7)
	    do  true
	    },
	    {   Cstr1 = (sum(SI7) #>= 1),
		live_ranges_min_max(P8, P8min, P8max),
		live_ranges_min_max(Q8, Q8min, Q8max),
		(PP=:=1 -> Cstr2 = Cstr1 ; Cstr2 = (Cstr1 #\/ P8max #=< P8min)),
		(PQ=:=1 -> Cstr3 = Cstr2 ; Cstr3 = (Cstr2 #\/ Q8max #=< P8min)),
		(QP=:=1 -> Cstr4 = Cstr3 ; Cstr4 = (Cstr3 #\/ P8max #=< Q8min)),
		(QQ=:=1 -> Cstr5 = Cstr4 ; Cstr5 = (Cstr4 #\/ Q8max #=< Q8min))
	    }
	),
	(   foreach([I2|I2s],Linked)
	do  (   foreach(I3,I2s),
		param(I2)
	    do  [s(I2) #= s(I3)]
	    )
	),
	(   foreach(Dif1,Diff)
	do  {   foreach(P1,Dif1),
		foreach(p(P1),Dif2)
	    do  true
	    },
	    (   {Dif2 = [PP1,PP2]}
	    ->  [PP1 #\= PP2]
	    ;   [all_different(Dif2)]
	    )
	),
	(   foreach([Si,Ci,LBi],Cec)
	do  [s(Si) #=> c(Ci) #>= LBi]
	),
	{retractall(current_domain(_,_))},
	(   foreach(Dom1,PDom),
	    count(P7,0,_),
	    param(MAXP)
	do  {json2range(Dom1, Dom2)},
	    {assertz(current_domain(P7,Dom2))},
	    ({Dom2==(0..MAXP)} -> [] ; [p(P7) in Dom2])
	),
	live_begin_ctr(NoTemp, T2DU),
	live_end_ctr(NoTemp, T2DU),
	precedence_ctr(E),
	(   {sort(W, [1])} ->
	    s_sink_aux(E, I2Rus, Source, NoInsn) % needs adaption for VLIW
	;   []
	),
	resource_ctr(Cap, I2Rus, Source, NoInsn),
	placement_ctr(Def, W, Source, NoInsn),
	alt_copy_ctr(AC, CRs, I2DU, I2Atom, E, Mac),
	value_precede_chains(Atoms, Must, BBs).

live_ranges_min_max([P|Ps], ls(P), Max) :-
	(   foreach(Q,Ps),
	    fromto(le(P),Max1,max(le(Q),Max1),Max)
	do  true
	).

json2range(Dom1, Dom3) :-
	(   foreach([H,T],Dom1),
	    foreach([H|T],Dom2)
	do  true
	),
	fdset_to_range(Dom2, Dom3).

value_precede_chains(Atoms, Must, BBs) -->
	{   foreach([A1,A2],Atoms),
	    fromto([],Regs1,Regs3,Regs4)
	do  range_to_fdset(A1..A2, Regs2),
	    fdset_union(Regs1, Regs2, Regs3)
	},
	{   foreach([_,A3],Must),
	    fromto(Regs4,Regs5,Regs6,Regs7)
	do  fdset_del_element(Regs5, A3, Regs6)
	},
	{fdset_to_list(Regs7, Regs)},
	{   foreach(R1,Regs),
	    foreach(Tag-R1,KL1),
	    param(Atoms)
	do  (   foreach([A4,A5],Atoms),
		foreach(B,Tag),
		param(R1)
	    do  (A4 =< R1, R1 =< A5 -> B=1 ; B=0)
	    )
	},
	{keysort(KL1, KL2)},
	{keyclumped(KL2, KL3)},
 	{   foreach(bb(_,T1..T2),BBs),
	    fromto(Hs,Hs1,Hs4,[]),
	    param(Must)
	do  (   for(T0,T1,T2),
		fromto(Hs1,Hs2,Hs3,Hs4),
		param(Must)
	    do  (member([T0,_], Must) -> Hs2 = Hs3 ; Hs2 = [T0|Hs3])
	    )
	},
	(   foreach(_-Class,KL3),
	    param(Hs)
	do  {list_to_fdset(Class, CSet)},
	    {   foreach(T,Hs),
		fromto(Ts1,Ts2,Ts3,[]),
		param(CSet)
	    do  (   current_domain(T, TDom),
		    range_to_fdset(TDom, TSet),
		    fdset_intersect(TSet, CSet) ->
		    Ts2 = [p(T)|Ts3]
		;   Ts2 = Ts3
		)
	    },
	    {length(Class, Clen)},
	    {length(Ts1, Tlen)},
	    (   {Clen =< 1} -> []
	    ;   {Tlen =< 1} -> []
	    ;   {Clen =< Tlen} -> [value_precede_chain(Class,Ts1)]
	    ;   {prefix_length(Class, Cpart, Tlen)},
		[value_precede_chain(Cpart,Ts1)]
	    )
	).

issue_cycle(Source, MaxC, MAXC, NoInsn) :- % (1)
	(   for(I,0,NoInsn-1),
	    fromto(Source,Source1,Source2,[]),
	    fromto(MaxC,MaxC1,MaxC2,[]),
	    fromto(0,M1,M2,_),
	    param(MAXC)
	do  (   Source1 = [I|Source2] ->
		MaxC1 = [M2|MaxC2]
	    ;   Source1 = Source2,
		MaxC1 = MaxC2,
		M1 = M2
	    )
	).

live_begin_ctr(NoTemp, T2DU) --> % (6)
	(   for(T,0,NoTemp-1),
	    param(T2DU)
	do  {avl_fetch(T, T2DU, [d(J)|_])},
	    [ls(T) #= c(J)]
	).

live_end_ctr(NoTemp, T2DU) --> % (7)
	(   for(T,0,NoTemp-1),
	    param(T2DU)
	do  {avl_fetch(T, T2DU, [_|Us])},
	    (   {Us = [u(J)]} -> [le(T) #= c(J)]
	    ;   {   foreach(u(K),Us),
		    foreach(c(K),Cs)
		do  true
		},
		[maximum(Cs,le(T))]
	    )
	).

placement_ctr(Def, W, [0|Source], NoInsn) -->	% (11,12,13)
	{append(Source, [NoInsn], Sink)},
	(   foreach(Srcb,[0|Source]),
	    foreach(Sinkb,Sink),
	    param(Def,W)
	do  (   foreach(Defi,Defis),
		for(I,Srcb,Sinkb-1),
		param(Def)
	    do  {nth0(I, Def, Defi)}
	    ),
	    {append(Defis, Defb)},
	    (   foreach(T,Defb),
		foreach(rect(p(T),p(T)+WT,ls(T),le(T)),Rects),
		param(W)
	    do  {nth0(T, W, WT)}
	    ),
	    (   {Rects=[]} -> []
	    ;   {Rects=[_]} -> []
	    ;   [disjoint2(Rects)]
	    )
	).

alt_copy_ctr(AC, CRs, I2DU, I2Atom, E, Mac) --> % (17,18,19,20,21)
	(   foreach([Hd|Tl],AC),
	    param(CRs,I2DU,I2Atom,E,Mac)
	do  {avl_fetch(Hd, I2DU, [Td]-[Ts])},
	    do_alt_copy_ctr(Ts, Td, Hd, Tl, CRs, I2DU, I2Atom, E, Mac)
	).

do_alt_copy_ctr(Ts, Td, Hd, Tl, CRs, I2DU, I2Atom, E, Mac) -->
	(   {member([Hd|Tl], Mac)} -> []
	;   [Sum #= 0 #=> p(Ts) #= p(Td)],
	    (   {Tl==[]} -> {Sum = s(Hd)}
	    ;   {Sum = sum([s(Hd)|Ss])},
	        % now dealt with as an instruction cluster
		% (   foreach(L,Tl),
		%     foreach(s(L),Ss),
		%     param(Hd)
		% do  [c(Hd) #= c(L)]
		% ),
		[sum([s(Hd)|Ss]) #=< 1]
	    ),
	    {   foreach([I1,I2,_],E),
		fromto([S1|Succ1],Succ2,Succ3,[]),
		param(Hd)
	    do  (I1=:=Hd -> Succ2 = [c(I2)|Succ3] ; Succ2 = Succ3)
	    },
	    {   foreach(S2,Succ1),
		fromto(S1,Min1,min(S2,Min1),Min)
	    do  true
	    },
	    [Sum #= 0 #=> c(Hd) #= Min] % extra: fix cycle of unscheduled insn
	),
	(   foreach(I,[Hd|Tl]),
	    param(Ts,Td,CRs,I2DU,I2Atom)
	do  {get_crs(I, Ts, CRs, I2DU, I2Atom, Doms)},
	    (   {current_domain(Ts, Doms)} -> []
	    ;   [s(I) #=> p(Ts) in Doms]
	    ),
	    {get_crs(I, Td, CRs, I2DU, I2Atom, Domt)},
	    (   {current_domain(Td, Domt)} -> []
	    ;   [s(I) #=> p(Td) in Domt]
	    )
	).

