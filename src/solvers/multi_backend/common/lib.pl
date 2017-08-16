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
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(clpfd)).

%%% aux procedures

% much too expensive
% probe_chunks(Chunks) :-
% 	(   foreach([Out,Asum,Is,As,Os,Ts,Cs,Rs],Chunks)
% 	do  findall([Out,Asum|As], (local_solve(Out,Asum,As,Os,Ts,Cs,Rs) -> true), [[Out1,Asum1|As1]]),
% 	    lex_chain([[Out1,Asum1|As1], [Out,Asum|As]]),
% 	    (   foreach(Y,As1),
% 		foreach(I,Is),
% 		fromto(Ones1,Ones2,Ones3,[])
% 	    do  (Y=0 -> Ones2 = Ones3 ; Ones2 = [a(I)|Ones3])
% 	    ),
% 	    print_message(warning, lex_lower_bound(Out1-Asum1-Ones1))
% 	).

% local_solve(Out, Asum, As, Os, Ts, _Cs, _Rs) :-
% 	indomain(Out),
% 	indomain(Asum),
% 	labeling([], As),
% 	labeling([], Os),
% 	labeling([], Ts), % show(a, 0, As), show(o, 0, Os), show(t, 0, Ts), break,
% 	c_labeling(Cs), -- too expensive
% 	labeling([], Rs),
% 	true.

probe_chunks(Chunks) :-
	(   foreach([Out,Asum,_Is,_As,_Os,_Ts,Cs,_Rs],Chunks)
	do  findall([Out,Asum], (local_solve(Out,Asum,Cs) -> true), [[Out1,Asum1]]),
	    lex_chain([[Out1,Asum1], [Out,Asum]]),
	    print_message(warning, lex_lower_bound([Out1,Asum1]))
	).

local_solve(Out, Asum, Cs) :-
	indomain(Out),
	indomain(Asum),
	c_labeling(Cs).

bottom_up :-
	main(Obj, As, Cs, Os, Ts, Rs, LSs, LDs, LEs, Asums),
	indomain(Obj),
	print_message(warning, format('solving obj=~d',[Obj])),
	search(Obj, As, Cs, Os, Ts, Rs, LSs, LDs, LEs, Asums),
	statistics(runtime, [_,RT1]),
	fd_statistics(backtracks, BT1),
	basename(Base),
	print_message(warning, format('solved ~w obj=~d bt=~d ms=~d',[Base,Obj,BT1,RT1])),
	write_json(Obj, As, Cs, Os, Ts, Rs, LSs, LDs, LEs), !,
	proven.
bottom_up :-
	proven.

top_down :-
	main(Obj, As, Cs, Os, Ts, Rs, LSs, LDs, LEs, Asums),
	minimize((search(Obj, As, Cs, Os, Ts, Rs, LSs, LDs, LEs, Asums),
		  statistics(runtime, [_,RT1]),
		  fd_statistics(backtracks, BT1),
		  print_message(warning, format('solved obj=~d bt=~d ms=~d',[Obj,BT1,RT1])),
		  write_json(Obj, As, Cs, Os, Ts, Rs, LSs, LDs, LEs)),
		  Obj), !,
	proven.
top_down :-
	proven.

proven :-
	statistics(runtime, [_,RT2]),
	fd_statistics(backtracks, BT2),
	print_message(warning, format('proven bt=~d ms=~d',[BT2,RT2])),
	write('========\n').

middle_out :-
	retractall(stats(_,_,_)),
	main(Obj, As, Cs, Os, Ts, Rs, LSs, LDs, LEs, Asums),
	fd_min(Obj, LB),
	fd_max(Obj, UB0),
	UB is UB0+1,
	middle_out(LB, UB, Obj, search(Obj, As, Cs, Os, Ts, Rs, LSs, LDs, LEs, Asums), void,
		   write_json(Obj, As, Cs, Os, Ts, Rs, LSs, LDs, LEs)), !,
	write('========\n').
middle_out :-
	write('========\n').

middle_out(LB, LB, LB, Goal, Goal, _) :- !,
	statistics(runtime, [_,RT2]),
	fd_statistics(backtracks, BT2),
	findall([Obj,RT,BT], stats(Obj,RT,BT), Rows),
	transpose(Rows, [Objs,RTs,BTs]),
	sumlist([RT2|RTs], RTsum),
	sumlist([BT2|BTs], BTsum),
	min_member(Objmin, Objs),
	basename(Base),
	print_message(warning, format('solved ~w obj=~d bt=~d ms=~d',[Base,Objmin,BTsum,RTsum])).
middle_out(LB, UB, Obj, Goal, _, Write) :-
	Mid is (LB+UB)//2,
	print_message(warning, format('solving obj=~d lb=~d ub=~d',[Mid,LB,UB])),
	findall([RT,BT,Obj,Goal], (
			    Obj #=< Mid,
			      call(Goal),
			      statistics(runtime, [_,RT]),
			      fd_statistics(backtracks, BT),
			      call(Write)
			  -> true
			  ), [[RT1,BT1,UB1,Best]]), !,
	print_message(warning, format('solved obj=~d bt=~d ms=~d',[UB1,BT1,RT1])),
	assertz(stats(UB1,RT1,BT1)),
	middle_out(LB, UB1, Obj, Goal, Best, Write).
middle_out(LB, UB, Obj, Goal, Best, Write) :-
	LB1 is (LB+UB)//2 + 1,
	middle_out(LB1, UB, Obj, Goal, Best, Write).

write_json(Obj, _As, Cs, Os, Ts, [_|Rs], [_|LSs], [_|LDs], [_|LEs]) :-
	format('\"objective\": ~d\n', [Obj]),
	format('\"cycles\": ~w\n', [Cs]),
	format('\"instructions\": ~w\n', [Os]),
	format('\"temporaries\": ~w\n', [Ts]),
	format('\"registers\": ~w\n', [Rs]),
	format('\"live start\": ~w\n', [LSs]),
	format('\"live duration\": ~w\n', [LDs]),
	format('\"live end\": ~w\n', [LEs]),
	write('--------\n').

runtime_entry(start) :-
	statistics(runtime, _),
	bottom_up,
	halt.

show(Name, Base, Arr) :-
	(   foreach(X,Arr),
	    count(I,Base,_),
	    param(Name)
	do  (   nonvar(X) ->
		format('~w(~d) = ~d\n', [Name,I,X])
	    ;   fd_dom(X, Dom),
		format('~w(~d) in ~w\n', [Name,I,Dom])
	    )
	).

show_precedences(Ps) :-
	(   foreach((From-To#=Var),Ps)
	do  fd_dom(Var, Dom),
	    print_message(warning, format('distance from ~d to ~d is in ~w',[From,To,Dom]))
	).

show_main(Obj, As, Cs, Os, Ts, Rs, LSs, LDs, LEs, _) :-
	show(obj, 0, [Obj]),
	show(a, 0, As),
	show(c, 0, Cs),
	show(o, 0, Os),
	show(t, 0, Ts),
	show(r, -1, Rs),
	show(ls, -1, LSs),
	show(ld, -1, LDs),
	show(le, -1, LEs).

show_search(Obj, As, Cs, Os, Ts, Rs, LSs, LDs, LEs, _) :-
	show(obj, 0, [Obj]),
	show(a, 0, As),
	show(c, 0, Cs),
	show(o, 0, Os),
	show(t, 0, Ts),
	show(r, -1, Rs),
	show(ls, -1, LSs),
	show(ld, -1, LDs),
	show(le, -1, LEs).

probe(As,Indexes) :-
	(   foreach(A,As)
	do  findall(A, member(A,[0,1]), Xs),
	    (   Xs = [_,_] -> true
	    ;   Xs = [X] -> A = X
	    )
	),
	findall(As, once(labeling([],As)), [LexLB]),
	lex_chain([LexLB,As]),
	(   foreach(Y,LexLB),
	    foreach(I,Indexes),
	    fromto(Ones1,Ones2,Ones3,[])
	do  (Y=0 -> Ones2 = Ones3 ; Ones2 = [a(I)|Ones3])
	),
	print_message(warning, lex_lower_bound(Ones1)).

display_domains(Vars) :-
	(   foreach(V,Vars)
	do  fd_dom(V, Dom),
	    write(Dom),
	    nl
	).

r_labeling(Xs) :-
	labeling([ff], Xs).

c_labeling([]).
c_labeling([X|Xs]) :- nonvar(X), !,
	c_labeling(Xs).
c_labeling([X|Xs]) :-
	clpfd:delete(min, [X|Xs], Y, _),
	fd_min(Y, Min),
	Min1 is Min+1,
	(Y=Min ; Y in Min1..sup),
	c_labeling([X|Xs]).

c_labeling_down([]).
c_labeling_down([X|Xs]) :- nonvar(X), !,
	c_labeling_down(Xs).
c_labeling_down([X|Xs]) :-
	clpfd:delete(max, [X|Xs], Y, _),
	fd_max(Y, Max),
	Max1 is Max-1,
	(   Y=Max ;   Y in inf..Max1),
	c_labeling_down([X|Xs]).

% An attempt to preventing "slack" in the schedule, by a temporary tabu list of insns that can't be placed next
% No better, no worse either, just slower
% c_labeling([]).
% c_labeling([X|Xs]) :- nonvar(X), !,
% 	c_labeling(Xs).
% c_labeling([X|Xs]) :-
% 	clpfd:delete(min, [X|Xs], Y, _),
% 	fd_min(Y, Min),
% 	Min1 is Min+1,
% 	(   Y=Min,
% 	    c_labeling([X|Xs])
% 	;   Y in Min1..sup,
% 	    c_labeling([X|Xs], [Y])
% 	).

% c_labeling([], _).
% c_labeling([X|Xs], Tabu) :- nonvar(X), !,
% 	c_labeling(Xs, Tabu).
% c_labeling([X|Xs], Tabu) :-
% 	list_is_free_of_nonvar(Tabu), !,
% 	clpfd:delete(min, [X|Xs], Y, _),
% 	fd_min(Y, Min),
% 	Min1 is Min+1,
% 	(   list_is_free_of(Tabu, Y),
% 	    Y = Min,
% 	    c_labeling([X|Xs])
% 	;   Y in Min1..sup,
% 	    c_labeling([X|Xs], [Y|Tabu])
% 	).
% c_labeling([X|Xs], _) :-
% 	c_labeling([X|Xs]).

% list_is_free_of_nonvar(L) :-
% 	(   foreach(X,L)
% 	do  var(X)
% 	).

% list_is_free_of(L, Y) :-
% 	(   foreach(X,L),
% 	    param(Y)
% 	do  X \== Y
% 	).

%%% aux constraints

all_eq_or_joker(_, V) :-
	integer(V), !.
all_eq_or_joker(Xs, V) :-
	element(_, Xs, V).	% was better than dc_element
% all_eq_or_joker(Xs, V) :-	% was worse than element
% 	N in 1..2,
% 	nvalue(N, [-1,V|Xs]).

live_end_or_null(To, I, Cu, LEi) :-
	To #= I #<=> B,
	if_then_else(B, Cu, -1, LEi).

if_then_else(B, Then, ElseVal, Val) +:
	Val in ((1..max(B)) ? dom(Then)) \/
	       ((min(B)..0) ? {ElseVal}),
	Then in dom(Val) \/ ((min(B)..0) ? (inf..sup)),
	B  in ((dom(Val) /\ dom(Then)) ? {1}) \/
	      ((dom(Val) /\ {ElseVal}) ? {0}).

% get nth ls(T)
element_ls(I, LS, Val) :-
	I2 #= I+2,
	element(I2, LS, Val).

% get nth ld(T)
element_ld(I, LD, Val) :-
	I2 #= I+2,
	element(I2, LD, Val).

% get nth le(T)
element_le(I, LE, Val) :-
	I2 #= I+2,
	element(I2, LE, Val).

% get nth r(T)
element_r(I, R, Val) :-
	I2 #= I+2,
	element(I2, R, Val).

dc_element(I, [X1,X2], Y) :- !,
	ixelement(I, X1, X2, Y).
dc_element(I, [X1,X2,X3], Y) :- !,
	ixelement(I, X1, X2, X3, Y).
dc_element(I, [X1,X2,X3,X4], Y) :- !,
	ixelement(I, X1, X2, X3, X4, Y).
dc_element(I, [X1,X2,X3,X4,X5], Y) :- !,
	ixelement(I, X1, X2, X3, X4, X5, Y).
dc_element(I, [X1,X2,X3,X4,X5,X6], Y) :- !,
	ixelement(I, X1, X2, X3, X4, X5, X6, Y).
dc_element(I, [X1,X2,X3,X4,X5,X6,X7], Y) :- !,
	ixelement(I, X1, X2, X3, X4, X5, X6, X7, Y).
dc_element(I, [X1,X2,X3,X4,X5,X6,X7,X8], Y) :- !,
	ixelement(I, X1, X2, X3, X4, X5, X6, X7, X8, Y).
dc_element(I, [X1,X2,X3,X4,X5,X6,X7,X8,X9], Y) :- !,
	ixelement(I, X1, X2, X3, X4, X5, X6, X7, X8, X9, Y).
dc_element(I, [X1,X2,X3,X4,X5,X6,X7,X8,X9,X10], Y) :- !,
	ixelement(I, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, Y).
dc_element(I, L, Y) :-
	element(I, L, Y).

ixelement(I, X1, X2, Y) +:
	(I#=1 #/\ X1#=Y) #\/
	(I#=2 #/\ X2#=Y).
ixelement(I, X1, X2, X3, Y) +:
	(I#=1 #/\ X1#=Y) #\/
	(I#=2 #/\ X2#=Y) #\/
	(I#=3 #/\ X3#=Y).
ixelement(I, X1, X2, X3, X4, Y) +:
	(I#=1 #/\ X1#=Y) #\/
	(I#=2 #/\ X2#=Y) #\/
	(I#=3 #/\ X3#=Y) #\/
	(I#=4 #/\ X4#=Y).
ixelement(I, X1, X2, X3, X4, X5, Y) +:
	(I#=1 #/\ X1#=Y) #\/
	(I#=2 #/\ X2#=Y) #\/
	(I#=3 #/\ X3#=Y) #\/
	(I#=4 #/\ X4#=Y) #\/
	(I#=5 #/\ X5#=Y).
ixelement(I, X1, X2, X3, X4, X5, X6, Y) +:
	(I#=1 #/\ X1#=Y) #\/
	(I#=2 #/\ X2#=Y) #\/
	(I#=3 #/\ X3#=Y) #\/
	(I#=4 #/\ X4#=Y) #\/
	(I#=5 #/\ X5#=Y) #\/
	(I#=6 #/\ X6#=Y).
ixelement(I, X1, X2, X3, X4, X5, X6, X7, Y) +:
	(I#=1 #/\ X1#=Y) #\/
	(I#=2 #/\ X2#=Y) #\/
	(I#=3 #/\ X3#=Y) #\/
	(I#=4 #/\ X4#=Y) #\/
	(I#=5 #/\ X5#=Y) #\/
	(I#=6 #/\ X6#=Y) #\/
	(I#=7 #/\ X7#=Y).
ixelement(I, X1, X2, X3, X4, X5, X6, X7, X8, Y) +:
	(I#=1 #/\ X1#=Y) #\/
	(I#=2 #/\ X2#=Y) #\/
	(I#=3 #/\ X3#=Y) #\/
	(I#=4 #/\ X4#=Y) #\/
	(I#=5 #/\ X5#=Y) #\/
	(I#=6 #/\ X6#=Y) #\/
	(I#=7 #/\ X7#=Y) #\/
	(I#=8 #/\ X8#=Y).
ixelement(I, X1, X2, X3, X4, X5, X6, X7, X8, X9, Y) +:
	(I#=1 #/\ X1#=Y) #\/
	(I#=2 #/\ X2#=Y) #\/
	(I#=3 #/\ X3#=Y) #\/
	(I#=4 #/\ X4#=Y) #\/
	(I#=5 #/\ X5#=Y) #\/
	(I#=6 #/\ X6#=Y) #\/
	(I#=7 #/\ X7#=Y) #\/
	(I#=8 #/\ X8#=Y) #\/
	(I#=9 #/\ X9#=Y).
ixelement(I, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, Y) +:
	(I#=1 #/\ X1#=Y) #\/
	(I#=2 #/\ X2#=Y) #\/
	(I#=3 #/\ X3#=Y) #\/
	(I#=4 #/\ X4#=Y) #\/
	(I#=5 #/\ X5#=Y) #\/
	(I#=6 #/\ X6#=Y) #\/
	(I#=7 #/\ X7#=Y) #\/
	(I#=8 #/\ X8#=Y) #\/
	(I#=9 #/\ X9#=Y) #\/
	(I#=10 #/\ X10#=Y).

dc_disjoint(Rs, Ws) :-
	sort(Ws, [1]), !,
	all_distinct(Rs).
dc_disjoint([R1|Rs], [W1|Ws]) :-
	sort(Ws, [1]),
	integer(R1), !,
	R2 is R1+W1-1,
	range_to_fdset(R1..R2, Set),
	fdset_complement(Set, Compl),
	(   foreach(R,Rs),
	    param(Compl)
	do  R in_set Compl
	),
	all_distinct(Rs).
dc_disjoint(Rs, Ws) :-
	ground(Ws), !,
	(   foreach(R,Rs),
	    foreach(W,Ws),
	    fromto(Xs1,[R|Xs2],Xs4,[]),
	    foreach(item(R,W),Items)
	do  (   for(I,1,W-1),
		fromto(Xs2,[Y|Xs3],Xs3,Xs4),
		param(R)
	    do  Y #= R+I
	    )
	),
	disjoint1(Items),
	all_distinct(Xs1).
dc_disjoint(Rs, Ws) :-
	(   foreach(R,Rs),
	    foreach(W,Ws),
	    foreach(item(R,W),Items)
	do  true
	),
	disjoint1(Items).

%% This version posts live_noverlap/10 for all pairs of temps

% live_noverlap(R1, W1, LS1, LD1, LE1, R2, W2, LS2, LD2, LE2) +:
% 	LD1 #= 0 #\/
% 	LD2 #= 0 #\/
% 	R1+W1 #=< R2 #\/
% 	R2+W2 #=< R1 #\/
% 	LE1 #=< LS2 #\/
% 	LE2 #=< LS1.

% dc_disjoint(Rs, Ws, LSs, LDs, LEs) :-
% 	(   fromto(Rs,[R1|Rs1],Rs1,[_]),
% 	    fromto(Ws,[W1|Ws1],Ws1,[_]),
% 	    fromto(LSs,[LS1|LSs1],LSs1,[_]),
% 	    fromto(LDs,[LD1|LDs1],LDs1,[_]),
% 	    fromto(LEs,[LE1|LEs1],LEs1,[_])
% 	do  (   foreach(R2,Rs1),
% 		foreach(W2,Ws1),
% 		foreach(LS2,LSs1),
% 		foreach(LD2,LDs1),
% 		foreach(LE2,LEs1),
% 		param(R1,W1,LS1,LD1,LE1)
% 	    do  live_noverlap(R1, W1, LS1, LD1, LE1, R2, W2, LS2, LD2, LE2)
% 	    )
% 	).

%% NoOverlapPerCycle version
% dc_disjoint(Rs, Ws, LSs, _LDs, LEs) :-
% 	(   foreach(R,Rs),
% 	    foreach(W,Ws),
% 	    foreach(LS,LSs),
% 	    foreach(LE,LEs),
% 	    foreach(temp(R,W,LS,LE),Items)
% 	do  true
% 	),
% 	unison(Items).
%% Main version: uses disjoint2/1
dc_disjoint(Rs, Ws, LSs, LDs, LEs) :-
	(   foreach(R,Rs),
	    foreach(W,Ws),
	    foreach(LS,LSs),
	    foreach(LD,LDs),
	    foreach(LE,LEs),
	    foreach(item(R,W,LS,LD),Items),
	    fromto(0,Max1,Max2,_Max)
	do  fd_max(LE, LEmax),
	    Max2 is max(Max1,LEmax)
	),
	disjoint2(Items).
/******
	max_member(MaxW, Ws),
	(   for(C,1,Max),	% would be redundant for cycle 0
	    param(Rs,Ws,LSs,LEs,MaxW)
	do  (   foreach(R1,Rs),
		foreach(S1,Ss),
		foreach(LS1,LSs),
		foreach(LE1,LEs),
		count(I,1,_),
		param(C,MaxW)
	    do  NI is (-I-1)*MaxW,
		in_range(C, LS1, LE1, B),
		if_then_else(B, R1, NI, S1)
	    ),
	    dc_disjoint(Ss, Ws)
	).

in_range(C, LS, LE, B) +:
	LS #=< C #/\ C+1 #=< LE #<=> B.
******/

%% This version falls back on disjoint2/2

% dc_disjoint(Rs, Ws, LSs, LEs) :-
% 	(   foreach(R,Rs),
% 	    foreach(W,Ws),
% 	    foreach(LS,LSs),
% 	    foreach(LE,LEs),
% 	    foreach(item(R,W,LS,LD),Items)
% 	do  LD #= LE-LS
% 	),
% 	disjoint2(Items).

%% This version falls back on disjoint2/2 + redundant cumulative/2

% cumulative_regs(mips, 0, 14).

% dc_disjoint(Rs, Ws, LSs, LEs) :-
% 	cumulative_regs(mips, Min, Max),
% 	Lim is Max-Min+1,
% 	(   foreach(R,Rs),
% 	    foreach(W,Ws),
% 	    foreach(LS,LSs),
% 	    foreach(LE,LEs),
% 	    foreach(item(R,W,LS,LD),Items),
% 	    foreach(task(LS,LD,LE,H,1),Tasks),
% 	    param(Min,Max)
% 	do  LD #= LE-LS,
% 	    R in Min..Max #<=> B,
% 	    if_then_else(B, W, 0, H)
% 	),
% 	disjoint2(Items),
% 				% 20130423: [5] extra cumulative, needs param
% 	% cumulative(Tasks, [limit(Lim),global(true)]),
% 	true.

temp_cardinality(TempVars, ValueAList) :-
	(   foreach(Value-A,ValueAList),
	    foreach(Value-C,ValueCList)
	do  temp_count(A, C)
	),
	global_cardinality(TempVars, [-1-_|ValueCList]).

temp_count(Ai, Count) +:
	Count in ((1..max(Ai)) ? (1..sup)) \/
	         ((min(Ai)..0) ? (0..0)),
	Ai    in ((1..max(Count)) ? (1..1)) \/
	         ((min(Count)..0) ? (0..0)).

value_precede_chain(SymmValues, AllValues, Vars) :-
	(   fromto(SymmValues,ST1,ST2,[]),
	    foreach(sink(State),Sinks),
	    fromto(Arcs,Arcs1,Arcs5,[]),
	    count(State,1,_),
	    param(AllValues)
	do  ST1 = [Y|ST2],
	    (   ST2==[] -> Nono = [], Arcs1 = Arcs2
	    ;   Nono = ST1,
		Arcs1 = [arc(State,Y,State1)|Arcs2],
		State1 is State+1
	    ),
	    (   foreach(V,AllValues),
		fromto(Arcs2,Arcs3,Arcs4,Arcs5),
		param(Nono,State)
	    do  (   member(V, Nono) -> Arcs3 = Arcs4
		;   Arcs3 = [arc(State,V,State)|Arcs4]
		)
	    )
	),
	automaton(Vars, [source(1)|Sinks], Arcs).

lb_record(Is, Cs) :-
	retractall(lb(_,_)),
	(   foreach(I,Is),
	    foreach(C,Cs)
	do  fd_min(C, Cmin),
	    assertz(lb(I, Cmin))
	).

lb_report(Is, Cs) :-
	(   foreach(I,Is),
	    foreach(C,Cs)
	do  lb(I, LB),
	    print_message(warning, c_expected_got(I,LB,C))
	).
