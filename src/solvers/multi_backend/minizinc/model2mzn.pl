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
:- ensure_loaded('../common/common').

json2mzn(InputName) :-
	statistics(runtime, [T1,_]),
	replace_file_extension(InputName, '.ext.json', '.mzn', OutputName),
	json2avl(InputName, AVL1),
	model(AVL1, Vars, Objective, BBs, Constraints),
	avl_fetch(width, AVL1, Width),
	tell(OutputName),
	call_cleanup(model2mzn(Vars, Objective, BBs, Constraints, Width), told),
	statistics(runtime, [T2,_]),
	print_message(warning, format('generated .mzn in ~d msec',[T2-T1])).


model2mzn(Decls, scalar_product(Coeffs,Terms), BBs, Constraints, Width) :-
	write('include "globals.mzn";\n'),
	write('var int: obj;\n'),
	memberchk(var(r(_) in RDom), Decls),
	retractall(cur_rdom(_)),
	assertz(cur_rdom(RDom)),
	(   foreach(Decl,Decls)
	do  (   Decl = var(Term in Range),
		Term =.. [Name,Dim1] ->
		(Range == (0..1) -> Type = bool ; Type = Range),
		format('array[~w] of var ~w: ~w;\n', [Dim1,Type,Name])
	    ;   Decl = var(Term in Range),
		Term =.. [Name,Dim1,Dim2] ->
		(Range == (0..1) -> Type = bool ; Type = Range),
		format('array[~w,~w] of var ~w: ~w;\n', [Dim1,Dim2,Type,Name])
	    ;   Decl = array(Term = Init),
		Term =.. [Name,Dim1] ->
		format('array[~w] of int: ~w = array1d(~w, ~w);\n', [Dim1,Name,Dim1,Init])
	    ;   Decl = array(Term = Init),
		Term =.. [Name,Dim1,Dim2] ->
		format('array[~w,~w] of int: ~w = array2d(~w, ~w,\n', [Dim1,Dim2,Name,Dim1,Dim2]),
		(   foreach(Row,Init),
		    fromto('[',Pre1,' ',_)
		do  write('    '),
		    write(Pre1),
		    (   foreach(Elt,Row),
			fromto('| ',Pre2,', ',_)
		    do  write(Pre2),
			write(Elt)
		    ),
		    nl
		),
		write('     |]\n);\n')
	    )
	),
	(   foreach(Ctr,Constraints)
	do  mzn_constraint_top(Ctr)
	),
	(   foreach(C,Coeffs),
	    foreach(Te,Terms),
	    foreach(C*Te,Obj)
	do  true
	),
	mzn_constraint_top(obj #= sum(Obj)),
	% begin bound last cycle per bb
	(domain(Width, 1, 1) -> IsMIPS=1 ; IsMIPS=0),
	(   foreach(bb(Imin..Imax,_,_,RealsWithCopies,_,M1,M2,CopyList,_),BBs),
	    param(IsMIPS)
	do  sort(CopyList, CopySet),
	    ord_subtract(RealsWithCopies, CopySet, RealsOnly),
	    length(RealsOnly, NR),
	    format('constraint\n    forall(i in ~d..~d) (c[i] <= ~d);\n', [Imin,Imax,M1]),
	    (   IsMIPS=:=1
	    ->  format('constraint\n    1+sum(i in ~w)(bool2int(a[i])) <= c[~d];\n', [RealsWithCopies,Imax]),
		format('constraint\n    1+sum(i in ~d..~d)(bool2int(a[i])) >= c[~d];\n', [Imin,Imax,Imax]),
		format('constraint\n    ~d <= c[~d];\n', [NR+M2+1,Imax])
	    ;   M2 > 0
	    ->  format('constraint\n    1+sum(i in ~w)(bool2int(a[i])) >= ~d;\n', [CopySet,M2])
	    ;   true
	    )
	),
	% end   bound last cycle per bb
	generate_search(BBs, Coeffs),
	write('output\n'),
	write('    ["{\\"objective\\": "] ++ [show(obj)] ++ [",\\n"] ++\n'),
	write('    [" \\"cycles\\": "] ++ [show(c)] ++ [",\\n"] ++\n'),
	write('    [" \\"instructions\\": "] ++ [show(o)] ++ [",\\n"] ++\n'),
	write('    [" \\"temporaries\\": "] ++ [show(t)] ++ [",\\n"] ++\n'),
	write('    [" \\"registers\\": "] ++ [show([r[i] | i in 0..max(index_set(r))])] ++ ["}\\n"];\n').

/***
% var order: A1,Calls1,C1,O1,...,An,Callsn,Cn,On, R1,...,Rn
% in static order
generate_search(BBs, _Coeffs) :-
	(   foreach(bb(Irange,Orange,Trange,Reals,Calls,_),BBs),
	    fromto(Searches,S1,S2,S3),
	    fromto(S3,S4,S5,[])
	do  S1 = [for(a,Irange),
		  forall(c,Calls),
		  for(c,Irange),
		  for(o,Irange),
		  for(t,Orange)|S2],
	    S4 = [for(r,Trange)|S5]
	),
	write('solve :: seq_search([\n'),
	write('     int_search([obj], input_order, indomain_min, complete)\n'),
	(   foreach(Search,Searches)
	do  (   Search = for(a,First..Last)
	    ->  format('    ,~w_search([~w[i] | i in ~d..~d], input_order, indomain_min, complete)\n', [bool,a,First,Last])
	    ;   Search = for(Arr,First..Last)
	    ->  format('    ,~w_search([~w[i] | i in ~d..~d], input_order, indomain_min, complete)\n', [int,Arr,First,Last])
	    ;   Search = forall(Arr,List),
		List \== []
	    ->  write('    ,int_search('),
		(   foreach(L,List),
		    fromto('[',Sep,',',_),
		    param(Arr)
		do  format('~w~w[~d]', [Sep,Arr,L])
		),
		write('], input_order, indomain_min, complete)\n')
	    ;   true
	    )
	),
	write('    ]) satisfy;\n').
***/

% var order: A1,...,An,Calls1,C1,...,Callsn,Cn,O1,...,On,T1,...,Tn,R1,...,Rn
% by decreasing weight
generate_search(BBs, Coeffs) :-
	(   foreach(BB1,BBs),
	    foreach(C,Coeffs),
	    foreach(NC-BB1,KL1)
	do  NC is -C
	),
	keysort(KL1, KL2),
	(   foreach(_-bb(Irange,Orange,Trange,_,_,_,_,AOrder,COrder),KL2),
	    fromto(Searches,S1,S2,S6),
	    fromto(S3,S4,S5,S12),
	    fromto(S6,S7,S8,S9),
	    fromto(S9,S10,S11,S3),
	    foreach(for(r,Trange),S12),
	    fromto(-1,_,ILast,_)
	do  S1 = [indomain(c(ILast)),forall(a,AOrder)|S2],
	    S4 = [forall(c,COrder)|S5],
	    S7 = [for(o,Irange)|S8],
	    S10 = [for(t,Orange)|S11],
	    Irange = (_..ILast)
	),
	write('solve :: seq_search([\n'),
	write('     int_search([obj], input_order, indomain_min, complete)\n'),
	(   foreach(Search,Searches)
	do  (arg(1, Search, a) -> Type = bool ; Type = int),
	    (   Search = for(Arr,First..Last)
	    ->  format('    ,~w_search([~w[i] | i in ~d..~d], input_order, indomain_min, complete)\n', [Type,Arr,First,Last])
	    ;   Search = forall(Arr,List),
		List \== []
	    ->  format('    ,~w_search(', [Type]),
		(   foreach(L,List),
		    fromto('[',Sep,',',_),
		    param(Arr)
		do  format('~w~w[~d]', [Sep,Arr,L])
		),
		write('], input_order, indomain_min, complete)\n')
	    ;   Search = indomain(c(Index))
	    ->  format('    ,~w_search([~w[~d]], input_order, indomain_min, complete)\n', [int,c,Index])
	    ;   true
	    )
	),
	write('    ]) satisfy;\n').


mzn_domain({I}) :- write(I..I).
mzn_domain(I..J) :- write(I..J).
mzn_domain(D1\/D2) :-
	mzn_domain(D1),
	write(' union '),
	mzn_domain(D2).

mzn_array(Xs) :-
	(   foreach(Y,Xs)
	do  \+ (Y = if(_,_,_))
	), !,
	(   foreach(X,Xs),
	    fromto('[',Sep,', ',_)
	do  write(Sep),
	    mzn_expr(X, int)
	),
	write(']').
mzn_array(Xs) :-
	current_output(Stream),
	line_position(Stream, Pos),
	(   foreach(X,Xs),
	    fromto('[ ',Sep,', ',_),
	    param(Pos)
	do  write(Sep),
	    mzn_expr(X, int),
	    nl,
	    tab(Pos)
	),
	write(']').

mzn_2d_array(Rows) :-
	current_output(Stream),
	line_position(Stream, Pos),
	write('['),
	(   foreach(Row,Rows),
	    param(Pos)
	do  (   foreach(X,Row),
		fromto('| ',Sep,', ', _)
	    do  write(Sep),
		mzn_expr(X, int)
	    ),
	    nl,
	    tab(Pos),
	    write(' ')
	),
	write('|]').

mzn_expr(if(Ctr,1,0), int) :- !,
	write('bool2int('),
	mzn_constraint(Ctr),
	write(')').
mzn_expr(if(Ctr,Then,Else), int) :- !,
	current_output(Stream),
	line_position(Stream, Pos),
	write('let {array[0..1] of var int: ar = array1d(0..1, ['),
	mzn_expr(Else, int),
	write(','),
	mzn_expr(Then, int),
        write('])} in (\n'),
	tab(Pos),
	write('    ar[bool2int('),
	mzn_constraint(Ctr),
	write(')]\n'),
	tab(Pos),
	write(')').
mzn_expr(X, int) :-
	simple(X), !, write(X).
mzn_expr(1*Y, int) :- !,
	mzn_expr(Y, int).
mzn_expr(X*Y, int) :- !,
	write('('),
	mzn_expr(X, int),
	write(*),
	mzn_expr(Y, int),
	write(')').
mzn_expr(X+Y, int) :-
	integer(Y), Y < 0, !,
	write('('),
	mzn_expr(X, int),
	write(-),
	NY is -Y,
	write(NY),
	write(')').
mzn_expr(X+Y, int) :- !,
	write('('),
	mzn_expr(X, int),
	write(+),
	mzn_expr(Y, int),
	write(')').
mzn_expr(X-Y, int) :- !,
	write('('),
	mzn_expr(X, int),
	write(-),
	mzn_expr(Y, int),
	write(')').
mzn_expr(X#=Y, int) :- !,
	write('bool2int('),
	mzn_expr(X, int),
	write(=),
	mzn_expr(Y, int),
	write(')').
mzn_expr(max(X,Y), int) :- !,
	write('max('),
	mzn_expr(X, int),
	write(','),
	mzn_expr(Y, int),
	write(')').
mzn_expr(min(X,Y), int) :- !,
	write('min('),
	mzn_expr(X, int),
	write(','),
	mzn_expr(Y, int),
	write(')').
mzn_expr(sum([]), int) :- !,
	write(0).
mzn_expr(sum(Xs), int) :- !,
	(   foreach(X,Xs),
	    fromto('',Sep,'+',_)
	do  write(Sep),
	    mzn_expr(X, int)
	).
mzn_expr(a(I), bool) :- !,
	format('a[~d]', [I]).
mzn_expr(a(I), int) :- !,
	format('bool2int(a[~d])', [I]).
mzn_expr(X, int) :-
	X =.. [Xn,Xi], !,
	write(Xn),
	write('['),
	mzn_expr(Xi, int),
	write(']').
mzn_expr(X #>= Y, int) :- !,
	write('bool2int('),
	mzn_constraint(X #>= Y),
	write(')').
mzn_expr(X, int) :-
	X =.. [Xn,Xi,Xj], !,
	write(Xn),
	write('['),
	mzn_expr(Xi, int),
	write(','),
	mzn_expr(Xj, int),
	write(']').

mzn_constraint_top(Ctr) :-
	print_message(informational, Ctr), fail.
mzn_constraint_top(table([[o(_)|_]|_],_)) :- !.
	% (   foreach(X,Xs),
	%     param(Ys)
	% do  mzn_constraint_top(mzn_table(X,Ys))
	% ).
mzn_constraint_top(table(Xs,Ys)) :- !,
	(   foreach(X,Xs),
	    param(Ys)
	do  mzn_constraint_top(mzn_table(X,Ys))
	).
mzn_constraint_top(Ctr) :-
	write('constraint\n    '),
	mzn_constraint(Ctr),
	write(';\n').

mzn_constraint(true) :- !,
	write(true).
mzn_constraint(a(I)) :-
	mzn_expr(a(I), bool).
mzn_constraint(#\ a(I)) :- !,
	format('not a[~d]', [I]).
mzn_constraint(a(I) #= 0) :- !,
	format('not a[~d]', [I]).
mzn_constraint(a(I) #= 1) :- !,
	format('a[~d]', [I]).
mzn_constraint(X #= Y) :-
	mzn_expr(X, bool),
	write(' = '),
	mzn_expr(Y, bool), !.
mzn_constraint(X #= Y) :-
	mzn_expr(X, int),
	write(' = '),
	mzn_expr(Y, int).
mzn_constraint(X #=< Y) :-
	mzn_expr(X, int),
	write(' <= '),
	mzn_expr(Y, int).
mzn_constraint(X #< Y) :-
	mzn_expr(X, int),
	write(' < '),
	mzn_expr(Y, int).
mzn_constraint(#\ X #< Y) :- !,
	mzn_expr(X, int),
	write(' >= '),
	mzn_expr(Y, int).
mzn_constraint(sum(Ss) #>= 1) :-
	Ss = [a(_)|_], !,
	(   foreach(S,Ss),
	    fromto('',Sep,' \\/ ',_)
	do  write(Sep),
	    mzn_expr(S, bool)
	).
mzn_constraint(X #>= Y) :-
	mzn_expr(X, int),
	write(' >= '),
	mzn_expr(Y, int).
mzn_constraint(X #> Y) :-
	mzn_expr(X, int),
	write(' > '),
	mzn_expr(Y, int).
mzn_constraint(X #\= Y) :-
	mzn_expr(X, int),
	write(' != '),
	mzn_expr(Y, int).
mzn_constraint(X in Dom) :-
	write('('),
	mzn_expr(X, int),
	write(' in '),
	mzn_domain(Dom),
	write(')').
mzn_constraint(P #<=> Q) :-
	write('('),
	mzn_constraint(P),
	write(' <-> '),
	mzn_constraint(Q),
	write(')').
mzn_constraint(P #=> Q) :-
	write('('),
	mzn_constraint(P),
	write(' -> '),
	mzn_constraint(Q),
	write(')').
mzn_constraint(P #/\ Q) :-
	write('('),
	mzn_constraint(P),
	write(' /\\ '),
	mzn_constraint(Q),
	write(')').
mzn_constraint(#\ Q) :-
	write('(not '),
	mzn_constraint(Q),
	write(')').
mzn_constraint(P #\/ Q) :-
	write('('),
	mzn_constraint(P),
	write(' \\/ '),
	mzn_constraint(Q),
	write(')').
mzn_constraint(maximum(Y,Xs)) :-
	write('maximum('),
	mzn_expr(Y, int),
	write(', '),
	mzn_array(Xs),
	write(')').
mzn_constraint(all_different(Xs)) :-
	write('all_different('),
	mzn_array(Xs),
	write(')').
mzn_constraint(cumulative(Tasks1,Lim)) :-
	(   foreach(task(Ci,Dur,Use),Tasks1),
	    foreach(Ci,Cs),
	    foreach(Dur,Ds),
	    foreach(Use,Rs)
	do  true
	),
	write('cumulative'),
	current_output(Stream),
	line_position(Stream, Pos),
	write('( '),
	mzn_array(Cs),
	nl, tab(Pos),
	write(', '),
	mzn_array(Ds),
	nl, tab(Pos),
	write(', '),
	mzn_array(Rs),
	nl, tab(Pos),
	format(', ~d)', [Lim]).
mzn_constraint(live_cumulative(Tasks1,Lim)) :-
	(   foreach(task(LS,LD,_,Use),Tasks1),
	    foreach(task(LS,LD,Use),Tasks2)
	do  true
	),
	mzn_constraint(cumulative(Tasks2,Lim)).
mzn_constraint(disjoint1(Rects)) :-
	(   foreach(item(X,W),Rects),
	    foreach(X,Xs),
	    foreach(W,Ws),
	    foreach(task(X,W,1),Tasks)
	do  true
	),
	(   sort(Ws, [1])
	->  mzn_constraint(all_different(Xs))
	;   mzn_constraint(cumulative(Tasks,1))
	).
mzn_constraint(disjoint2(Rects1)) :-
	(   foreach(rect(Pi,W,LSi,LDi,_),Rects1),
	    foreach(Pi,Xs),
	    foreach(W,DXs),
	    foreach(LSi,Ys),
	    foreach(LDi,DYs)
	do  true
	),
	write('diffn('),
	mzn_array(Xs),
	write(', '),
	mzn_array(Ys),
	write(', '),
	mzn_array(DXs),
	write(', '),
	mzn_array(DYs),
	write(')').
mzn_constraint(mzn_table(Vars,Tuples)) :-
	write('table'),
	current_output(Stream),
	line_position(Stream, Pos),
	write('( '),
	mzn_array(Vars),
	nl, tab(Pos),
	write(', '),
	mzn_2d_array(Tuples),
	nl, tab(Pos),
	write(')').
mzn_constraint(dc_element(Index,List,Value)) :-
	mzn_constraint(element(Index,List,Value)).
mzn_constraint(element(Index,List,Value)) :-
	current_output(Stream),
	line_position(Stream, Pos),
	length(List, N),
	format('let {array[1..~d] of var int: elt = array1d(1..~d, ', [N,N]),
	mzn_array(List),
	write(')} in ('),
	nl,
	tab(Pos),
	write('    '),
	mzn_constraint(Value #= elt(Index)),
	nl,
	tab(Pos),
	write(')').
mzn_constraint(temp_cardinality(TempVars, ValueAList)) :-
	current_output(Stream),
	line_position(Stream, Pos),
	length(ValueAList, N),
	format('let {array[1..~d] of var int: aux} in (\n', [N]),
	tab(Pos),
	(   foreach(Value-C,ValueAList),
	    foreach(Value,Values),
	    foreach(aux(I),Counts),
	    count(I,1,N),
	    param(Pos)
	do  write('    '),
	    mzn_constraint(aux(I) #>= C),
	    write(' /\\\n'),
	    tab(Pos)
	),
	write('    global_cardinality('),
	mzn_array(TempVars),
	write(', '),
	mzn_array(Values),
	write(', '),
	mzn_array(Counts),
	write(')\n'),
	tab(Pos),
	write(')').
mzn_constraint(value_precede_chain(Ints,Vars)) :-
	write('value_precede_chain('),
	mzn_array(Ints),
	write(','),
	mzn_array(Vars),
	write(')').
mzn_constraint(let(Locals,Constraints1)) :-
	last(Constraints1, cumulative(Tasks1,Lim,_)), !,
	(   foreach(dis(J,K),Locals),
	    foreach(dis(J,K)-(c(K)-c(J)),Map)
	do  true
	),
	ord_list_to_avl(Map, AVL),
	substitute(Constraints1, Constraints2, AVL),
	current_output(Stream),
	line_position(Stream, Pos),
	(   foreach(Ctr,Constraints2),
	    param(Pos)
	do  (   Ctr = cumulative(_,_,_) -> true
	    ;   mzn_constraint(Ctr),
		write(' /\\\n'),
		tab(Pos)
	    )
	),
	mzn_constraint(cumulative(Tasks1,Lim)).
mzn_constraint(let(Locals,Constraints1)) :-
	last(Constraints1, sum(Locals) #=< 1), !,
	(   fromto(Constraints1,[Ctr#<=>Local|Constraints2],Constraints2,[_]),
	    foreach(Local,Locals),
	    foreach(Ctr,Ctrs)
	do  true
	),
	mzn_constraint(sum(Ctrs) #=< 1).
mzn_constraint(let(Locals,Constraints1)) :- % broken
	cur_rdom(Type),
	(   foreach(Local,Locals),
	    foreach(Local-iaux(I),KL1),
	    count(I,1,N)
	do  true
	),
	keysort(KL1, KL2),
	ord_list_to_avl(KL2, AVL),
	substitute(Constraints1, Constraints2, AVL),
	current_output(Stream),
	line_position(Stream, Pos),
	format('let {array[1..~d] of var ~w: ~w} in (\n', [N,Type,iaux]),
	tab(Pos),
	(   foreach(Ctr,Constraints2),
	    param(Pos)
	do  write('    '),
	    mzn_constraint(Ctr),
	    write(' /\\\n'),
	    tab(Pos)
	),
	write('true\n'),
	tab(Pos),
	write(')').

substitute(X, X, _) :- atomic(X), !.
substitute(X, Y, AVL) :-
	avl_fetch(X, AVL, Y), !.
substitute(X, Y, AVL) :-
	functor(X, F, N),
	functor(Y, F, N),
	(   foreacharg(A,X),
	    foreacharg(B,Y),
	    param(AVL)
	do  substitute(A, B, AVL)
	).

tab(N) :-
	format('~*c', [N,32]).

