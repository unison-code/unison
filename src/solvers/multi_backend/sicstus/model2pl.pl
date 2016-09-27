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
:- ensure_loaded('../common/common').

:- dynamic optimizing/1. % speed | size

json2pl(InputName, Objective) :-
	retractall(optimizing(_)),
	assertz(optimizing(Objective)),
	statistics(runtime, [T1,_]),
	replace_file_extension(InputName, '.ext.json', '', BaseName),
	atom_concat(BaseName, '.pl', OutputName),
	json2avl(InputName, AVL1),
	model(AVL1, Vars, FqTimesOutCs, BBs, CSSpill, Constraints),
	avl_fetch(callersaved, AVL1, CallerSaved),
	avl_fetch(width, AVL1, Width),
	avl_fetch(con, AVL1, Con),
	avl_fetch(instructions, AVL1, Instructions),
	avl_fetch(preassign, AVL1, Preassign),
	avl_fetch(maxf, AVL1, MAXF),
	list_to_fdset(CallerSaved, CSSet),
	fdset_to_range(CSSet, CSRange),
	empty_avl(AVL2a),
	avl_store(csrange, AVL2a, CSRange, AVL2b),
	avl_store(width, AVL2b, Width, AVL2c),
	avl_store(con, AVL2c, Con, AVL2d),
	avl_store(instructions, AVL2d, Instructions, AVL2e),
	avl_store(preassign, AVL2e, Preassign, AVL2f),
	avl_store(maxf, AVL2f, MAXF, AVL2g),
	model2pl(Vars, FqTimesOutCs, BBs, Constraints, Head, AVL2g, AVL2, Goals, []),
	Head = main(Obj,As,Cs,Os,Ts,Rs,LSs,LDs,LEs,ASums),
	SearchHead = search(Obj,As,Cs,Os,Ts,Rs,LSs,LDs,LEs,ASums),
	(   foreach(bb(_..ILast,_,_,_,_,_,_,_,_),BBs),
	    foreach(ASum,ASums),
	    fromto(AVL2,AVL3,AVL4,AVL5)
	do  avl_store(asum(ILast), AVL3, ASum, AVL4)
	),
	generate_search(BBs, AVL5, CSSpill, SearchGoals1, []),
	(   foreach(SG,SearchGoals1),
	    fromto(SearchGoals2,SearchGoals3,SearchGoals4,[]),
	    fromto(Probes1,Probes2,Probes3,[])
	do  (   SG = probe(_,_)
	    ->  SearchGoals3 = SearchGoals4, Probes2 = [SG|Probes3]
	    ;   SearchGoals3 = [SG|SearchGoals4], Probes2 = Probes3
	    )
	),
	andify(SearchGoals2, SearchBody),
	(   foreach(Goal,Goals),
	    fromto(Unify1,Unify2,Unify3,Call1),
	    fromto(Call1,Call2,Call3,Probes1)
	do  (   nonvar(Goal),
		Goal = (X #= Y),
		simple(X),
		simple(Y)
	    ->  Unify2 = [X=Y|Unify3], Call2 = Call3
	    ;   nonvar(Goal),
		Goal = (X #<=> Y),
		simple(X),
		simple(Y)
	    ->  Unify2 = [X=Y|Unify3], Call2 = Call3
	    ;   Unify2 = Unify3, Call2 = [Goal|Call3]
	    )
	),
	pretty_avl(AVL2),
	pretty_head(Head),
	andify(Unify1, Body),
	tell(OutputName),
	legacy_portray_clause((:- ensure_loaded('solvers/multi_backend/common/lib'))), nl,
	legacy_portray_clause((:- dynamic main/10, search/10)), nl,
	legacy_portray_clause((basename(BaseName))), nl,
	legacy_portray_clause((Head :- Body)), nl,
	legacy_portray_clause((:- set_prolog_flag(single_var_warnings,off))),
	legacy_portray_clause((SearchHead :- SearchBody)), nl,
	legacy_portray_clause((:- set_prolog_flag(single_var_warnings,on))),
	% legacy_portray_clause((:- initialization runtime_entry(start))),
	told,
	statistics(runtime, [T2,_]),
	print_message(warning, format('generated .pl in ~d msec',[T2-T1])).

model2pl(Decls, scalar_product(Coeffs,Terms), BBs, Constraints, Head, AVL0, AVL8) -->
	{Head = main(Obj,_As,_Cs,_Os,_Ts,Rs,LSs,LDs,LEs,ASums)},
	(   foreach(Decl,Decls),
	    fromto(AVL0,AVL1,AVL4,AVL5),
	    param(Head)
	do  (   {Decl = var(Term in L..U)},
		{Term =.. [Name,A..B]} ->
		(   for(I1,A,B),
		    foreach(V1,Vars1),
		    fromto(AVL1,AVL2,AVL3,AVL4),
		    param(Name)
		do  {Key1 =.. [Name,I1]},
		    dic_lookup(Key1, AVL2, V1, AVL3)
		),
		{head_arg(Name, Head, Vars1)},
		[domain(Vars1, L, U)]
	    ;   {Decl = array(Term = Init)} ->
		{functor(Term, Key2, _)},
		{avl_store(Key2, AVL1, Init, AVL4)}
	    )
	),
	% special for r array
	{avl_store(r, AVL5, RsVar, AVL5a)},
	[RsVar = Rs],
	% special for ls, ld, le arrays
	{avl_store(ls, AVL5a, LSsVar, AVL5b)},
	[LSsVar = LSs],
	{avl_store(ld, AVL5b, LDsVar, AVL5c)},
	[LDsVar = LDs],
	{avl_store(le, AVL5c, LEsVar, AVL5d)},
	[LEsVar = LEs],
	(   foreach(Ctr,Constraints),
	    fromto(AVL5d,AVL6,AVL7,AVL8)
	do  pl_constraint(Ctr, AVL6, AVL7)
	),
	% begin bound last cycle per bb
	% bb(Irange,OPNDrange,Trange,Reals,Calls,MaxC,OptionalMin,AOrder,COrder)
	(   foreach(bb(Imin..Imax,Omin..Omax,Tmin..Tmax,RealsWithCopies,_,M1,M2,CopyList,_),BBs),
	    foreach([IC,ASum,/*CopyList*/Is,Ais,Ois,Tis,Cis,Ris],Chunks),
	    foreach(ASum,ASums),
	    param(AVL8)
	do  {   for(I2,Imin,Imax),
		foreach(I2,Is),
		foreach(Ai,Ais),/**/
		foreach(Oi,Ois),
		foreach(Ci,Cis),
		param(AVL8)
	    do  avl_fetch(a(I2), AVL8, Ai),/**/
		avl_fetch(i(I2), AVL8, Oi),
		avl_fetch(c(I2), AVL8, Ci)
	    },
	    % {   foreach(I3,CopyList),
	    % 	foreach(Ai,Ais),
	    % 	param(AVL8)
	    % do  avl_fetch(a(I3), AVL8, Ai)
	    % },
	    {   for(J,Omin,Omax),
		foreach(Ti,Tis),
		param(AVL8)
	    do  avl_fetch(t(J), AVL8, Ti)
	    },
	    {   for(K,Tmin,Tmax),
		foreach(Ri,Ris),
		param(AVL8)
	    do  avl_fetch(r(K), AVL8, Ri)
	    },
	    {sort(CopyList, CopySet)},
	    {ord_subtract(RealsWithCopies, CopySet, RealsOnly)},
	    {   foreach(Coi,CopySet),
		foreach(CAi,CopyAs),
		param(AVL8)
	    do  avl_fetch(a(Coi), AVL8, CAi)
	    },
	    {avl_fetch(c(Imax), AVL8, IC)},
	    [domain(Cis, -1, M1)],
	    [sum(CopyAs, #=, ASum)],
	    [ASum #>= M2],
	    [sum([1|Ais], #>=, IC)], % trivial upper bound on (out) cycle
	    makespan_lb(ASum, IC, CopySet, RealsOnly, AVL8) % lower bound on (out) cycle
	),
	objective(Coeffs, Terms, Chunks, AVL8, Obj),
	({optimizing(speed)} -> [probe_chunks(Chunks)] ; []).
	% end   bound last cycle per bb

objective(Coeffs, Terms, _, AVL, Obj) -->
	{optimizing(speed)}, !,
	{   foreach(Te,Terms),
	    foreach(TV,TVs),
	    param(AVL)
	do  avl_fetch(Te, AVL, TV)
	},
	{avl_fetch(maxf, AVL, MAXF)},
	[Obj #=< MAXF],
	[scalar_product(Coeffs, TVs, #=, Obj)].
objective(_, _, Chunks, AVL, Obj) -->
	{optimizing(size)}, !,
	{   foreach([_,ASum,Os|_],Chunks),
	    foreach(ASum,ASums),
	    foreach(Min5,Coeffs),
	    foreach(Os,Oss),
	    param(Min5)
	do  true
	},
	{append(Oss, OSet)},
	{avl_fetch(instructions, AVL, Instructions)}, % set of instructions for op o
	{avl_fetch(con, AVL, [BitSizes|_])},
	{max_member(GCD1, BitSizes)},
	{   foreach(S,BitSizes),
	    fromto(GCD1,GCD2,GCD3,GCD4)
	do  (   S=:=0 -> GCD2 = GCD3
	    ;   GCD3 is gcd(GCD2,S)
	    )
	},
	{   foreach(O,OSet),
	    foreach(ISet,Instructions),
	    fromto(MandBits1,MandBits2,MandBits3,[]),
	    fromto(OptBits1,OptBits2,OptBits3,[]),
	    fromto(1024,Min1,Min4,Min5),
	    param(AVL,BitSizes)
	do  avl_fetch(con(0,i(O)), AVL, TV),
	    (   ISet = [0|Is]
	    ->  MandBits2 = MandBits3,
	        OptBits2 = [TV|OptBits3],
		(   foreach(I,Is),
		    fromto(Min1,Min2,Min3,Min4),
		    param(BitSizes)
		do  nth0(I, BitSizes, ISize),
		    Min3 is min(Min2,ISize)
		)
	    ;   MandBits2 = [TV|MandBits3],
		OptBits2 = OptBits3,
		Min1 = Min4
	    )
	},
	[sum(MandBits1, #=, MandSum)],
	[sum(OptBits1, #=, OptSum)],
	[MandSum + OptSum #= Obj],
	{avl_fetch(maxf, AVL, MAXF)},
	[Obj #< MAXF],
	[scalar_product([1|Coeffs], [MandSum|ASums], #=<, Obj)],
	[Obj mod GCD4 #= 0].

% lower bound on (out) cycle based on min. number of bits per instruction
makespan_lb(ASum, IC, CopySet, RealsOnly, AVL) -->
	[ASize*ASum + MandBits3 #=< BundleSize*IC],
	{avl_fetch(instructions, AVL, Instructions)},
	{avl_fetch(con, AVL, [Bits|_])},
	{max_member(BundleSize, Bits)},
	{   CopySet = []
	->  ASize = 0
	;   (   foreach(I,CopySet),
		foreach(Ops,Opss),
		param(Instructions)
	    do  nth0(I, Instructions, Ops)
	    ),
	    append(Opss, OpBag),
	    sort(OpBag, [0|OpSet]),
	    (   foreach(O,OpSet),
		foreach(Size1,Sizes),
		param(Bits)
	    do  nth0(O, Bits, Size1)
	    ),
	    min_member(ASize, Sizes)
	},
	{   foreach(J,RealsOnly),
	    fromto(BundleSize,MandBits1,MandBits2,MandBits3),
	    param(Bits,Instructions)
	do  nth0(J, Instructions, [Op|_]), % cheating, should take smallest bitsize
	    nth0(Op, Bits, Size2),
	    MandBits2 is MandBits1 + Size2
	}.

% var order: T per bb, I per bb, [Cout, C] per bb, R per bb
% bbs by increasing length
generate_search(BBs, AVL, _CSSpill) -->
	% {   foreach(A1,CSSpill),
	%     foreach(X1,Spill),
	%     param(AVL)
	% do  avl_fetch(a(A1), AVL, X1)
	% },
	{   foreach(BB1,BBs),
	    foreach(Rank-BB1,KL1),
	    fromto(0,_,Tn,MAXT)
	do  arg(1, BB1, I1..In),
	    arg(3, BB1, _..Tn),
	    Rank is I1-In
	},
	{keysort(KL1, KL2)},
	{avl_fetch(preassign, AVL, Preassign)},
	{transpose(Preassign, [PreassignedOs,_])},
	{searches(KL2, PreassignedOs, MAXT, Searches)},
	% [labeling([down], Spill)],
	(   foreach(Search,Searches),
	    param(AVL)
	do  {   Search = for(K,Min..Max)
	    ->  (   for(J,Min,Max),
		    foreach(V2,Vars4),
		    param(K,AVL)
		do  Key =.. [K,J],
		    avl_fetch(Key, AVL, V2)
		),
		Action = K
	    ;   Search = forall(Action,K,List)
	    ->  (   foreach(J2,List),
		    foreach(V3,Vars4),
		    param(K,AVL)
		do  Key =.. [K,J2],
		    avl_fetch(Key, AVL, V3)
		)
	    ;   Search = info(_)
	    ->  Action = Search
	    ;   Search = indomain(Key)
	    ->  avl_fetch(Key, AVL, V4),
		Action = Search
	    },
	    (   {Action = info(Msg)} -> [print_message(informational,Msg)]
	    ;   {Action = ff} -> [(labeling([ff],Vars4) /*-> true*/)]
	    ;   {Action = cup} -> [(c_labeling(Vars4) /*-> true*/)]
	    ;   {Action = cdown} -> [(c_labeling_down(Vars4) /*-> true*/)]
	    ;   {Action = r} -> [(r_labeling(Vars4) /*-> true*/)]
	    ;   {Action = indomain(_)} -> [indomain(V4)]
	    ;   [labeling([], Vars4)]
	    )
	).

searches(KL, PreassignedOs, _MAXT, Searches) :-
	optimizing(speed), !,
	(   foreach(_-bb(Irange,Orange,TFirst..TLast,_,_Calls,_,_,_,COrder),KL),
	    fromto(Cpart,[indomain(c(ILast)),forall(C,c,COrder)|Cpart1],Cpart1,[]),
	    foreach(for(i,Irange),Ipart),
	    fromto(Tpart,[forall(ff,t,TMand),forall(ff,t,TOpt)|Tpart1],Tpart1,[]),
	    foreach(for(r,TFirst1..TLast),Rpart),
	    fromto(-1,_,ILast,_),
	    param(PreassignedOs)
	do  Irange = (_IFirst..ILast),
	    (   cur_operation(ILast, _, _, Os, _),
		ord_intersect(Os, PreassignedOs)
	    ->  C = cdown
	    ;   C = cup
	    ),
	    search_t_part(Orange, TMand, TOpt),
	    TFirst1 is TFirst+1
	),
	append([Tpart,[info(labeled_T)],
		Ipart,[info(labeled_I)],
		Cpart,[info(labeled_C)],
		Rpart], Searches).
searches(KL, PreassignedOs, MAXT, Searches) :-
	optimizing(size), !,
	(   foreach(_-bb(Irange,Orange,_,_,_Calls,_,_,AOrder,COrder),KL),
	    fromto(Apart,[indomain(asum(ILast)),forall(a,a,AOrder)|Apart1], Apart1,[]),
	    foreach(forall(C,c,COrder),Cpart),
	    foreach(for(i,Irange),Ipart),
	    foreach(for(t,Orange),Tpart),
	    foreach(IFirst,_Ins),
	    fromto(-1,_,ILast,_),
	    param(PreassignedOs)
	do  Irange = (IFirst..ILast),
	    (   cur_operation(ILast, _, _, Os, _),
		ord_intersect(Os, PreassignedOs)
	    ->  C = cdown
	    ;   C = cup
	    )
	),
	append([Apart,[info(labeled_A)],
		Ipart,[info(labeled_I)],
		Tpart,[info(labeled_T)],
		Cpart,[info(labeled_C)],
		[for(r,0..MAXT)]], Searches).

search_t_part(Pmin..Pmax, Mand1, Opt1) :-
	(   for(P,Pmin,Pmax),
	    fromto(Mand1,Mand2,Mand3,[]),
	    fromto(Opt1,Opt2,Opt3,[])
	do  cur_operand(P,_,Use,Temps),
	    (   Use = 0
	    ->  Mand2 = Mand3, Opt2 = Opt3
	    ;   Temps = [-1|_]
	    ->  Mand2 = Mand3, Opt2 = [P|Opt3]
	    ;   true
	    ->  Mand2 = [P|Mand3], Opt2 = Opt3
	    )
	).

pl_constraint(CTerm, AVL0, AVL) -->
	% {print_message(informational, CTerm)},
	pl_translate_top(CTerm, Constraint, AVL0, AVL),
	% {print_message(informational, tr(Constraint))},
	(   {Constraint==true}
	->  []
	;   {simple(Constraint)}
	->  [Constraint#=1]
	;   {Constraint = (#\Negated)},
	    {simple(Negated)}
	->  [Negated #= 0]
	;   [Constraint]
	).

pl_translate_top(t(P)#=J, TI#=J, AVL0, AVL) --> {integer(J)}, !,
	dic_lookup(t(P), AVL0, TI, AVL).
pl_translate_top(CTerm, Constraint, AVL0, AVL) -->
	pl_translate(CTerm, Constraint, AVL0, AVL).

pl_translate(true, true, AVL, AVL) --> !.
pl_translate(false, false, AVL, AVL) --> !.
pl_translate(a(O), Var, AVL0, AVL) -->
	dic_lookup(a(O), AVL0, Var, AVL).
pl_translate(m(O), Var, AVL0, AVL) -->
	dic_lookup(m(O), AVL0, Var, AVL).
pl_translate(t(P)#=J, Var, AVL0, AVL) --> {integer(J)}, !,
	dic_lookup(t(P)#=J, AVL0, Var, AVL).
pl_translate(t(P)#\=J, #\ Var, AVL0, AVL) --> {integer(J)}, !,
	dic_lookup(t(P)#=J, AVL0, Var, AVL).
pl_translate(Term in CSRange, Var, AVL0, AVL) -->
	{avl_fetch(csrange, AVL0, CSRange)}, !,
	dic_lookup(Term in CSRange, AVL0, Var, AVL).
pl_translate(Term in Set, Var in Set, AVL0, AVL) -->
	dic_lookup(Term, AVL0, Var, AVL).
pl_translate(#\ T2, #\ C2, AVL0, AVL) -->
	pl_translate(T2, C2, AVL0, AVL).
pl_translate(T1 #/\ T2, C1 #/\ C2, AVL0, AVL) -->
	pl_translate(T1, C1, AVL0, AVL1),
	pl_translate(T2, C2, AVL1, AVL).
pl_translate(T1 #\/ T2, C1 #\/ C2, AVL0, AVL) -->
	pl_translate(T1, C1, AVL0, AVL1),
	pl_translate(T2, C2, AVL1, AVL).
pl_translate(T1 #=> T2, C1 #=> C2, AVL0, AVL) -->
	pl_translate(T1, C1, AVL0, AVL1),
	pl_translate(T2, C2, AVL1, AVL).
pl_translate(T1 #<=> T2, C1 #<=> C2, AVL0, AVL) -->
	pl_translate(T1, C1, AVL0, AVL1),
	pl_translate(T2, C2, AVL1, AVL).
pl_translate(T1 #= T2, V1 #= V2, AVL0, AVL) -->
	pl_translate_term(T1, V1, AVL0, AVL1),
	pl_translate_term(T2, V2, AVL1, AVL).
pl_translate(sum([X1|Xs]) #=< T2, Sum #=< V2, AVL0, AVL) --> !,
	pl_translate_term(X1, Sum1, AVL0, AVL1),
	(   foreach(X,Xs),
	    fromto(Sum1,Sum2,Sum2+Y,Sum),
	    fromto(AVL1,AVL2,AVL3,AVL4)
	do  pl_translate_term(X, Y, AVL2, AVL3)
	),
	pl_translate_term(T2, V2, AVL4, AVL).
pl_translate(sum([X1|Xs]) #>= T2, Sum #>= V2, AVL0, AVL) --> !,
	pl_translate_term(X1, Sum1, AVL0, AVL1),
	(   foreach(X,Xs),
	    fromto(Sum1,Sum2,Sum2+Y,Sum),
	    fromto(AVL1,AVL2,AVL3,AVL4)
	do  pl_translate_term(X, Y, AVL2, AVL3)
	),
	pl_translate_term(T2, V2, AVL4, AVL).
pl_translate(T1 #< T2, V1 #< V2, AVL0, AVL) -->
	pl_translate_term(T1, V1, AVL0, AVL1),
	pl_translate_term(T2, V2, AVL1, AVL).
pl_translate(T1 #=< T2, V1 #=< V2, AVL0, AVL) -->
	pl_translate_term(T1, V1, AVL0, AVL1),
	pl_translate_term(T2, V2, AVL1, AVL).
pl_translate(T1 #> T2, V1 #> V2, AVL0, AVL) -->
	pl_translate_term(T1, V1, AVL0, AVL1),
	pl_translate_term(T2, V2, AVL1, AVL).
pl_translate(T1 #>= T2, V1 #>= V2, AVL0, AVL) -->
	pl_translate_term(T1, V1, AVL0, AVL1),
	pl_translate_term(T2, V2, AVL1, AVL).
pl_translate(T1 #\= T2, V1 #\= V2, AVL0, AVL) -->
	pl_translate_term(T1, V1, AVL0, AVL1),
	pl_translate_term(T2, V2, AVL1, AVL).
pl_translate(lr_overlap(LS1,LE1,LS2,LE2), (LE3 #> LS4 #/\ LE4 #> LS3), AVL0, AVL) -->
	pl_translate_term(LS1, LS3, AVL0, AVL1),
	pl_translate_term(LS2, LS4, AVL1, AVL2),
	pl_translate_term(LE1, LE3, AVL2, AVL3),
	pl_translate_term(LE2, LE4, AVL3, AVL).
pl_translate(maximum(T1,List1), maximum(V1,List2), AVL0, AVL) -->
	pl_translate_term(T1, V1, AVL0, AVL1),
	(   foreach(Elt1,List1),
	    foreach(Elt2,List2),
	    fromto(AVL1,AVL2,AVL3,AVL)
	do  pl_translate_term(Elt1, Elt2, AVL2, AVL3)
	).
pl_translate(minimum(T1,List1), minimum(V1,List2), AVL0, AVL) -->
	pl_translate_term(T1, V1, AVL0, AVL1),
	(   foreach(Elt1,List1),
	    foreach(Elt2,List2),
	    fromto(AVL1,AVL2,AVL3,AVL)
	do  pl_translate_term(Elt1, Elt2, AVL2, AVL3)
	).
pl_translate(atleast_min(T1,List1), V1 #>= V2, AVL0, AVL) -->
	pl_translate_term(T1, V1, AVL0, AVL1),
	(   foreach(Elt1,List1),
	    foreach(Elt2,List2),
	    fromto(AVL1,AVL2,AVL3,AVL)
	do  pl_translate_term(Elt1, Elt2, AVL2, AVL3)
	),
	[minimum(V2,List2)].
pl_translate(all_different(List1), all_distinct(List2), AVL1, AVL) -->
	(   foreach(Elt1,List1),
	    foreach(Elt2,List2),
	    fromto(AVL1,AVL2,AVL3,AVL)
	do  pl_translate_term(Elt1, Elt2, AVL2, AVL3)
	).
pl_translate(disjoint1(List1), dc_disjoint(R2s,W2s), AVL1, AVL) -->
	(   foreach(item(R1,W1),List1),
	    foreach(R2,R2s),
	    foreach(W2,W2s),
	    fromto(AVL1,AVL2,AVL4,AVL)
	do  pl_translate_term(R1, R2, AVL2, AVL3),
	    pl_translate_term(W1, W2, AVL3, AVL4)
	).
pl_translate(disjoint2(Rects1), dc_disjoint(R2s,W1s,LS2s,LD2s,LE2s), AVL, AVL) -->
	(   foreach(rect(R1,W1,LS1,LD1,LE1),Rects1),
	    foreach(R2,R2s),
	    foreach(W1,W1s),
	    foreach(LS2,LS2s),
	    foreach(LD2,LD2s),
	    foreach(LE2,LE2s),
	    param(AVL)
	do  {avl_fetch(R1, AVL, R2)},
	    {avl_fetch(LS1, AVL, LS2)},
	    {avl_fetch(LD1, AVL, LD2)},
	    {avl_fetch(LE1, AVL, LE2)}
	).
pl_translate(nvalue(sum([X1|Xs]),Vars1), nvalue(SumVar,Vars2), AVL0, AVL7) -->
	pl_translate_term(X1, Sum1, AVL0, AVL1),
	(   foreach(X,Xs),
	    fromto(Sum1,Sum2,Sum2+Y,Sum),
	    fromto(AVL1,AVL2,AVL3,AVL4)
	do  pl_translate_term(X, Y, AVL2, AVL3)
	),
	(   foreach(V1,Vars1),
	    foreach(V2,Vars2),
	    fromto(AVL4,AVL5,AVL6,AVL7)
	do  pl_translate_term(V1, V2, AVL5, AVL6)
	),
	[Sum #= SumVar].
pl_translate(cumulative(Tasks1,Lim,Ps1), cumulative(Tasks2,[precedences(Ps2),limit(Lim),global(true)/*,bounding_box(_,UB)*/]), AVL0, AVL) -->
	(   foreach(task(c(O),Dur0,Height0),Tasks1),
	    fromto(Tasks2,Tasks3,Tasks4,[]),
	    fromto(Tasks5,Tasks6,Tasks7,[]),
	    fromto(Os2,Os3,Os4,[]),
	    fromto(AVL0,AVL1,AVL4,AVL)
	    % fromto(0,_,Start,StartOut),
	do  pl_translate_term(Height0, Height, AVL1, AVL2),
	    dic_lookup(c(O), AVL2, Start, AVL3),
	    dic_lookup(Dur0, AVL3, Dur, AVL4),
	    {   Dur==0
	    ->  Tasks3 = Tasks4,
		Tasks6 = Tasks7,
		Os3 = Os4
	    ;   Height==0
	    ->  Tasks3 = Tasks4,
		Tasks6 = Tasks7,
		Os3 = Os4
	    ;   true
	    ->  Tasks3 = [task(Start,Dur,_,Height,O)|Tasks4],
		Tasks6 = [task(Start,Dur,_,Height,1)|Tasks7],
		Os3 = [O|Os4]
	    }
	),
	[cumulatives(Tasks5, [machine(1,Lim)], [bound(upper)])],
	{   foreach(dis(A,B),Ps1),
	    fromto(Ps2,Ps3,Ps4,[]),
	    param(Os2,AVL)
	do  (   ord_member(A, Os2),
		ord_member(B, Os2)
	    ->  Ps3 = [B-A #= Diff|Ps4],
		avl_fetch(dis(A,B), AVL, Diff)
	    ;   Ps3 = Ps4
	    )
	}.
pl_translate(live_cumulative(Tasks1,Lim), cumulative(Tasks2,[limit(Lim),global(true)/*,bounding_box(_,UB)*/]), AVL0, AVL) -->
	(   foreach(task(ls(T),Dur0,End0,Height0),Tasks1),
	    fromto(Tasks2,Tasks3,Tasks4,[]),
	    fromto(AVL0,AVL1,AVL5,AVL)
	do  pl_translate_term(Height0, Height, AVL1, AVL2),
	    dic_lookup(ls(T), AVL2, Start, AVL3),
	    dic_lookup(Dur0, AVL3, Dur, AVL4),
	    dic_lookup(End0, AVL4, End, AVL5),
	    {   Dur==0 -> Tasks3 = Tasks4
	    ;   Height==0 -> Tasks3 = Tasks4
	    ;   true -> Tasks3 = [task(Start,Dur,End,Height,T)|Tasks4]
	    }
	).
				% [StartOut #>= UB].
pl_translate(increasing_each(Iss,Map1), table(Tuples1,Map2), AVL0, AVL) -->
	(   foreach(Op,Map1),
	    foreach([Op,J],Map2),
	    count(J,1,_)
	do  []
	),
	(   foreach(Is,Iss),
	    fromto(Tuples1,Tuples2,Tuples4,[]),
	    fromto(AVL0,AVL1,AVL4,AVL)
	do  (   foreach(I,Is),
		fromto(start,Y0,Y,_),
		fromto(Tuples2,[[X,Y]|Tuples3],Tuples3,Tuples4),
		fromto(AVL1,AVL2,AVL3,AVL4)
	    do  dic_lookup(I, AVL2, X, AVL3),
		({Y0==start} -> [] ; [Y0 #=< Y])
	    )
	).
pl_translate(all_eq_or_joker(Xs,V), all_eq_or_joker(Ys,W), AVL0, AVL) -->
	(   foreach(A,[V|Xs]),
	    foreach(B,[W|Ys]),
	    fromto(AVL0,AVL1,AVL2,AVL)
	do  pl_translate_term(A, B, AVL1, AVL2)
	).
pl_translate(element(I,Xs,V), element(J,Ys,W), AVL0, AVL) -->
	(   foreach(A,[I,V|Xs]),
	    foreach(B,[J,W|Ys]),
	    fromto(AVL0,AVL1,AVL2,AVL)
	do  pl_translate_term(A, B, AVL1, AVL2)
	).
pl_translate(dc_element(I,Xs,V), dc_element(J,Ys,W), AVL0, AVL) -->
	(   foreach(A,[I,V|Xs]),
	    foreach(B,[J,W|Ys]),
	    fromto(AVL0,AVL1,AVL2,AVL)
	do  pl_translate_term(A, B, AVL1, AVL2)
	).
pl_translate(let(Qs,Cs), true, AVL0, AVL) -->
	(   foreach(Q1,Qs),
	    fromto(AVL0,AVL1,AVL2,AVL3)
	do  {avl_store(Q1, AVL1, _, AVL2)}
	),
	(   foreach(C,Cs),
	    fromto(AVL3,AVL4,AVL5,AVL6)
	do  % {print_message(informational, pl_translate_in_let(C))},
	    pl_translate(C, Tran, AVL4, AVL5),
	    % {print_message(informational, pl_translated_in_let(Tran))},
	    [Tran]
	),
	(   foreach(Q2,Qs),
	    fromto(AVL6,AVL7,AVL8,AVL)
	do  {avl_delete(Q2, AVL7, _, AVL8)}
	).
pl_translate(temp_cardinality(Temps1,ValuesCounts1),
	     temp_cardinality(Temps2,ValuesCounts2), AVL0, AVL) -->
	(   foreach(T1,Temps1),
	    foreach(T2,Temps2),
	    fromto(AVL0,AVL1,AVL2,AVL3)
	do  pl_translate_term(T1, T2, AVL1, AVL2)
	),
	(   foreach(I-A1,ValuesCounts1),
	    foreach(I-A2,ValuesCounts2),
	    fromto(AVL3,AVL4,AVL5,AVL)
	do  pl_translate_term(A1, A2, AVL4, AVL5)
	).
pl_translate(table(Matrix1,Relation),
	     table(Matrix2,Relation), AVL0, AVL) -->
	(   foreach(Row1,Matrix1),
	    foreach(Row2,Matrix2),
	    fromto(AVL0,AVL1,AVL4,AVL)
	do  (   foreach(T1,Row1),
		foreach(T2,Row2),
		fromto(AVL1,AVL2,AVL3,AVL4)
	    do  pl_translate_term(T1, T2, AVL2, AVL3)
	    )
	).
pl_translate(value_precede_chain(Symm,All,Vars1),
	     value_precede_chain(Symm,All,Vars2), AVL1, AVL) -->
	(   foreach(Elt1,Vars1),
	    foreach(Elt2,Vars2),
	    fromto(AVL1,AVL2,AVL3,AVL)
	do  pl_translate_term(Elt1, Elt2, AVL2, AVL3)
	).

pl_translate_term(C, C, AVL, AVL) -->
	{integer(C)}, !.
pl_translate_term(T1 * T2, C1 * C2, AVL0, AVL) --> !,
	pl_translate_term(T1, C1, AVL0, AVL1),
	pl_translate_term(T2, C2, AVL1, AVL).
pl_translate_term(T1 + (-1), C1 - 1, AVL0, AVL) --> !,
	pl_translate_term(T1, C1, AVL0, AVL).
pl_translate_term(T1 + T2, C1 + C2, AVL0, AVL) --> !,
	pl_translate_term(T1, C1, AVL0, AVL1),
	pl_translate_term(T2, C2, AVL1, AVL).
pl_translate_term(T1 - T2, C1 - C2, AVL0, AVL) --> !,
	pl_translate_term(T1, C1, AVL0, AVL1),
	pl_translate_term(T2, C2, AVL1, AVL).
% pl_translate_term(if(t(Q2) #= Val,c(I1),-1), Term, AVL, AVL) --> !,
% 	{avl_fetch(t(Q2), AVL, TQ2)},
% 	{avl_fetch(c(I1), AVL, CI1)},
% 	[live_end_or_null(TQ2, Val, CI1, Term)].
pl_translate_term(if(Ctr1,Then1,Else1), Term, AVL0, AVL) --> !,
	pl_translate(Ctr1, Ctr2, AVL0, AVL1),
	pl_translate_term(Then1, Then2, AVL1, AVL2),
	pl_translate_term(Else1, Else2, AVL2, AVL),
	({var(Ctr2)} -> {Ctr2=B} ; [Ctr2 #<=> B]),
	[if_then_else(B, Then2, Else2, Term)].
pl_translate_term(Ctr, Var, AVL0, AVL) -->
	pl_translate(Ctr, Var, AVL0, AVL), !.
pl_translate_term(Term, Var, AVL0, AVL) -->
	dic_lookup(Term, AVL0, Var, AVL).


head_arg(a, main(_,X,_,_,_,_,_,_,_,_), X).
head_arg(c, main(_,_,X,_,_,_,_,_,_,_), X).
head_arg(i, main(_,_,_,X,_,_,_,_,_,_), X).
head_arg(t, main(_,_,_,_,X,_,_,_,_,_), X).
head_arg(r, main(_,_,_,_,_,X,_,_,_,_), X).
head_arg(ls, main(_,_,_,_,_,_,X,_,_,_), X).
head_arg(ld, main(_,_,_,_,_,_,_,X,_,_), X).
head_arg(le, main(_,_,_,_,_,_,_,_,X,_), X).

dic_lookup(Key, AVL0, Val, AVL) -->
	{integer(Key)}, !,
	{Val = Key},
	{AVL = AVL0}.
dic_lookup(Key, AVL0, Val, AVL) -->
	{avl_fetch(Key, AVL0, Val)}, !,
	{AVL = AVL0}.
% special for common subexpression elimination
dic_lookup(Term in CSRange, AVL0, Val, AVL) --> !,
	{avl_fetch(Term, AVL0, TV)},
	{avl_store(Term in CSRange, AVL0, Val, AVL)},
	[TV in CSRange #<=> Val].
dic_lookup(t(P)#=J, AVL0, Val, AVL) --> {integer(J)}, !,
	{avl_fetch(t(P), AVL0, TP)},
	{avl_store(t(P)#=J, AVL0, Val, AVL)},
	[TP#=J #<=> Val].
dic_lookup(ls(t(P)), AVL0, Val, AVL) --> !,
	{avl_fetch(ls, AVL0, LS)},
	{avl_fetch(t(P), AVL0, TP)},
	{avl_store(ls(t(P)), AVL0, Val, AVL)},
	[element_ls(TP, LS, Val)].
dic_lookup(ld(t(P)), AVL0, Val, AVL) --> !,
	{avl_fetch(ld, AVL0, LD)},
	{avl_fetch(t(P), AVL0, TP)},
	{avl_store(ld(t(P)), AVL0, Val, AVL)},
	[element_ld(TP, LD, Val)].
dic_lookup(le(t(P)), AVL0, Val, AVL) --> !,
	{avl_fetch(le, AVL0, LE)},
	{avl_fetch(t(P), AVL0, TP)},
	{avl_store(le(t(P)), AVL0, Val, AVL)},
	[element_le(TP, LE, Val)].
dic_lookup(r(t(P)), AVL0, Val, AVL) --> !,
	{avl_fetch(r, AVL0, R)},
	{avl_fetch(t(P), AVL0, TP)},
	{avl_store(r(t(P)), AVL0, Val, AVL)},
	[element_r(TP, R, Val)].
% new item
dic_lookup(Key, AVL0, Val, AVL) -->
	{avl_store(Key, AVL0, Val, AVL)}.

pretty_avl(AVL) :-
	avl_to_list(AVL, KL),
	(   foreach(Key-Var,KL)
	do  pretty_key_var(Key, Var)
	).

pretty_key_var(t(P) #= J, Var) :- !,
	number_codes(P, Pcodes),
	number_codes(J, Jcodes),
	append(["T_", Pcodes, "_", Jcodes], Name),
	Var = '$VAR'(Name).
pretty_key_var(ls(t(P)), Var) :- !,
	number_codes(P, Pcodes),
	append("LS_T_", Pcodes, Name),
	Var = '$VAR'(Name).
pretty_key_var(ld(t(P)), Var) :- !,
	number_codes(P, Pcodes),
	append("LD_T_", Pcodes, Name),
	Var = '$VAR'(Name).
pretty_key_var(le(t(P)), Var) :- !,
	number_codes(P, Pcodes),
	append("LE_T_", Pcodes, Name),
	Var = '$VAR'(Name).
pretty_key_var(r(t(P)), Var) :- !,
	number_codes(P, Pcodes),
	append("R_T_", Pcodes, Name),
	Var = '$VAR'(Name).
pretty_key_var(r(P) in _, Var) :- !,
	number_codes(P, Pcodes),
	append("CS_R_", Pcodes, Name),
	Var = '$VAR'(Name).
pretty_key_var(_, _).

pretty_head(main(Obj,As,Cs,Os,Ts,Rs,LSs,LDs,LEs,ASums)) :-
	Obj = '$VAR'("Obj"),
	pretty_vars(As, "A_", 0),
	pretty_vars(Cs, "C_", 0),
	pretty_vars(Os, "O_", 0),
	pretty_vars(Ts, "T_", 0),
	pretty_vars(Rs, "R_", -1),
	pretty_vars(LSs, "LS_", -1),
	pretty_vars(LDs, "LD_", -1),
	pretty_vars(LEs, "LE_", -1),
	pretty_vars(ASums, "ASum_", 0).

pretty_vars(Vars, Prefix, Base) :-
	(   foreach('$VAR'(Name),Vars),
	    count(I,Base,_),
	    param(Prefix)
	do  (   I =:= -1
	    ->  Suffix = "M1"
	    ;   number_codes(I, Suffix)
	    ),
	    append(Prefix, Suffix, Name)
	).

% for SP4.3

legacy_portray_clause(Clause) :-
        current_output(Stream),
        legacy_portray_clause(Stream, Clause).

legacy_portray_clause(SA, Clause) :-
	prolog:arg_option(legacy_numbervars, write_term, 4, _), !,
        Goal = legacy_portray_clause(SA,Clause),
        prolog:dealias(SA, outtext, Stream, Goal, 1),
        legacy_portray_clause1(Stream, Clause).
legacy_portray_clause(SA, Clause) :-
	portray_clause(SA, Clause).


legacy_portray_clause1(Stream, Clause) :-
        prolog:(
                  write_term_options([quoted(true),generate_variable_names_(true),anonymous_variable_names_(true),
                                      % [PM] legacy_numbervars(true) is what differs from portray_clause/2.
                                      legacy_numbervars(true)
                                      ], Style, Clause),
                  portray_clause1(Clause, Style, null, Co, Stream),
                  write_fullstop(Co, Stream),
                  fail
        ).
legacy_portray_clause1(Stream, _) :-
        prolog:(
                  autoflush_output(Stream)
               ).


